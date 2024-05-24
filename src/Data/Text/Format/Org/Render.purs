module Data.Text.Format.Org.Render where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Char (fromCharCode)
import Data.Date as DT
import Data.Enum (fromEnum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.String (toUpper, toLower, take) as String
import Data.String.CodeUnits (singleton) as String
import Data.Text.Doc (Doc, (<+>), (</>), (<//>))
import Data.Text.Doc as D
import Data.Time as DT
import Data.Tuple.Nested ((/\), type (/\))

import Data.Text.Format.Org.Construct as Org
import Data.Text.Format.Org.Types (OrgFile(..), OrgDoc(..))
import Data.Text.Format.Org.Types as Org
import Data.Text.Format.Org.Keywords as Keywords


newtype Deep = Deep Int


data KwMode
    = AsProperty
    | AsKeyword


data DrawerMode
    = DrawerUpper
    | DrawerLower


layout :: OrgFile -> Doc
layout (OrgFile { meta, doc }) =
    case (Keywords.isEmpty meta /\ Org.isDocEmpty doc) of
        (true /\ true) -> D.nil
        (false /\ true) -> renderMetaBlock
        (true /\ false) -> layoutDoc root doc
        (false /\ false) ->
            renderMetaBlock
            <//> layoutDoc root doc
    where
        renderMetaBlock = meta # Keywords.fromKeywords # map (layoutKeyword AsKeyword) # D.stack


layoutDoc :: Deep -> OrgDoc -> Doc
layoutDoc deep (OrgDoc { zeroth, sections }) =
    if (not $ Array.length zeroth == 0) then
        renderZerothBlock
        </>
        renderSectionsBlock
    else
        renderSectionsBlock
    where
        renderZerothBlock = zeroth # map (layoutBlock deep) # D.joinWith D.break
        renderSectionsBlock = sections # map layoutSection # D.join


layoutBlock :: Deep -> Org.Block -> Doc
layoutBlock deep = case _ of
    Org.Of conf words ->
        case blockNameAndArgs conf of
            nameDoc /\ mbArgsDoc ->
                D.nest' indent
                    [ D.text "#+begin_" <> nameDoc <> case mbArgsDoc of
                        Just argsDoc -> D.space <> argsDoc
                        Nothing -> D.nil
                    , D.join $ layoutWords <$> NEA.toArray words
                    , D.text "#+end_" <> nameDoc
                    ]
    Org.List items ->
        layoutItems (deepToIndent deep) items
    Org.Table format rows -> 
        layoutTable $ NEA.toArray rows
    Org.Paragraph words ->
        words
            # NEA.toArray
            # splitByBreak
            # map (map layoutWords)
            # map D.join
            # D.nest' indent
    Org.WithKeyword (Keywords.Keyword kwd) block ->
        D.nest indent (layoutKeyword AsKeyword kwd)
        </> layoutBlock deep block
    Org.JoinB blockA blockB ->
        layoutBlock deep blockA </> layoutBlock deep blockB
    Org.IsDrawer drawer -> 
        layoutDrawer indent drawer
    Org.Footnote label def ->
        D.bracket "[" (D.text "fn:" <> D.text label) "]"
            <+> D.stack (layoutWords <$> NEA.toArray def) -- FIXME: impoperly renders line breaks, see 04e
    -- Quote
    -- const D.nil
    -- where
    --     withIndent lines =
    --         D.nest
    where

        blockNameAndArgs = case _ of
            Org.Quote -> D.text "quote" /\ Nothing
            Org.Example -> D.text "example" /\ Nothing
            Org.Center -> D.text "center" /\ Nothing
            Org.Verse -> D.text "verse" /\ Nothing
            Org.Export -> D.text "export" /\ Nothing
            Org.Comment -> D.text "comment" /\ Nothing
            Org.Code mbLang -> D.text "src" /\ (case mbLang of
                    Just (Org.Language lang) -> Just $ D.text lang
                    Nothing -> Nothing)
            Org.Custom name args ->
                D.text name /\ (
                    if (Array.length args > 0)
                        then Just $ D.joinWith D.space $ (D.text <$> args)
                        else Nothing
                    )

        splitByBreak :: Array Org.Words -> Array (Array Org.Words)
        splitByBreak =
            Array.foldl
                (\{ prev, last } w ->
                    case w of
                        Org.Break ->
                            { prev : Array.snoc prev last
                            , last : []
                            }
                        _ ->
                            { prev
                            , last : Array.snoc last w
                            }
                )
                { prev : ([] :: Array (Array Org.Words))
                , last : ([] :: Array Org.Words)
                }
            >>> \{ prev, last } -> Array.snoc prev last

        indent = deepToIndent deep


layoutSection :: Org.Section -> Doc
layoutSection (Org.Section section) =
    let
        headingText =
            section.heading # NEA.toArray # map layoutWords # D.join
        levelPrefix = Array.replicate section.level "*" # Array.fold # D.text
        commentPrefix = if section.comment then Just $ D.text "COMMENT" else Nothing
        todoPrefix = section.todo
                        <#> case _ of
                            Org.Todo -> "TODO"
                            Org.Doing -> "DOING"
                            Org.Done -> "DONE"
                            Org.CustomKW s -> s
                        <#> D.text
        priorityPrefix = section.priority
                        <#> case _ of
                            Org.Alpha c -> "[#" <> String.toUpper (String.singleton c) <> "]"
                            Org.Num n -> "[#" <> show n <> "]"
                        <#> D.text
        cookieSuffix = section.cookie
                        <#> case _ of
                            Org.Split -> "[/]"
                            Org.Percent -> "[%]"
                            Org.Pie -> "[o]"
                        <#> D.text
        tagsSuffix = if Array.length section.tags > 0 then
                        Just $ D.join $ D.text <$> [":"] <> Array.intersperse ":" section.tags <> [":"]
                     else Nothing
        headlingLineCombined =
            [ Just levelPrefix
            , todoPrefix
            , priorityPrefix
            , commentPrefix
            , Just headingText
            , cookieSuffix
            , tagsSuffix
            ]
            # D.spacify
        planningItem tag datetime = D.text (String.toUpper tag) <> D.text ":" <+> layoutDateTime datetime
        planning =
            unwrap section.planning
        hasPlanning
            =  isJust planning.scheduled
            || isJust planning.deadline
            || isJust planning.timestamp
            || isJust planning.closed
        hasProperties =
            Keywords.hasKeywords section.props
        propertiesDrawer =
            section.props
                # Keywords.fromKeywords'
                # map (layoutKeyword AsProperty)
                # D.joinWith D.break
                # layoutDrawer' 0 DrawerUpper "properties"
        hasOtherDrawers = 
            Array.length section.drawers > 0
        otherDrawers = 
            section.drawers 
                # map (layoutDrawer 0)
                # D.joinWith D.break
        planningLine =
            [ planningItem "TIMESTAMP" <$> planning.timestamp
            , planningItem "DEADLINE"  <$> planning.deadline
            , planningItem "SCHEDULED" <$> planning.scheduled
            , planningItem "CLOSED"    <$> planning.closed
            ]
            # D.spacify
    in
        if not $ Org.isDocEmpty section.doc then
            headlingLineCombined
                <> (if hasPlanning then D.break <> planningLine <> D.break else D.break)
                <> (if hasProperties then propertiesDrawer <> D.break else D.nil)
                <> (if hasOtherDrawers then otherDrawers <> D.break else D.nil)
                <> layoutDoc (deepAs section.level) section.doc
        else
            headlingLineCombined
            <> (if hasProperties then D.break <> propertiesDrawer <> D.break else D.nil)
            <> (if hasOtherDrawers then otherDrawers <> D.break else D.nil)
            <> (if hasPlanning then D.break <> planningLine <> D.break else D.break)


layoutWords :: Org.Words -> Doc
layoutWords = case _ of
    Org.Plain plain -> D.text plain
    Org.Marked mk s ->
        markWith mk $ D.text s
    Org.Break -> D.break
    Org.Link trg mbText ->
        case mbText of
            Just text ->
                D.bracket
                    "["
                    (D.concat (D.bracket "[" (linkTrg trg) "]")
                              (D.bracket "[" (D.text text) "]")
                    )
                    "]"
            Nothing ->
                D.bracket "[[" (linkTrg trg) "]]"
    Org.Image src ->
        D.bracket "[[" (imgSrc src) "]]"
    Org.Punct p -> D.nil -- FIXME
    Org.Markup str -> D.nil -- FIXME
    Org.DateTime { start, end } ->
        case end of
            Just end' -> layoutDateTime start <> D.text "--" <> layoutDateTime end'
            Nothing -> layoutDateTime start
    Org.ClockW (Org.Clock clock) -> D.text "=>" <+> layoutClock clock
    Org.DiaryW (Org.Diary diary) ->
        D.bracket "<%%"
            (D.text diary.expr <> case diary.time of
                Just range -> D.space <> layoutRange range
                Nothing -> D.nil
            )
        ">"
    Org.FootnoteRef { label, def } ->
        D.bracket "["
            (D.text "fn:" <> D.text label <> case def of
                Just def' -> D.text ":" <> D.text def'
                Nothing -> D.nil
            )
        "]"
    Org.JoinW wa wb -> layoutWords wa <> layoutWords wb
    where
        markWith mk doc = case mk of
            Org.Bold -> D.wrap "*" doc
            Org.Italic -> D.wrap "/" doc
            Org.Underline -> D.wrap "_" doc
            Org.Strike -> D.wrap "+" doc
            Org.InlineCode -> D.wrap "~" doc
            Org.Verbatim -> D.wrap "=" doc
            Org.Highlight -> doc -- FIXME
            Org.Error -> D.wrap "X" doc
            Org.And mka mkb -> markWith mkb $ markWith mka doc
        linkTrg = case _ of
            Org.Remote url -> D.text url
            Org.Local url -> D.text "file:" <> D.text url
            Org.Heading trg -> D.text trg
        imgSrc = case _ of
            Org.RemoteSrc url -> D.text url
            Org.LocalSrc url -> D.text "file:" <> D.text url
        layoutClock c =
            D.text (showdd $ c.hour) <> D.text ":" <>
            D.text (showdd $ c.minute)


layoutDateTime :: Org.OrgDateTime -> Doc
layoutDateTime =
    case _ of
        Org.OrgDateTime
            { active, date, time, repeat, delay } ->
                let
                    datetime_ =
                        [ Just $ layoutDate date
                        , layoutRange <$> time
                        , layoutRepeater <$> repeat
                        , layoutDelay <$> delay
                        ]
                        # D.spacify
                in if active then
                    D.bracket "<" datetime_ ">"
                else
                    D.bracket "[" datetime_ "]"
    where
        interval = case _ of
            Org.Hour -> "h"
            Org.Day -> "d"
            Org.Week -> "w"
            Org.Month -> "m"
            Org.Year -> "y"
        rmode = case _ of
            Org.Single -> "+"
            Org.FromToday -> "++"
            Org.Jump -> ".+"
        dmode = case _ of
            Org.One -> "-"
            Org.All -> "--"
        layoutRepeater = case _ of
            Org.Repeater r ->
                rmode r.mode <> show r.value <> interval r.interval <>
                    case r.with of
                        Just w -> "/" <> show w.value <> interval w.interval
                        Nothing -> ""
            >>> D.text
        layoutDelay = case _ of
            Org.Delay d ->
                dmode d.mode <> show d.value <> interval d.interval
            >>> D.text


layoutDate :: DT.Date -> Doc
layoutDate d =
    D.text (show $ fromEnum $ DT.year d) <> D.text "-" <>
    D.text (showdd $ fromEnum $ DT.month d) <> D.text "-" <>
    D.text (showdd $ fromEnum $ DT.day d) <+>
    D.text (showwd $ DT.weekday d)
    where
        showwd = String.take 3 <<< show


layoutTime :: DT.Time -> Doc
layoutTime t =
    D.text (showdd $ fromEnum $ DT.hour t) <> D.text ":" <>
    D.text (showdd $ fromEnum $ DT.minute t)


layoutRange :: Org.OrgTimeRange -> Doc
layoutRange = case _ of
    Org.OrgTimeRange { start, end } ->
        layoutTime start <> case end of
            Just end' -> D.text "-" <> layoutTime end'
            Nothing -> D.nil


layoutItems :: Int -> Org.ListItems -> Doc
layoutItems indent (Org.ListItems lt items) =
    let
        markerPrefix idx = case lt of
            Org.Bulleted -> "*"
            Org.Plussed -> "+"
            Org.Hyphened -> "-"
            Org.Numbered -> show (idx + 1) <> "."
            Org.NumberedFrom n -> show (idx + n) <> "."
            Org.Alphed -> (fromMaybe "?" $ String.singleton <$> (fromCharCode $ 0x61 + idx)) <> "."
            # D.text
        checkPrefix = case _ of
            Org.Uncheck -> "[ ]"
            Org.Halfcheck -> "[-]"
            Org.Check -> "[X]"
            >>> D.text
        counterPrefix (Org.Counter n) =
            D.text $ "[@" <> show n <> "]"
        tagPrefix tag =
            D.text $ tag <> " ::"
        itemText ws =
            ws # NEA.toArray # map layoutWords # D.join
        itemLine idx (Org.Item opts ws Nothing) =
            [ Just $ markerPrefix idx
            , opts.check <#> checkPrefix
            , opts.counter <#> counterPrefix
            , opts.tag <#> tagPrefix
            , Just $ itemText ws
            ]
            # D.spacify
        itemLine idx (Org.Item opts ws (Just subs)) =
            itemLine idx (Org.Item opts ws Nothing)
            </> layoutItems (howDeep idx) subs
        itemMaybeWithDrawers idx item@(Org.Item opts _ _) = 
            itemLine idx item
            <> if hasDrawers opts then drawers opts else D.nil 
        hasDrawers opts = 
            Array.length opts.drawers > 0
        drawers opts = 
            opts.drawers 
                # map (layoutDrawer indent)
                # D.joinWith D.break            
        howDeep idx = indent + case lt of
            Org.Bulleted -> 2
            Org.Plussed -> 2
            Org.Hyphened -> 2
            Org.Numbered -> if (idx + 1) < 10 then 3 else 4
            Org.NumberedFrom n -> if (idx + n) < 10 then 3 else 4
            Org.Alphed -> 3
    in
        D.nest' indent $ NEA.toArray $ NEA.mapWithIndex itemMaybeWithDrawers items


layoutKeyword :: KwMode -> Keywords.KeywordRec String -> Doc
layoutKeyword AsProperty kwd =
    case kwd.value of
        Just value ->
            D.wrap ":" (D.text kwd.name) <+> D.text value
        Nothing ->
            D.wrap ":" (D.text kwd.name)
layoutKeyword AsKeyword kwd =
    case kwd.default /\ kwd.value of
            Just optVal /\ Nothing -> D.bracket "#+" (D.text kwd.name <> D.bracket "[" (D.text optVal) "]") ":"
            Just optVal /\ Just value -> D.bracket "#+" (D.text kwd.name <> D.bracket "[" (D.text optVal) "]") ":" <+> D.text value
            Nothing /\ Just value ->  D.bracket "#+" (D.text kwd.name) ":" <+> D.text value
            Nothing /\ Nothing -> D.bracket "#+" (D.text kwd.name) ":"


layoutDrawer :: Int -> Org.Drawer -> Doc
layoutDrawer indent (Org.Drawer { name, content }) =
    content
        # NEA.toArray
        # map layoutWords
        # D.join
        # layoutDrawer' indent DrawerLower name
        # D.nest indent


layoutDrawer' :: Int -> DrawerMode -> String -> Doc -> Doc
layoutDrawer' indent mode name content =
    D.wrap ":" (D.text $ applyMode name)
    </> D.nest indent content
    </> D.nest indent (D.wrap ":" (D.text $ applyMode "end"))
    where
        applyMode =
            case mode of
                DrawerUpper -> String.toUpper
                DrawerLower -> String.toLower


layoutTable :: Array Org.TableRow -> Doc
layoutTable rows =
    D.joinWith D.break $ layoutRow <$> rows
    where 
        columnsCount = Array.foldl max 0 $ columnsAt <$> rows
        columnsAt Org.BreakT = 0
        columnsAt (Org.Row columns) = NEA.length columns
        layoutRow Org.BreakT = 
            D.wrap "|" $ D.joinWith (D.text "+") $ Array.replicate columnsCount $ D.text "-"
        layoutRow (Org.Row columns) = 
            D.wrap "|" $ D.joinWith (D.text "|") $ layoutColumn <$> NEA.toArray columns
        layoutColumn Org.Empty = 
            D.text " "
        layoutColumn (Org.Column words) = 
            D.join $ layoutWords <$> NEA.toArray words


showdd n =
    if n < 10 then "0" <> show n else show n


root :: Deep
root = Deep 0


deepAs :: Int -> Deep
deepAs = Deep


deeper :: Deep -> Deep
deeper = incDeep 1


incDeep :: Int -> Deep -> Deep
incDeep m (Deep n) = Deep $ n + m


deepToIndent :: Deep -> Int
deepToIndent (Deep 0) = 0
deepToIndent (Deep 1) = 2
deepToIndent (Deep n) = 1 + n -- in spaces
