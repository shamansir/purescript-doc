module Data.Text.Format.Org.Render where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Char (fromCharCode)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (toUpper, take) as String
import Data.String.CodeUnits (singleton) as String
import Data.Text.Doc (Doc, (<+>), (</>), (<//>))
import Data.Text.Doc as D
import Data.Text.Format.Org.Construct as Org
import Data.Text.Format.Org.Types (OrgFile(..), OrgDoc(..))
import Data.Text.Format.Org.Types as Org
import Data.Tuple.Nested ((/\), type (/\))
import Data.Date as DT
import Data.Time as DT
import Data.Enum (fromEnum)


newtype Deep = Deep Int


layout :: OrgFile -> Doc
layout (OrgFile { meta, doc }) =
    case (Map.isEmpty meta /\ Org.isDocEmpty doc) of
        (true /\ true) -> D.nil
        (false /\ true) -> renderMetaBlock
        (true /\ false) -> layoutDoc root doc
        (false /\ false) ->
            renderMetaBlock
            <//> layoutDoc root doc
    where
        renderMetaBlock = meta # Map.toUnfoldable # map renderMeta # D.stack
        renderMeta ((idx /\ key) /\ value) =
            D.bracket "#+" (D.text key) ":" <+> D.text value


layoutDoc :: Deep -> OrgDoc -> Doc
layoutDoc deep (OrgDoc { zeroth, sections }) =
    if (not $ Array.length zeroth == 0) then
        renderZerothBlock
        </>
        renderSectionsBlock
    else
        renderSectionsBlock
    where
        renderZerothBlock = zeroth # map (layoutBlock deep) # Array.intersperse D.break # Array.foldl (<>) D.nil
        renderSectionsBlock = sections # map layoutSection # Array.foldl (<>) D.nil


layoutBlock :: Deep -> Org.Block -> Doc
layoutBlock deep = case _ of
    Org.Of conf str ->
        case blockNameAndArgs conf of
            nameDoc /\ mbArgsDoc ->
                D.nest' indent
                    [ D.text "#+begin_" <> nameDoc <> case mbArgsDoc of
                        Just argsDoc -> D.space <> argsDoc
                        Nothing -> D.nil
                    , D.text str -- TODO: replace breaks, add markup
                    , D.text "#+end_" <> nameDoc
                    ]
    Org.List items ->
        layoutItems (deepToIndent deep) items
    Org.Table _ -> D.text "TABLE"  -- TODO
    Org.Paragraph words ->
        words
            # NEA.toArray
            # splitByBreak
            # map (map layoutWords)
            # map (Array.foldl (<>) D.nil)
            # D.nest' indent
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
                        then Just $ Array.foldl (<>) D.nil $ Array.intersperse D.space $ (D.text <$> args)
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
            section.heading # NEA.toArray # map layoutWords # Array.foldl (<>) D.nil
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
                        Just $ Array.foldl (<>) D.nil $ D.text <$> [":"] <> Array.intersperse ":" section.tags <> [":"]
                     else Nothing
        everythingCombined =
            [ Just levelPrefix
            , todoPrefix
            , priorityPrefix
            , commentPrefix
            , Just headingText
            , cookieSuffix
            , tagsSuffix
            ]
            # D.spacify
    in
        if not $ Org.isDocEmpty section.doc then
            everythingCombined </> layoutDoc (deepAs section.level) section.doc
        else
            everythingCombined <> D.break


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
            Just end' -> dateTime start <> D.text "--" <> dateTime end'
            Nothing -> dateTime start
    Org.ClockW (Org.Clock clock) -> D.text "=>" <+> lclock clock
    Org.DiaryW (Org.Diary diary) ->
        D.bracket "<%%"
            (D.text diary.expr <> case diary.time of
                Just range -> D.space <> lrange range
                Nothing -> D.nil
            )
        ">"
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
        showwd = String.take 3 <<< show
        showdd n =
            if n < 10 then "0" <> show n else show n
        ldate d =
            D.text (show $ fromEnum $ DT.year d) <> D.text "-" <>
            D.text (showdd $ fromEnum $ DT.month d) <> D.text "-" <>
            D.text (showdd $ fromEnum $ DT.day d) <+>
            D.text (showwd $ DT.weekday d)
        ltime t =
            D.text (showdd $ fromEnum $ DT.hour t) <> D.text ":" <>
            D.text (showdd $ fromEnum $ DT.minute t)
        lclock c =
            D.text (showdd $ c.hour) <> D.text ":" <>
            D.text (showdd $ c.minute)
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
        lrepeater = case _ of
            Org.Repeater r ->
                rmode r.mode <> show r.value <> interval r.interval <>
                    case r.with of
                        Just w -> "/" <> show w.value <> interval w.interval
                        Nothing -> ""
            >>> D.text
        lrange = case _ of
            Org.OrgTimeRange { start, end } ->
                ltime start <> case end of
                    Just end' -> D.text "-" <> ltime end'
                    Nothing -> D.nil
        ldelay = case _ of
            Org.Delay d ->
                dmode d.mode <> show d.value <> interval d.interval
            >>> D.text
        dateTime = case _ of
            Org.OrgDateTime
                { active, date, time, repeat, delay } ->
                    let
                        datetime_ =
                            [ Just $ ldate date
                            , lrange <$> time
                            , lrepeater <$> repeat
                            , ldelay <$> delay
                            ]
                            # D.spacify
                    in if active then
                        D.bracket "<" datetime_ ">"
                    else
                        D.bracket "[" datetime_ "]"


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
            ws # NEA.toArray # map layoutWords # Array.foldl (<>) D.nil
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
        howDeep idx = indent + case lt of
            Org.Bulleted -> 2
            Org.Plussed -> 2
            Org.Hyphened -> 2
            Org.Numbered -> if (idx + 1) < 10 then 3 else 4
            Org.NumberedFrom n -> if (idx + n) < 10 then 3 else 4
            Org.Alphed -> 3

    in
        D.nest' indent $ NEA.toArray $ NEA.mapWithIndex itemLine items



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
