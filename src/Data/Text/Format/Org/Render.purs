module Data.Text.Format.Org.Render where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (toUpper) as String
import Data.String.CodeUnits (singleton) as String
import Data.Char (fromCharCode)
import Data.Text.Doc (Doc, (<+>), (</>), (<//>))
import Data.Text.Doc as D
import Data.Text.Format.Org.Construct as Org
import Data.Text.Format.Org.Types (OrgFile(..), OrgDoc(..))
import Data.Text.Format.Org.Types as Org
import Data.Tuple.Nested ((/\), type (/\))


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
    Org.Quote str -> D.nest indent $ D.text str -- TODO: replace breaks, add markup
    Org.Example str -> D.nest indent $ D.text str -- TODO: replace breaks, add markup
    Org.Code maybeLang str -> D.nest indent $ D.text str -- TODO: replace breaks, add markup
    Org.List items -> layoutItems (deepToIndent deep) items
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
                            Org.TODO -> "TODO"
                            Org.DOING -> "DOING"
                            Org.DONE -> "DONE"
                            Org.Custom s -> s
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
            # Array.catMaybes
            # Array.intersperse D.space
            # Array.foldl (<>) D.nil
    in
        if not $ Org.isDocEmpty section.doc then
            everythingCombined </> layoutDoc (deepAs section.level) section.doc
        else
            everythingCombined <> D.break


layoutWords :: Org.Words -> Doc
layoutWords = case _ of
    Org.Plain plain -> D.text plain
    Org.Break -> D.break
    _ -> D.nil


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
            # Array.catMaybes
            # Array.intersperse D.space
            # Array.foldl (<>) D.nil
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
