module Data.Text.Format.Dodo.SeamAlt where

import Prelude



import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String
import Data.Array (foldl, intersperse, replicate) as Array


type Markup = String


data SeamAlt
    = Nil
    | Single Markup
    -- | IndentBefore { width :: Int }
    | SurroundInline { spaced :: Boolean, left :: Markup, right :: Markup }
    | SurroundBlock { above :: Markup, below :: Markup }
    | Mark { marker :: Markup, spaced :: Boolean }
    | HtmlTag { name :: String, attrs :: Array (String /\ String) }


{-
instance Semigroup Seam where
    append = Join


nil :: Seam
nil = Nil


text :: String -> Seam
text = Plain


space :: Seam
space = Space


markup :: Markup -> Seam
markup = Markup


tag :: String -> Array (String /\ String) -> Seam -> Seam
tag name attrs = Tag { name, attrs }


mark :: Markup -> Seam -> Seam
mark marker = Mark { marker }


markBlock :: Markup -> Array Seam -> Seam
markBlock marker = MarkBlock { marker }


wrap :: Markup -> Seam -> Seam
wrap with = bracket_ with with


bracket :: Markup -> Seam -> Markup -> Seam
bracket left content right = bracket_ left right content


bracket_ :: Markup -> Markup -> Seam -> Seam
bracket_ left right = SurroundInline { left, right }


embrace :: Markup -> Seam -> Seam
embrace with = surround_ with with


surround :: Markup -> Seam -> Markup -> Seam
surround above content below = surround_ above below content


surround_ :: Markup -> Markup -> Seam -> Seam
surround_ above below = SurroundBlock { above, below }


break :: Seam
break = Break


-- nl :: Seam
-- nl = break -- Not to confuse with `nil`


replicate :: Int -> Seam -> Seam
replicate n = join <<< Array.replicate n


join :: Array Seam -> Seam
join = Array.foldl (<>) nil


joinWith :: Seam -> Array Seam -> Seam
joinWith bw = Array.intersperse bw >>> join


render :: Seam -> String
render = case _ of
    Nil -> ""
    Plain str -> str
    Markup str -> str
    Indent { width } seam -> (String.joinWith "" $ Array.replicate width " ") <> render seam
    Mark { marker } seam -> marker <> " " <> render seam
    Join seamA seamB -> render seamA <> render seamB
    SurroundInline { left, right } seam ->
        left <> render seam <> right
    SurroundBlock { above, below } seam ->
        above <> "\n" <> render seam <> "\n" <> below
    MarkBlock { marker } seams ->
        String.joinWith "\n" $ (\seam -> marker <> " " <> render seam) <$> seams
    Space -> " "
    Break -> "\n"
    Tag { name, attrs } seam ->
        case attrs of
            [] -> "<" <> name <> ">" <> render seam <> "</" <> name <> ">"
            theAttrs ->
                "<" <> name <> " "
                    <> String.joinWith " " ((\(attrName /\ attrValue) -> attrName <> "=\"" <> attrValue <> "\"") <$> theAttrs) <>
                ">"
                <> render seam
                <> "</" <> name <> ">"


instance Show Seam where
    show = case _ of
        Nil -> "<nil>"
        Space -> "<sp>"
        Break -> "<br>"
        Indent { width } seam -> "<ind:" <> show width <> "("<> show seam <>")>"
        Mark { marker } seam -> "<mark:" <> show marker <> "("<> show seam <>")>"
        MarkBlock { marker } seams -> "<mark-block:" <> show marker <> "("<> String.joinWith "," (show <$> seams) <>")>"
        SurroundInline { left, right } seam ->
            "<surrond:" <> show left <> "," <> show right <> "(" <> show seam <> ")"
        SurroundBlock { above, below } seam ->
            "<surrond-block:" <> show above <> "," <> show below <> "(" <> show seam <> ")"
        Plain str ->
            "<text(" <> str <> ")>"
        Markup str ->
            "<markup(" <> str <> ")>"
        Join seamA seamB ->
            "<concat(" <> show seamA <> "," <> show seamB <> ")>"
        Tag { name, attrs } seam ->
            case attrs of
                [] -> "<tag:" <> name <> "(" <> show seam <> ")"
                theAttrs ->
                     "<tag:" <> name <> "..." <> "(" <> show seam <> ")"
                        -- <> String.joinWith " " ((\(attrName /\ attrValue) -> attrName <> "=\"" <> attrValue <> "\"") <$> theAttrs) <>
-}