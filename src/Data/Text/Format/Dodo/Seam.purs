module Data.Text.Format.Dodo.Seam where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String
import Data.Array (foldl, intersperse, replicate) as Array


type Markup = String


data Seam
    = Nil
    | Plain String
    | Markup Markup
    | Join Seam Seam
    | SurroundInline { left :: Markup, right :: Markup } Seam
    | SurroundBlock { above :: Markup, below :: Markup } Seam
    | Mark { marker :: Markup } Seam
    | MarkBlock { marker :: Markup } (Array Seam)
    | Space
    | Break
    | Tag { name :: String, attrs :: Array (String /\ String) } Seam


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