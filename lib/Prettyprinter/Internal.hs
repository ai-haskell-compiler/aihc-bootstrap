-- This file is derived from the package prettyprinter, Copyright 2008, Daan
-- Leijen and Max Bolingbroke, 2016 David Luposchainsky, and is under that
-- package's licence: see licenses/prettyprinter.txt.

module Prettyprinter.Internal where

-- Documents that are laid out to fit a page width. AIHC writes files with
-- this, so the definitions are upstream's: the same document gives the
-- same text.
--
-- One difference: the ribbon fraction of a page width is an Int here,
-- because the interpreter has no Double yet. AIHC only uses 1.

import Data.Functor.Identity (Identity (..))
import Data.Int
import Data.List (foldr1)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void, absurd)
import Data.Word

infixr 6 <+>

data Doc ann
  = -- Cannot be laid out: what a line break becomes when it is flattened.
    Fail
  | Empty
  | Char Char
  | -- The length of the text, and the text, which has no newline.
    Text Int Text
  | -- A line break that cannot be flattened.
    Line
  | -- The first document; the second one if the group it is in is flat.
    FlatAlt (Doc ann) (Doc ann)
  | Cat (Doc ann) (Doc ann)
  | -- The document with its indentation increased.
    Nest Int (Doc ann)
  | -- The first document if it fits, else the second. The first lines of
    -- the first are at least as long as those of the second.
    Union (Doc ann) (Doc ann)
  | -- A document that depends on the column it starts at.
    Column (Int -> Doc ann)
  | WithPageWidth (PageWidth -> Doc ann)
  | -- A document that depends on the indentation it is at.
    Nesting (Int -> Doc ann)
  | Annotated ann (Doc ann)

instance Semigroup (Doc ann) where
  (<>) = Cat

instance Monoid (Doc ann) where
  mempty = emptyDoc

instance IsString (Doc ann) where
  fromString string = pretty (T.pack string)

instance Functor Doc where
  fmap = reAnnotate

instance Show (Doc ann) where
  showsPrec _ doc = renderShowS (layoutPretty defaultLayoutOptions doc)

-- ---- Values as documents ----

class Pretty a where
  pretty :: a -> Doc ann
  pretty = viaShow

  -- How a list of them looks. It is there so that a list of characters
  -- can be a string.
  prettyList :: [a] -> Doc ann
  prettyList list' = align (list (map pretty list'))

instance Pretty a => Pretty (Identity a) where
  pretty (Identity a) = pretty a

instance Pretty a => Pretty [a] where
  pretty = prettyList

instance Pretty a => Pretty (NonEmpty a) where
  pretty (x :| xs) = prettyList (x : xs)

instance Pretty () where
  pretty _ = unsafeTextWithoutNewlines (T.pack "()")

instance Pretty Bool where
  pretty b = unsafeTextWithoutNewlines (T.pack (if b then "True" else "False"))

instance Pretty Char where
  pretty c = if c == '\n' then line else Char c
  prettyList string = pretty (T.pack string)

viaShow :: Show a => a -> Doc ann
viaShow a = pretty (T.pack (show a))

-- For a value whose `show` has no newline in it.
unsafeViaShow :: Show a => a -> Doc ann
unsafeViaShow a = unsafeTextWithoutNewlines (T.pack (show a))

instance Pretty Int where
  pretty = unsafeViaShow

instance Pretty Int8 where
  pretty = unsafeViaShow

instance Pretty Int16 where
  pretty = unsafeViaShow

instance Pretty Int32 where
  pretty = unsafeViaShow

instance Pretty Int64 where
  pretty = unsafeViaShow

instance Pretty Word where
  pretty = unsafeViaShow

instance Pretty Word8 where
  pretty = unsafeViaShow

instance Pretty Word16 where
  pretty = unsafeViaShow

instance Pretty Word32 where
  pretty = unsafeViaShow

instance Pretty Word64 where
  pretty = unsafeViaShow

instance Pretty Integer where
  pretty = unsafeViaShow

instance (Pretty a1, Pretty a2) => Pretty (a1, a2) where
  pretty (x1, x2) = tupled [pretty x1, pretty x2]

instance (Pretty a1, Pretty a2, Pretty a3) => Pretty (a1, a2, a3) where
  pretty (x1, x2, x3) = tupled [pretty x1, pretty x2, pretty x3]

instance Pretty a => Pretty (Maybe a) where
  pretty = maybe mempty pretty
  prettyList list' = prettyList (catMaybes list')

-- Each newline of the text is a `line`.
instance Pretty Text where
  pretty text = vsep (map unsafeTextWithoutNewlines (T.splitOn (T.pack "\n") text))

instance Pretty Void where
  pretty = absurd

unsafeTextWithoutNewlines :: Text -> Doc ann
unsafeTextWithoutNewlines text = case T.uncons text of
  Nothing -> Empty
  Just (c, rest) -> if T.null rest then Char c else Text (T.length text) text

-- ---- Basic documents ----

emptyDoc :: Doc ann
emptyDoc = Empty

-- The document with the lines after its first indented by that much more.
nest :: Int -> Doc ann -> Doc ann
nest i x = if i == 0 then x else Nest i x

-- A line break, or a space in a group that fits on one line.
line :: Doc ann
line = FlatAlt Line (Char ' ')

-- A line break, or nothing in a group that fits on one line.
line' :: Doc ann
line' = FlatAlt Line mempty

-- A space, or a line break if what follows does not fit.
softline :: Doc ann
softline = Union (Char ' ') Line

softline' :: Doc ann
softline' = Union mempty Line

-- A line break, always.
hardline :: Doc ann
hardline = Line

-- The document on one line if that fits, else as it is.
group :: Doc ann -> Doc ann
group x = case x of
  Union _ _ -> x
  FlatAlt a b -> case changesUponFlattening b of
    Flattened b' -> Union b' a
    AlreadyFlat -> Union b a
    NeverFlat -> a
  _ -> case changesUponFlattening x of
    Flattened x' -> Union x' x
    AlreadyFlat -> x
    NeverFlat -> x

data FlattenResult a
  = Flattened a
  | AlreadyFlat
  | NeverFlat

instance Functor FlattenResult where
  fmap f result = case result of
    Flattened a -> Flattened (f a)
    AlreadyFlat -> AlreadyFlat
    NeverFlat -> NeverFlat

-- The document with every choice made for one line, if that changes it.
changesUponFlattening :: Doc ann -> FlattenResult (Doc ann)
changesUponFlattening doc = case doc of
  FlatAlt _ y -> Flattened (flatten y)
  Line -> NeverFlat
  Union x _ -> Flattened x
  Nest i x -> fmap (Nest i) (changesUponFlattening x)
  Annotated ann x -> fmap (Annotated ann) (changesUponFlattening x)
  Column f -> Flattened (Column (flatten . f))
  Nesting f -> Flattened (Nesting (flatten . f))
  WithPageWidth f -> Flattened (WithPageWidth (flatten . f))
  Cat x y -> case changesUponFlattening x of
    NeverFlat -> NeverFlat
    Flattened x' -> case changesUponFlattening y of
      NeverFlat -> NeverFlat
      Flattened y' -> Flattened (Cat x' y')
      AlreadyFlat -> Flattened (Cat x' y)
    AlreadyFlat -> case changesUponFlattening y of
      NeverFlat -> NeverFlat
      Flattened y' -> Flattened (Cat x y')
      AlreadyFlat -> AlreadyFlat
  Empty -> AlreadyFlat
  Char _ -> AlreadyFlat
  Text _ _ -> AlreadyFlat
  Fail -> NeverFlat

flatten :: Doc ann -> Doc ann
flatten doc = case doc of
  FlatAlt _ y -> flatten y
  Cat x y -> Cat (flatten x) (flatten y)
  Nest i x -> Nest i (flatten x)
  Line -> Fail
  Union x _ -> flatten x
  Column f -> Column (flatten . f)
  WithPageWidth f -> WithPageWidth (flatten . f)
  Nesting f -> Nesting (flatten . f)
  Annotated ann x -> Annotated ann (flatten x)
  _ -> doc

-- The first document; the second one if the group it is in fits on a line.
flatAlt :: Doc ann -> Doc ann -> Doc ann
flatAlt = FlatAlt

-- ---- Alignment ----

-- The document with its lines indented to the column it starts at.
align :: Doc ann -> Doc ann
align d = column (\k -> nesting (\i -> nest (k - i) d))

-- As align, with the lines after the first indented by that much more.
hang :: Int -> Doc ann -> Doc ann
hang i d = align (nest i d)

-- The document moved to the right by that many columns.
indent :: Int -> Doc ann -> Doc ann
indent i d = hang i (spaces i <> d)

-- The documents between the delimiters, with the separator in front of
-- each but the first: on one line if they fit, else one under another.
encloseSep :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSep l r s ds = case ds of
  [] -> l <> r
  [d] -> l <> d <> r
  _ -> cat (zipWith (<>) (l : repeat s) ds) <> r

list :: [Doc ann] -> Doc ann
list docs = group (encloseSep (flatAlt (text "[ ") (text "[")) (flatAlt (text " ]") (text "]")) (text ", ") docs)

tupled :: [Doc ann] -> Doc ann
tupled docs = group (encloseSep (flatAlt (text "( ") (text "(")) (flatAlt (text " )") (text ")")) (text ", ") docs)

-- A string without newlines as a document.
text :: String -> Doc ann
text string = unsafeTextWithoutNewlines (T.pack string)

-- ---- Putting documents together ----

-- With a space between them.
(<+>) :: Doc ann -> Doc ann -> Doc ann
(<+>) x y = x <> Char ' ' <> y

concatWith :: Foldable t => (Doc ann -> Doc ann -> Doc ann) -> t (Doc ann) -> Doc ann
concatWith f ds = case foldr (:) [] ds of
  [] -> mempty
  docs -> foldr1 f docs

hsep :: [Doc ann] -> Doc ann
hsep = concatWith (<+>)

vsep :: [Doc ann] -> Doc ann
vsep = concatWith (\x y -> x <> line <> y)

-- As many on each line as fit.
fillSep :: [Doc ann] -> Doc ann
fillSep = concatWith (\x y -> x <> softline <> y)

-- hsep if it fits, else vsep.
sep :: [Doc ann] -> Doc ann
sep docs = group (vsep docs)

hcat :: [Doc ann] -> Doc ann
hcat = concatWith (<>)

vcat :: [Doc ann] -> Doc ann
vcat = concatWith (\x y -> x <> line' <> y)

fillCat :: [Doc ann] -> Doc ann
fillCat = concatWith (\x y -> x <> softline' <> y)

-- hcat if it fits, else vcat.
cat :: [Doc ann] -> Doc ann
cat docs = group (vcat docs)

-- The punctuation after each document but the last.
punctuate :: Doc ann -> [Doc ann] -> [Doc ann]
punctuate p docs = case docs of
  [] -> []
  [d] -> [d]
  d : ds -> (d <> p) : punctuate p ds

column :: (Int -> Doc ann) -> Doc ann
column = Column

nesting :: (Int -> Doc ann) -> Doc ann
nesting = Nesting

-- The second document is given the width of the first.
width :: Doc ann -> (Int -> Doc ann) -> Doc ann
width doc f = column (\colStart -> doc <> column (\colEnd -> f (colEnd - colStart)))

pageWidth :: (PageWidth -> Doc ann) -> Doc ann
pageWidth = WithPageWidth

-- The document with spaces after it until it is that wide.
fill :: Int -> Doc ann -> Doc ann
fill n doc = width doc (\w -> spaces (n - w))

-- As fill, but a document that is too wide gets a line break after it.
fillBreak :: Int -> Doc ann -> Doc ann
fillBreak f x = width x (\w -> if w > f then nest f line' else spaces (f - w))

spaces :: Int -> Doc ann
spaces n
  | n <= 0 = Empty
  | n == 1 = Char ' '
  | otherwise = Text n (textSpaces n)

textSpaces :: Int -> Text
textSpaces n = T.replicate n (T.pack " ")

plural :: (Num amount, Eq amount) => doc -> doc -> amount -> doc
plural one multiple n = if n == 1 then one else multiple

enclose :: Doc ann -> Doc ann -> Doc ann -> Doc ann
enclose l r x = l <> x <> r

surround :: Doc ann -> Doc ann -> Doc ann -> Doc ann
surround x l r = l <> x <> r

-- ---- Annotations ----

annotate :: ann -> Doc ann -> Doc ann
annotate = Annotated

unAnnotate :: Doc ann -> Doc xxx
unAnnotate = alterAnnotations (const [])

reAnnotate :: (ann -> ann') -> Doc ann -> Doc ann'
reAnnotate re = alterAnnotations (\ann -> [re ann])

alterAnnotations :: (ann -> [ann']) -> Doc ann -> Doc ann'
alterAnnotations re doc = case doc of
  Fail -> Fail
  Empty -> Empty
  Char c -> Char c
  Text l t -> Text l t
  Line -> Line
  FlatAlt x y -> FlatAlt (alterAnnotations re x) (alterAnnotations re y)
  Cat x y -> Cat (alterAnnotations re x) (alterAnnotations re y)
  Nest i x -> Nest i (alterAnnotations re x)
  Union x y -> Union (alterAnnotations re x) (alterAnnotations re y)
  Column f -> Column (alterAnnotations re . f)
  WithPageWidth f -> WithPageWidth (alterAnnotations re . f)
  Nesting f -> Nesting (alterAnnotations re . f)
  Annotated ann x -> foldr Annotated (alterAnnotations re x) (re ann)

-- ---- Laying out ----

-- A document with every choice made: text and line breaks, in order.
data SimpleDocStream ann
  = SFail
  | SEmpty
  | SChar Char (SimpleDocStream ann)
  | SText Int Text (SimpleDocStream ann)
  | -- A line break, and the indentation of the new line.
    SLine Int (SimpleDocStream ann)
  | SAnnPush ann (SimpleDocStream ann)
  | SAnnPop (SimpleDocStream ann)

-- The documents still to lay out, each with its indentation.
data LayoutPipeline ann
  = Nil
  | Cons Int (Doc ann) (LayoutPipeline ann)
  | UndoAnn (LayoutPipeline ann)

-- The width of a line, and how much of it text may take (the ribbon), not
-- counting indentation. Upstream's ribbon is a fraction of the width; here
-- it is a whole number, and 1 means all of it.
data PageWidth
  = AvailablePerLine Int Int
  | Unbounded
  deriving (Eq, Ord, Show)

defaultPageWidth :: PageWidth
defaultPageWidth = AvailablePerLine 80 1

-- How many columns are left, on a line indented by `lineIndent` with the
-- cursor at `currentColumn`.
remainingWidth :: Int -> Int -> Int -> Int -> Int
remainingWidth lineLength ribbonFraction lineIndent currentColumn =
  min columnsLeftInLine columnsLeftInRibbon
  where
    columnsLeftInLine = lineLength - currentColumn
    columnsLeftInRibbon = lineIndent + ribbonWidth - currentColumn
    ribbonWidth = max 0 (min lineLength (lineLength * ribbonFraction))

newtype LayoutOptions = LayoutOptions {layoutPageWidth :: PageWidth}
  deriving (Eq, Ord, Show)

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = LayoutOptions defaultPageWidth

-- A choice fits if what it puts on the current line fits.
layoutPretty :: LayoutOptions -> Doc ann -> SimpleDocStream ann
layoutPretty (LayoutOptions pageWidth_) = case pageWidth_ of
  AvailablePerLine lineLength ribbonFraction ->
    layoutWadlerLeijen
      (\lineIndent currentColumn _ sdoc ->
         fitsOnLine (remainingWidth lineLength ribbonFraction lineIndent currentColumn) sdoc)
      pageWidth_
  Unbounded -> layoutUnbounded

-- Whether the stream fits in that many columns up to its first line break.
fitsOnLine :: Int -> SimpleDocStream ann -> Bool
fitsOnLine w sdoc
  | w < 0 = False
  | otherwise = case sdoc of
      SFail -> False
      SEmpty -> True
      SChar _ x -> fitsOnLine (w - 1) x
      SText l _ x -> fitsOnLine (w - l) x
      SLine _ _ -> True
      SAnnPush _ x -> fitsOnLine w x
      SAnnPop x -> fitsOnLine w x

-- As layoutPretty, but a choice also has to fit on the lines after it that
-- are indented more than where it started.
layoutSmart :: LayoutOptions -> Doc ann -> SimpleDocStream ann
layoutSmart (LayoutOptions pageWidth_) = case pageWidth_ of
  AvailablePerLine lineLength ribbonFraction ->
    layoutWadlerLeijen
      (\lineIndent currentColumn initialIndentY sdoc ->
         let minNestingLevel = case initialIndentY of
               Just i -> min i currentColumn
               Nothing -> currentColumn
             go w stream
               | w < 0 = False
               | otherwise = case stream of
                   SFail -> False
                   SEmpty -> True
                   SChar _ x -> go (w - 1) x
                   SText l _ x -> go (w - l) x
                   SLine i x -> if minNestingLevel < i then go (lineLength - i) x else True
                   SAnnPush _ x -> go w x
                   SAnnPop x -> go w x
          in go (remainingWidth lineLength ribbonFraction lineIndent currentColumn) sdoc)
      pageWidth_
  Unbounded -> layoutUnbounded

layoutUnbounded :: Doc ann -> SimpleDocStream ann
layoutUnbounded = layoutWadlerLeijen (\_ _ _ sdoc -> not (failsOnFirstLine sdoc)) Unbounded

failsOnFirstLine :: SimpleDocStream ann -> Bool
failsOnFirstLine sds = case sds of
  SFail -> True
  SEmpty -> False
  SChar _ s -> failsOnFirstLine s
  SText _ _ s -> failsOnFirstLine s
  SLine _ _ -> False
  SAnnPush _ s -> failsOnFirstLine s
  SAnnPop s -> failsOnFirstLine s

-- Lays the document out, taking the first of two choices whenever the
-- function says it fits. The function is given the indentation of the
-- current line, the current column, the indentation the second choice
-- starts its next line with (if it starts with a line break), and the
-- first choice laid out.
layoutWadlerLeijen ::
  (Int -> Int -> Maybe Int -> SimpleDocStream ann -> Bool) ->
  PageWidth ->
  Doc ann ->
  SimpleDocStream ann
layoutWadlerLeijen fits pageWidth_ doc = best 0 0 (Cons 0 doc Nil)
  where
    -- `nl` is the indentation of the current line, `cc` the current
    -- column.
    best nl cc pipeline = case pipeline of
      Nil -> SEmpty
      UndoAnn ds -> SAnnPop (best nl cc ds)
      Cons i d ds -> case d of
        Fail -> SFail
        Empty -> best nl cc ds
        Char c -> SChar c (best nl (cc + 1) ds)
        Text l t -> SText l t (best nl (cc + l) ds)
        Line ->
          let x = best i i ds
              -- No indentation on a line that has nothing on it.
              i' = case x of
                SEmpty -> 0
                SLine _ _ -> 0
                _ -> i
           in SLine i' x
        FlatAlt x _ -> best nl cc (Cons i x ds)
        Cat x y -> best nl cc (Cons i x (Cons i y ds))
        Nest j x -> best nl cc (Cons (i + j) x ds)
        Union x y ->
          let x' = best nl cc (Cons i x ds)
              y' = best nl cc (Cons i y ds)
           in if fits nl cc (initialIndentation y') x' then x' else y'
        Column f -> best nl cc (Cons i (f cc) ds)
        WithPageWidth f -> best nl cc (Cons i (f pageWidth_) ds)
        Nesting f -> best nl cc (Cons i (f i) ds)
        Annotated ann x -> SAnnPush ann (best nl cc (Cons i x (UndoAnn ds)))

initialIndentation :: SimpleDocStream ann -> Maybe Int
initialIndentation sds = case sds of
  SLine i _ -> Just i
  SAnnPush _ s -> initialIndentation s
  SAnnPop s -> initialIndentation s
  _ -> Nothing

-- ---- As text ----

renderShowS :: SimpleDocStream ann -> ShowS
renderShowS sds rest = case sds of
  SFail -> error "Prettyprinter: a document that cannot be laid out"
  SEmpty -> rest
  SChar c x -> c : renderShowS x rest
  SText _ t x -> T.unpack t ++ renderShowS x rest
  SLine i x -> '\n' : (replicate i ' ' ++ renderShowS x rest)
  SAnnPush _ x -> renderShowS x rest
  SAnnPop x -> renderShowS x rest

-- ---- Symbols ----

squotes :: Doc ann -> Doc ann
squotes = enclose squote squote

dquotes :: Doc ann -> Doc ann
dquotes = enclose dquote dquote

parens :: Doc ann -> Doc ann
parens = enclose lparen rparen

angles :: Doc ann -> Doc ann
angles = enclose langle rangle

brackets :: Doc ann -> Doc ann
brackets = enclose lbracket rbracket

braces :: Doc ann -> Doc ann
braces = enclose lbrace rbrace

squote :: Doc ann
squote = Char '\''

dquote :: Doc ann
dquote = Char '"'

lparen :: Doc ann
lparen = Char '('

rparen :: Doc ann
rparen = Char ')'

langle :: Doc ann
langle = Char '<'

rangle :: Doc ann
rangle = Char '>'

lbracket :: Doc ann
lbracket = Char '['

rbracket :: Doc ann
rbracket = Char ']'

lbrace :: Doc ann
lbrace = Char '{'

rbrace :: Doc ann
rbrace = Char '}'

semi :: Doc ann
semi = Char ';'

colon :: Doc ann
colon = Char ':'

comma :: Doc ann
comma = Char ','

space :: Doc ann
space = Char ' '

dot :: Doc ann
dot = Char '.'

slash :: Doc ann
slash = Char '/'

backslash :: Doc ann
backslash = Char '\\'

equals :: Doc ann
equals = Char '='

pipe :: Doc ann
pipe = Char '|'
