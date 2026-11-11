{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Compared with the real prettyprinter, run from its source (it is not
-- shipped with GHC):
--
--   P=~/.cache/aihc/hackage/prettyprinter-1.7.2
--   runghc --ghc-arg=-package-env --ghc-arg=- --ghc-arg=-XHaskell2010 \
--     --ghc-arg=-i$P/src --ghc-arg=-I$P/misc \
--     --ghc-arg=-optP-Wno-expansion-to-defined \
--     tests/prettyprinter.hs > tests/prettyprinter.stdout

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)

at :: Int -> Doc ann -> String
at w doc = renderString (layoutPretty (LayoutOptions (AvailablePerLine w 1)) doc)

smartAt :: Int -> Doc ann -> String
smartAt w doc = renderString (layoutSmart (LayoutOptions (AvailablePerLine w 1)) doc)

unbounded :: Doc ann -> String
unbounded doc = renderString (layoutPretty (LayoutOptions Unbounded) doc)

-- The document at several widths, each under a ruler of its width.
shown :: String -> Doc ann -> IO ()
shown name doc = do
  putStrLn ("== " ++ name)
  mapM_ one [80, 40, 24, 12, 5]
  putStrLn "(unbounded)"
  putStrLn (unbounded doc)
  where
    one w = do
      putStrLn ("|" ++ replicate (w - 2) '-' ++ "| " ++ show w)
      putStrLn (at w doc)

smart :: String -> Doc ann -> IO ()
smart name doc = do
  putStrLn ("== " ++ name ++ ", pretty and smart")
  mapM_ one [40, 24, 12]
  where
    one w = do
      putStrLn ("|" ++ replicate (w - 2) '-' ++ "| " ++ show w)
      putStrLn (at w doc)
      putStrLn "~"
      putStrLn (smartAt w doc)

-- ---- A small language, printed as AIHC prints its own ----

data Expr
  = Var String
  | Lit Int
  | App Expr [Expr]
  | Lam [String] Expr
  | Let [(String, Expr)] Expr
  | Case Expr [(String, [String], Expr)]
  | Record [(String, Expr)]

expr :: Expr -> Doc ann
expr e = case e of
  Var name -> pretty name
  Lit n -> pretty n
  App f args -> group (nest 2 (vsep (atom f : map atom args)))
  Lam names body -> group (nest 2 ("\\" <> hsep (map pretty names) <+> "->" <> line <> expr body))
  Let bindings body ->
    group (vsep ["let" <+> align (vsep [pretty n <+> "=" <+> expr b | (n, b) <- bindings]), "in" <+> expr body])
  Case scrutinee alternatives ->
    "case" <+> expr scrutinee <+> "of" <> hardline
      <> indent 2 (vsep [hsep (map pretty (c : names)) <+> "->" <+> align (expr body) | (c, names, body) <- alternatives])
  Record fields ->
    group (encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", " [pretty n <+> "=" <+> expr v | (n, v) <- fields])

atom :: Expr -> Doc ann
atom e = case e of
  Var _ -> expr e
  Lit _ -> expr e
  Record _ -> expr e
  _ -> parens (expr e)

example :: Expr
example =
  Let
    [ ("compose", Lam ["f", "g", "x"] (App (Var "f") [App (Var "g") [Var "x"]])),
      ("point", Record [("x", Lit 1), ("y", Lit 20), ("label", App (Var "show") [Lit 300])])
    ]
    ( Case
        (App (Var "lookup") [Var "key", Var "table"])
        [ ("Nothing", [], App (Var "error") [Var "message"]),
          ("Just", ["value"], App (Var "compose") [Var "render", Lam ["v"] (App (Var "scale") [Var "v", Lit 2]), Var "value"])
        ]
    )

data Colour = Red | Green
  deriving (Show)

instance Pretty Colour

newtype Name = Name String

instance Pretty Name where
  pretty (Name n) = "<" <> pretty n <> ">"

main :: IO ()
main = do
  -- Text and the basic pieces.
  shown "words" (hsep (map pretty (words "the quick brown fox jumps over the lazy dog")))
  shown "vsep" (vsep ["one", "two", "three"])
  shown "sep" (sep ["one", "two", "three"])
  shown "cat" (cat ["one", "two", "three"])
  shown "hcat, vcat" (hcat ["a", "b"] <+> vcat ["c", "d"])
  shown "fillSep" (fillSep (map pretty (words "a paragraph of text that is filled so that every line is as long as it can be")))
  shown "fillCat" (fillCat (map pretty (words "abc def ghi jkl mno pqr stu vwx yz")))
  shown "group of lines" (group ("a" <> line <> "b" <> line' <> "c"))
  shown "softline" ("left" <> softline <> "right" <> softline' <> "end")
  shown "hardline in a group" (group ("a" <> line <> "b" <> hardline <> "c" <> line <> "d"))
  shown "flatAlt" (group (flatAlt "broken" "flat" <> line <> "x"))
  shown "text with newlines" (pretty ("first\nsecond\n\nfourth" :: Text) <+> "end")
  shown "string with a newline" ("a" <+> pretty ("b\nc" :: String))
  shown "empty pieces" (mempty <> "a" <> emptyDoc <+> mempty <+> "b" <> vsep [] <> hsep [mempty])

  -- Indentation.
  shown "nest" ("begin" <> nest 4 (line <> "one" <> line <> "two") <> line <> "end")
  shown "nest in a group" (group ("begin" <> nest 4 (line <> "one" <> line <> "two") <> line <> "end"))
  shown "nest by nothing and less" (nest 0 ("a" <> line <> "b") <> nest 3 (nest (-2) (line <> "c")))
  shown "align" ("prefix" <+> align (vsep ["one", "two", "three"]))
  shown "hang" ("prefix" <+> hang 2 (fillSep (map pretty (words "some words that hang under the first one of them"))))
  shown "indent" ("prefix" <+> indent 3 (vsep ["one", "two"]) <> line <> indent 2 "three")
  shown "column and nesting" ("ab" <> column (\c -> "col" <+> pretty c) <> nest 5 (line <> nesting (\n -> "nesting" <+> pretty n)))
  shown "width" (width "four" (\w -> " is" <+> pretty w <+> "wide"))
  shown "fill" (vsep [fill 8 (pretty name) <+> "::" <+> pretty ty | (name, ty) <- [("id" :: String, "a -> a" :: String), ("const", "a -> b -> a"), ("composition", "..")]])
  shown "fillBreak" (vsep [fillBreak 8 (pretty name) <+> "::" <+> pretty ty | (name, ty) <- [("id" :: String, "a -> a" :: String), ("composition", "..")]])
  shown "pageWidth" (pageWidth (\w -> case w of AvailablePerLine n _ -> "width" <+> pretty n; Unbounded -> "no width"))

  -- Lists and brackets.
  shown "list" (list (map pretty [1 :: Int .. 12]))
  shown "list of lists" (list [list (map pretty [n .. n + 4]) | n <- [1 :: Int, 100, 10000]])
  shown "tupled" (tupled ["first", "second", "third"])
  shown "empty and single" (list [] <+> tupled [] <+> list ["x"] <+> tupled ["y"])
  shown "encloseSep" ("f" <> encloseSep "<" ">" "; " ["alpha", "beta", "gamma"])
  shown "punctuate" (hsep (punctuate comma ["a", "b", "c"]) <+> hsep (punctuate comma []) <+> hsep (punctuate semi ["z"]))
  shown "symbols" (hsep [parens "a", brackets "b", braces "c", angles "d", squotes "e", dquotes "f", lparen <> rparen, lbracket <> rbracket, lbrace <> rbrace, langle <> rangle, squote, dquote, semi, colon, comma, dot, slash, backslash, equals, pipe] <> space <> "|")
  shown "enclose, surround" (enclose "<" ">" "x" <+> surround "." "a" "b")
  shown "concatWith" (concatWith (\a b -> a <> "/" <> b) ["usr", "local", "bin"] <+> concatWith (<+>) ([] :: [Doc ()]))

  -- Values.
  shown "numbers" (hsep [pretty (42 :: Int), pretty (-7 :: Integer), pretty (maxBound :: Int)])
  shown "others" (hsep [pretty True, pretty (), pretty 'c', pretty (Just 'j'), pretty (Nothing :: Maybe Int), "|"])
  shown "tuples" (pretty (1 :: Int, "two" :: String) <+> pretty ('a', True, 3 :: Int))
  shown "lists of values" (pretty [1 :: Int, 2, 3] <+> pretty ["ab" :: String, "cd"] <+> pretty [Just 'x', Nothing, Just 'y'] <+> pretty ('n' :| "onempty"))
  shown "long list of values" (pretty [1000000 :: Int .. 1000012])
  shown "viaShow" (viaShow (Just [1 :: Int, 2]) <+> unsafeViaShow 'q' <+> pretty Red <+> pretty [Red, Green])
  shown "own instance" (pretty (Name "n") <+> pretty [Name "a", Name "b"] <+> pretty (Just (Name "m")))
  shown "plural" (pretty (1 :: Int) <+> plural "thing" "things" (1 :: Int) <> comma <+> pretty (3 :: Int) <+> plural "thing" "things" (3 :: Int))

  -- Annotations do not change the layout.
  shown "annotate" (annotate 'x' ("a" <+> annotate 'y' (group ("b" <> line <> "c"))) <+> unAnnotate (annotate () "d") <+> reAnnotate succ (annotate 'p' "e"))
  shown "fmap" (fmap not (annotate True (sep ["f", "g"])))

  -- The language.
  shown "expression" (expr example)
  shown "application" (expr (App (Var "function") [Var "argument", App (Var "another") [Lit 1, Lit 2, Lit 3], Record [("field", Lit 1), ("other", Var "value")]]))
  shown "record" (expr (Record [(name, Lit n) | (name, n) <- zip ["alpha", "beta", "gamma", "delta"] [1 ..]]))

  -- Where layoutSmart looks further than the current line.
  smart "function call" ("fun" <> align (list ["abcdef", "ghijklm"]) <> "(" <> align (list ["abcdef", "ghijklm"]) <> ")")
  smart "nested calls" (nest 2 ("outer" <> parens (line' <> nest 2 ("inner" <> parens (line' <> vsep (punctuate comma ["first argument", "second argument"]))) <> line')))
  smart "expression" (expr example)

  -- Text, and show.
  putStrLn (T.unpack (renderStrict (layoutPretty defaultLayoutOptions (vsep ["as text", indent 2 "indented"]))))
  print (renderStrict (layoutPretty (LayoutOptions (AvailablePerLine 10 1)) (fillSep (map pretty (words "a few short words in a row")))))
  print ("shown" <+> list ["a", "b"] :: Doc ())
  putStrLn (case layoutPageWidth defaultLayoutOptions of AvailablePerLine w _ -> "default width " ++ show w; Unbounded -> "unbounded")
