{-# LANGUAGE TypeFamilies #-}

module Main where

-- Compared with the real megaparsec, run from its source (it is not
-- shipped with GHC):
--
--   H=~/.cache/aihc/hackage
--   runghc --ghc-arg=-package-env --ghc-arg=- --ghc-arg=-XHaskell2010 \
--     --ghc-arg=-i$H/megaparsec-9.8.3 --ghc-arg=-i$H/parser-combinators-1.3.1 \
--     --ghc-arg=-itests/ghc-stubs tests/megaparsec.hs > tests/megaparsec.stdout

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import qualified Text.Megaparsec as MPC
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Internal as MPI

data Custom = Custom String Int
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Custom where
  showErrorComponent (Custom message n) = message ++ " (" ++ show n ++ ")"

type Parser = Parsec Custom String

type TextParser = Parsec Custom Text

report :: (Show a) => Either (ParseErrorBundle String Custom) a -> IO ()
report result = case result of
  Right a -> putStrLn ("  ok: " ++ show a)
  Left bundle -> do
    mapM_ (\e -> putStrLn ("  error: " ++ show e)) (NE.toList (bundleErrors bundle))
    putStr (unlines (map ("  | " ++) (lines (errorBundlePretty bundle))))

run :: (Show a) => String -> Parser a -> String -> IO ()
run name p input = do
  putStrLn (name ++ " on " ++ show input)
  report (parse p "test" input)

-- As run, but also what was left of the input.
runRest :: (Show a) => String -> Parser a -> String -> IO ()
runRest name p = run name ((,) <$> p <*> takeRest)

runText :: (Show a) => String -> TextParser a -> String -> IO ()
runText name p input = do
  putStrLn (name ++ " on text " ++ show input)
  case parse p "text" (T.pack input) of
    Right a -> putStrLn ("  ok: " ++ show a)
    Left bundle -> do
      mapM_ (\e -> putStrLn ("  error: " ++ show e)) (NE.toList (bundleErrors bundle))
      putStr (unlines (map ("  | " ++) (lines (errorBundlePretty bundle))))

-- ---- A small language over characters ----

sc :: Parser ()
sc = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

data Expr
  = Number Int
  | Variable String
  | Add Expr Expr
  | Multiply Expr Expr
  | Call String [Expr]
  | Let String Expr Expr
  deriving (Eq, Show)

identifier :: Parser String
identifier = lexeme ((:) <$> C.letterChar <*> many C.alphaNumChar <?> "identifier")

keyword :: String -> Parser ()
keyword word = lexeme (try (C.string word *> notFollowedBy C.alphaNumChar))

term :: Parser Expr
term =
  choice
    [ Number <$> lexeme L.decimal,
      between (symbol "(") (symbol ")") expression,
      letExpression,
      try (Call <$> identifier <*> between (symbol "(") (symbol ")") (expression `sepBy` symbol ",")),
      Variable <$> identifier
    ]

letExpression :: Parser Expr
letExpression = do
  keyword "let"
  name <- identifier
  void (symbol "=")
  bound <- expression
  keyword "in"
  Let name bound <$> expression

product' :: Parser Expr
product' = foldl Multiply <$> term <*> many (symbol "*" *> term)

expression :: Parser Expr
expression = foldl Add <$> product' <*> many (symbol "+" *> product')

program :: Parser [Expr]
program = sc *> (expression `sepEndBy` symbol ";") <* eof

-- ---- A parser with an environment, as AIHC's parsers of its own formats ----

type Scoped = ReaderT [String] (Parsec Custom Text)

-- Names have to be bound by an enclosing `\name.` to be used.
scoped :: Scoped [String]
scoped = do
  parts <- MPC.many part
  MPC.eof
  pure parts
  where
    part = MPC.try binder MPC.<|> use MPC.<?> "a part"

binder :: Scoped String
binder = do
  void (C.char '\\')
  name <- MPC.some C.lowerChar
  void (C.char '.')
  inner <- local (name :) (MPC.many (MPC.try binder MPC.<|> use))
  void (lift (C.char ';'))
  pure (name ++ "{" ++ unwords inner ++ "}")

use :: Scoped String
use = do
  offset <- MPC.getOffset
  name <- MPC.takeWhile1P (Just "letter") (`elem` ['a' .. 'z']) <* C.space
  bound <- ask
  if T.unpack name `elem` bound
    then pure (T.unpack name)
    else MPC.region (MPC.setErrorOffset offset) (MPC.customFailure (Custom ("unbound " ++ T.unpack name) (length bound)))

runScoped :: String -> IO ()
runScoped input = do
  putStrLn ("scoped on " ++ show input)
  case parse (runReaderT scoped ["top"]) "scoped" (T.pack input) of
    Right a -> putStrLn ("  ok: " ++ show a)
    Left bundle -> do
      mapM_ (\e -> putStrLn ("  error: " ++ show e)) (NE.toList (bundleErrors bundle))
      putStr (unlines (map ("  | " ++) (lines (errorBundlePretty bundle))))

-- ---- A stream of tokens, read through the parser's own state ----

data Tok = TInt Int | TName String | TOpen | TClose | TComma
  deriving (Eq, Ord, Show)

newtype Toks = Toks [Tok]
  deriving (Show)

instance Stream Toks where
  type Token Toks = Tok
  type Tokens Toks = [Tok]
  tokenToChunk _ tok = [tok]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (Toks toks) = case toks of
    [] -> Nothing
    t : ts -> Just (t, Toks ts)
  takeN_ n (Toks toks)
    | n <= 0 = Just ([], Toks toks)
    | null toks = Nothing
    | otherwise = Just (take n toks, Toks (drop n toks))
  takeWhile_ f (Toks toks) = (takeWhile f toks, Toks (dropWhile f toks))

type TokParser = Parsec Custom Toks

-- The next token without reading it, written against the parser's
-- representation, as AIHC's parser does.
peek :: TokParser (Maybe Tok)
peek = MPI.ParsecT $ \s _ _ eok _ ->
  eok (fst <$> take1_ (stateInput s)) s mempty

-- Replaces a trivial error of the parser by a custom one.
expecting :: String -> TokParser a -> TokParser a
expecting what parser =
  MPI.ParsecT $ \s cok cerr eok eerr ->
    MPI.unParser parser s cok (relabel what cerr) eok (relabel what eerr)

relabel :: String -> (ParseError Toks Custom -> State Toks Custom -> r) -> ParseError Toks Custom -> State Toks Custom -> r
relabel what k err errState = case err of
  TrivialError off _ _ -> k (FancyError off (Set.singleton (ErrorCustom (Custom what (stateOffset errState))))) errState
  _ -> k err errState

data Tree = Leaf Int | Node String [Tree]
  deriving (Show)

tree :: TokParser Tree
tree = do
  next <- peek
  case next of
    Just (TInt _) -> expecting "a number" (token (\t -> case t of TInt n -> Just (Leaf n); _ -> Nothing) Set.empty)
    Just (TName name) -> do
      void anySingle
      Node name <$> option [] (between (single TOpen) (single TClose) (tree `sepBy` single TComma))
    _ -> expecting "a tree" empty

runToks :: String -> [Tok] -> IO ()
runToks name toks = do
  putStrLn (name ++ " on " ++ show toks)
  case parse (tree <* eof) "toks" (Toks toks) of
    Right a -> putStrLn ("  ok: " ++ show a)
    Left bundle -> mapM_ (\e -> putStrLn ("  error: " ++ show e)) (NE.toList (bundleErrors bundle))

main :: IO ()
main = do
  -- One token, chunks, and what an error says.
  run "single" (single 'a') "abc"
  run "single" (single 'a') "xbc"
  run "single" (single 'a') ""
  run "satisfy" (satisfy (> 'm')) "z"
  run "satisfy" (satisfy (> 'm')) "a"
  run "anySingle" anySingle ""
  run "anySingleBut" (anySingleBut 'a') "ab"
  run "oneOf" (oneOf "abc") "d"
  run "noneOf" (noneOf "abc") "d"
  runRest "chunk" (chunk "let") "letter"
  run "chunk" (chunk "let") "lex"
  run "chunk" (chunk "let") "le"
  run "chunk" (chunk "") "x"
  run "eof" eof ""
  run "eof" eof "x"
  runRest "takeWhileP" (takeWhileP (Just "digit") (`elem` "0123456789")) "123abc"
  runRest "takeWhileP" (takeWhileP (Just "digit") (`elem` "0123456789")) "abc"
  run "takeWhileP then" (takeWhileP (Just "digit") (`elem` "0123456789") *> single 'x') "12y"
  run "takeWhile1P" (takeWhile1P (Just "digit") (`elem` "0123456789")) "abc"
  run "takeWhile1P" (takeWhile1P Nothing (`elem` "0123456789")) ""
  runRest "takeP" (takeP (Just "letter") 2) "abc"
  run "takeP" (takeP (Just "letter") 4) "abc"
  run "takeP" (takeP Nothing 0) ""

  -- Choice: errors at the same place are put together; consuming input
  -- commits to an alternative unless it is under try.
  run "choice" (single 'a' <|> single 'b' <|> single 'c') "d"
  run "choice" (chunk "ab" <|> chunk "ac") "ac"
  run "no try" ((single 'a' *> single 'b') <|> (single 'a' *> single 'c')) "ac"
  run "try" (try (single 'a' *> single 'b') <|> (single 'a' *> single 'c')) "ac"
  run "try" (try (single 'a' *> single 'b') <|> (single 'a' *> single 'c')) "ad"
  run "longer error wins" (try (chunk "ab" *> single 'x') <|> single 'q') "abc"
  run "empty" (empty :: Parser Char) "a"
  run "empty or" (empty <|> single 'a') "b"
  run "choice of none" (choice [] :: Parser Char) "a"
  run "option" (option 'z' (single 'a')) "b"
  run "optional" (optional (single 'a') <* single 'b') "c"
  run "hints after optional" (optional (single 'a') *> optional (single 'b') *> single 'c') "d"
  run "hints dropped by progress" (optional (single 'a') *> single 'x' *> single 'c') "xd"

  -- Labels.
  run "label" (single 'a' <?> "the first letter") "b"
  run "label" ((single 'a' <|> single 'b') <?> "a or b") "c"
  run "label after input" ((single 'a' *> single 'b') <?> "ab") "ac"
  run "hidden" (hidden (single 'a') <|> single 'b') "c"
  run "hidden only" (hidden (single 'a')) "c"
  run "label on success" (many (single 'a' <?> "an a") *> single 'b') "aac"
  run "label of many" ((many (single 'a') <?> "some a") *> single 'b') "c"

  -- Repetition.
  runRest "many" (many (single 'a')) "aaab"
  run "many then" (many (single 'a') *> single 'b') "aac"
  run "many of consuming failure" (many (chunk "ab")) "ababa"
  run "some" (some (single 'a')) "b"
  runRest "some" (some (single 'a')) "aab"
  runRest "count" (count 2 anySingle) "abc"
  run "count" (count 3 (single 'a')) "aab"
  runRest "count'" (count' 1 3 (single 'a')) "aaaaa"
  run "count'" (count' 2 3 (single 'a')) "ab"
  runRest "sepBy" (single 'a' `sepBy` single ',') "a,a,ab"
  runRest "sepBy" (single 'a' `sepBy` single ',') "b"
  run "sepBy" (single 'a' `sepBy` single ',') "a,b"
  run "sepBy1" (single 'a' `sepBy1` single ',') "b"
  runRest "sepEndBy" (single 'a' `sepEndBy` single ';') "a;a;b"
  runRest "sepEndBy1" (single 'a' `sepEndBy1` single ';') "a;a"
  runRest "endBy" (single 'a' `endBy` single ';') "a;a;b"
  runRest "manyTill" (manyTill anySingle (single '.')) "abc.def"
  run "manyTill" (manyTill (single 'a') (single '.')) "aab"
  runRest "someTill" (someTill anySingle (single '.')) ".a.b"
  runRest "skipMany" (skipMany (single ' ')) "   x"
  run "skipSome" (skipSome (single ' ')) "x"
  runRest "skipManyTill" (skipManyTill anySingle (single '!')) "abc!d"
  run "between" (between (single '(') (single ')') (many (single 'a'))) "(aa"

  -- Looking without reading.
  runRest "lookAhead" (lookAhead (chunk "ab")) "abc"
  run "lookAhead" (lookAhead (chunk "ab") *> single 'x') "abc"
  run "lookAhead fails" (lookAhead (single 'a' *> single 'b') <|> single 'a') "ac"
  runRest "notFollowedBy" (single 'a' <* notFollowedBy (single 'b')) "ac"
  run "notFollowedBy" (single 'a' <* notFollowedBy (single 'b')) "ab"
  run "notFollowedBy at end" (notFollowedBy eof) ""
  run "atEnd" ((,) <$> atEnd <*> (anySingle *> atEnd)) "a"

  -- Failing on purpose.
  run "fail" (single 'a' *> fail "no good" :: Parser ()) "ab"
  run "fail or" (fail "first" <|> fail "second" :: Parser ()) "ab"
  run "fail or token" (fail "first" <|> single 'x') "ab"
  run "customFailure" (single 'a' *> customFailure (Custom "bad" 7) :: Parser ()) "ab"
  run "two custom failures" (customFailure (Custom "one" 1) <|> customFailure (Custom "two" 2) :: Parser ()) ""
  run "unexpected" (unexpected (Label (NE.fromList "a surprise")) :: Parser ()) "ab"
  run "failure" (failure (Just EndOfInput) (Set.fromList [Tokens (NE.fromList "x"), Label (NE.fromList "y")]) :: Parser ()) "ab"
  run "failure with nothing" (failure Nothing Set.empty :: Parser ()) "ab"

  -- Errors as values.
  run "observing" (observing (single 'a')) "b"
  runRest "observing after input" (observing (single 'a' *> single 'b')) "acd"
  run "observing then" (observing (single 'a') *> single 'c') "b"
  runRest "withRecovery" (withRecovery (\_ -> '?' <$ anySingle) (single 'a')) "bc"
  runRest "withRecovery after input" (withRecovery (\e -> errorOffset e <$ takeWhileP Nothing (/= ';')) (0 <$ chunk "abc")) "abx;y"
  run "withRecovery fails too" (withRecovery (\_ -> single 'q') (single 'a')) "b"
  run "registerParseError" (many (single 'a' <|> (anySingle >>= \c -> registerFancyFailure (Set.singleton (ErrorFail ("not a: " ++ [c]))) >> pure c))) "abaca"
  run "registered and failed" (registerFailure Nothing (Set.singleton EndOfInput) *> anySingle *> single 'x') "ab"
  run "region" (region (setErrorOffset 0) (chunk "ab" *> single 'x')) "abc"
  run "region with registered" (region (\e -> FancyError (errorOffset e) (Set.singleton (ErrorCustom (Custom "in region" (errorOffset e))))) (anySingle *> registerFailure Nothing Set.empty *> anySingle *> single 'x')) "abc"

  -- The state.
  runRest "getOffset" ((,) <$> (chunk "ab" *> getOffset) <*> (anySingle *> getOffset)) "abcd"
  run "getInput" (anySingle *> getInput) "abc"
  run "setInput" (setInput "xyz" *> many anySingle) "abc"
  run "setOffset" (setOffset 10 *> single 'q') "abc"
  run "qualified field update" (MPC.updateParserState (\s -> s {MPC.stateOffset = MPC.stateOffset s + 5}) *> single 'q') "abc"
  run "getSourcePos" ((,,) <$> getSourcePos <*> (takeWhileP Nothing (/= 'x') *> getSourcePos) <*> (anySingle *> getSourcePos)) "ab\n\tcd\nefxg"
  run "match" (match (many (single 'a') *> single 'b')) "aabc"
  run "match" (match (pure ())) "abc"
  print (parseMaybe (many (single 'a') :: Parser String) "aaa", parseMaybe (many (single 'a') :: Parser String) "aab")
  print (fmap (stateOffset . fst) (Just (runParser' (chunk "ab" :: Parser String) (State "abc" 0 (PosState "abc" 0 (initialPos "name") defaultTabWidth "") []))))

  -- Characters.
  runRest "char" (C.char 'a') "ab"
  runRest "string" (C.string "ab") "abc"
  runRest "space" C.space " \t\n x"
  run "space1" C.space1 "x"
  runRest "hspace" C.hspace " \t\n x"
  runRest "eol" C.eol "\nx"
  runRest "eol" C.eol "\r\nx"
  run "eol" C.eol "\rx"
  runRest "newline, tab" ((,) <$> C.newline <*> C.tab) "\n\tx"
  run "digitChar" C.digitChar "x"
  run "letterChar" C.letterChar "1"
  run "upperChar" C.upperChar "a"
  run "lowerChar" C.lowerChar "A"
  run "alphaNumChar" C.alphaNumChar "-"
  run "spaceChar" C.spaceChar "x"
  run "hexDigitChar" (many C.hexDigitChar <* eof) "1aFg"
  run "octDigitChar" (many C.octDigitChar <* eof) "178"
  run "binDigitChar" (many C.binDigitChar <* eof) "102"
  run "printChar" C.printChar "\n"
  run "asciiChar" C.asciiChar "a"

  -- Lexer.
  runRest "decimal" (L.decimal :: Parser Int) "123abc"
  run "decimal" (L.decimal :: Parser Int) "abc"
  run "decimal" (L.decimal :: Parser Integer) "9007199254740993"
  runRest "hexadecimal" (L.hexadecimal :: Parser Int) "fFg"
  run "hexadecimal" (L.hexadecimal :: Parser Int) "g"
  runRest "octal" (L.octal :: Parser Int) "178"
  runRest "binary" (L.binary :: Parser Int) "1012"
  run "signed" (L.signed sc L.decimal :: Parser Int) "- 12"
  run "signed" (L.signed sc L.decimal :: Parser Int) "+12"
  run "signed" (L.signed (pure ()) L.decimal :: Parser Int) "- 12"
  run "charLiteral" (many L.charLiteral) "a\\n\\t\\\\\\'\\\"\\65\\x41\\o101\\SOH\\SO\\^A\\DELz"
  run "charLiteral" L.charLiteral "\\q"
  run "charLiteral" L.charLiteral ""
  runRest "skipLineComment" (L.skipLineComment "//") "// a comment\nnext"
  runRest "skipBlockComment" (L.skipBlockComment "/*" "*/") "/* a /* b */ c */"
  runRest "skipBlockCommentNested" (L.skipBlockCommentNested "/*" "*/") "/* a /* b */ c */ d"
  run "skipBlockComment" (L.skipBlockComment "/*" "*/") "/* never closed"
  runRest "symbol" (symbol "let") "let  {- c -} -- d\n  x"
  run "symbol" (symbol "let") "lex"

  -- The small language.
  run "program" program "1 + 2 * 3"
  run "program" program "  f(x, 2) * (y + 1); let a = 3 in a * a ; -- done\n"
  run "program" program "letter + lets"
  run "program" program "1 + "
  run "program" program "1 + * 2"
  run "program" program "f(1, 2"
  run "program" program "let x 3 in x"
  run "program" program "let x = 3 on x"
  run "program" program "(1 + 2))"
  run "program" program "1 {- open"
  run "program" program "a;\n  b +\n  {- note -} c d"
  run "program" program ""

  -- Text as the stream.
  runText "text" (many C.letterChar <* C.space <* eof) "abc  "
  runText "text" (C.string (T.pack "ab") *> C.char 'x') "abc"
  runText "text" (takeWhile1P (Just "digit") (`elem` "0123456789") <* eof) "12a"
  runText "text" ((,) <$> (L.decimal :: TextParser Int) <*> takeRest) "42 rest"
  runText "text" (C.eol *> getSourcePos) "\r\nx"
  runText "text" (match (C.string (T.pack "a") *> many C.digitChar)) "a12b"
  runText "text" (some (C.char 'é') *> C.char 'x') "ééy"

  -- An environment under the parser.
  runScoped "top top"
  runScoped "\\x.x top \\y.y x;;top"
  runScoped "\\x.x; x"
  runScoped "\\x.x y;"
  runScoped "\\x.x"
  runScoped "top \\X.;"
  runScoped ""

  -- A stream of tokens.
  runToks "tree" [TInt 1]
  runToks "tree" [TName "f", TOpen, TInt 1, TComma, TName "g", TOpen, TClose, TComma, TName "h", TClose]
  runToks "tree" [TName "f", TOpen, TInt 1, TComma, TClose]
  runToks "tree" [TName "f", TOpen, TInt 1, TInt 2]
  runToks "tree" [TComma]
  runToks "tree" []
  runToks "tree" [TInt 1, TInt 2]
