import           Control.Monad
import           Control.Monad.Except
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data JsonVal = Key String JsonVal
             | List [JsonVal]
             | Number Integer
             | String String
             | Bool Bool

showJson :: JsonVal -> String
showJson (String s) = s
showJson (Number i) = show i
showJson (Bool b)   = show b
showJson (List l)   = "[" ++ (unwords $ map showJson l) ++ "]"
showJson (Key s j)  = "{" ++ s ++ ": " ++ showJson j ++ "}"

instance Show JsonVal where
    show = showJson

data JsonError = Parser ParseError
               | InvalidValue String JsonVal

instance Show JsonError where
    show = showError

showError :: JsonError -> String
showError v@(Parser p)       = show v
showError (InvalidValue s j) = s ++ ": " ++ (show j)

---- Parsing ----
----

spaces :: Parser ()
spaces = skipMany1 space

spacesNewline :: Parser ()
spacesNewline = skipMany (space <|> newline)

parseBool :: Parser JsonVal
parseBool = do
    x <- try (string "true") <|> string "false"
    case x of
        "true"  -> return $ Bool True
        "false" -> return $ Bool False

parseNumber :: Parser JsonVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser JsonVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseKeyVal :: Parser JsonVal
parseKeyVal = do
    spacesNewline
    (String k) <- parseString
    v <- skipMany spaces >> char ':' >> skipMany spaces >> parseJson
    spacesNewline
    return $ Key k v

parseList :: Parser JsonVal
parseList = do
    char '['
    x <- sepBy parseJson (skipMany spaces >> char ',' >> skipMany spaces)
    char ']'
    return $ List x

parseJson :: Parser JsonVal
parseJson = parseString
        <|> parseBool
        <|> parseNumber
        <|> parseList
        <|> do char '{'
               x <- sepBy parseKeyVal (char ',')
               char '}'
               return $ List x

readExpr :: String -> Either ParseError JsonVal
readExpr s = case parse parseJson "JSON" s of
    Left err -> throwError err
    Right v  -> return v

---- Parsing ----
----

main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> do
                v <- liftM readExpr (readFile $ head args)
                case v of
                    Left err -> putStrLn $ "Error: " ++ (show err)
                    Right v  -> putStrLn $ show v
        _ -> putStrLn "./hjq <file.json>"
