import System.IO

data Token = 
    Number Float
    | Operator Char
    | End

tokenToString :: Token -> String
tokenToString (Number n) = show n
tokenToString (Operator op) = show op
tokenToString End = "End"

tokenize :: String -> [String]
tokenize = words

parseToken :: String -> Token
parseToken "+" = Operator '+'
parseToken "-" = Operator '-'
parseToken "*" = Operator '*'
parseToken "/" = Operator '/'
parseToken "^" = Operator '^'
parseToken number = Number (read number)

parseTokens :: [String] -> [Token]
parseTokens = map parseToken

applyOperation :: Float -> Char -> Float -> Float
applyOperation x '+' y = x + y
applyOperation x '-' y = x - y
applyOperation x '*' y = x * y
applyOperation x '/' y = if y == 0 then error "Divide by zero" else x / y
applyOperation x '^' y = x ** y
applyOperation _ op _ = error ("Unsupported operator: " ++ [op])

evaluate :: [Token] -> Float
evaluate [Number n] = n
evaluate (Number x : Operator op : Number y : ts) =
    evaluate (Number (applyOperation x op y) : ts)
evaluate _ = error "Invalid expression"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Enter your expression: "
    expression <- getLine

    let tokenizedExpression = tokenize expression
    let parsedTokens = parseTokens tokenizedExpression
    print $ evaluate parsedTokens
