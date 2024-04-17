import System.IO

data Token = 
    Number Float
    | Add (Float -> Float -> Float)
    | Sub (Float -> Float -> Float)
    | Mul (Float -> Float -> Float)
    | Div (Float -> Float -> Float)
    | Pow (Float -> Float -> Float)
    | End Float

tokenToString :: Token -> String
tokenToString (Number n) = show n
tokenToString (Add _) = "+"
tokenToString (Sub _) = "-"
tokenToString (Mul _) = "*"
tokenToString (Div _) = "/"
tokenToString (Pow _) = "^"
tokenToString (End n) = show n

tokenize :: String -> [String]
tokenize = words

parseToken :: String -> Token
parseToken "+" = Add (+)
parseToken "-" = Sub (-)
parseToken "*" = Mul (*)
parseToken "/" = Div (/)
parseToken "^" = Pow (**)
parseToken "\0" = End 0
parseToken number = Number (read number)

parseTokens :: [String] -> [Token]
parseTokens = map parseToken

applyOperation :: [Token] -> Float
applyOperation (Number x:t2:Number y:ts) = case t2 of
    Add f -> f x y
    Sub f -> f x y
    Mul f -> f x y
    Div f -> f x y
    Pow f -> f x y
    _ -> error "Invalid operation"

evaluateLine :: [Token] -> Float
evaluateLine (t1:t2:t3:ts) = evaluateLine (Number (applyOperation (t1:t2:t3:ts)) : ts)
evaluateLine (t1:t2:ts) = applyOperation (t1:t2:ts)
evaluateLine [End n] = n
evaluateLine [Number n1] = n1

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Enter your expression: "
    expression <- getLine

    let tokenizedExpression = tokenize expression
    let parsedTokens = parseTokens tokenizedExpression
    print (evaluateLine parsedTokens)

