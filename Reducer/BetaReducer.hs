import Data.List
import Data.List.Utils
import Control.Monad

--Abstract takes in an Expression and Expression instead of Variable and Expression
--because ghc won't allow reuse of the type constructor 'Variable'
data Expression = Variable String
                | Abstract Expression Expression
                | Apply Expression Expression
                deriving (Eq)

instance Show Expression where
    show (Variable var)                 = var
    show (Abstract (Variable var) expr) = ('λ':var) ++ ('.':(show expr))
    show (Apply expr1 expr2)            = ('(':(show expr1)) ++ ") (" ++ (show expr2) ++ ")"
    show expr                           = error "Cast error"

--The idea is to make a function that seeks out a variable and applies a function to it
--that takes expressions to other expressions
magic :: (Expression -> Expression) -> Expression -> Expression
magic f (Variable var)                 = f (Variable var)
magic f (Abstract (Variable var) expr) = Abstract (Variable var) (f expr)
magic f (Apply expr1 expr2)            = Apply (f expr1) (f expr2)

--Then to beta reduce you pass that function a function that takes in an expression
--and gives the argument expression if the expression is equal to the parameter for
--the function.
betaReduce :: Expression -> Expression
betaReduce (Apply (Abstract (Variable var) expr1) expr2) = let f = \expr -> if expr == Variable var then expr2 else expr in
    magic f (betaReduce expr1)
betaReduce (Apply expr1 expr2)                           = let beta1 = betaReduce expr1; beta2 = betaReduce expr2 in
    if beta1 == expr1 then Apply beta1 beta2 else betaReduce (Apply beta1 beta2)
betaReduce (Abstract (Variable var) expr)                = Abstract (Variable var) (betaReduce expr)
betaReduce expr                                          = expr

--Given the number of opened parantheses and a string
--this finds the index of the parantheses that closes all parantheses
findNextParens :: Int -> String -> Int
findNextParens _ []        = error "Couldn't find next parantheses"
findNextParens 0 (')':str) = error "Escaped parantheses"
findNextParens 1 (')':str) = 1
findNextParens n ('(':str) = 1 + findNextParens (n + 1) str
findNextParens n (')':str) = 1 + findNextParens (n - 1) str
findNextParens n (c:str)   = 1 + findNextParens n str

--finds the outermost set of parantheses in a string and removes them
parse :: String -> String
parse str = case elemIndex '(' str of
    Nothing -> str
    Just n  -> let nextIndex = findNextParens 0 str - 1 in
        drop (n + 1) (take nextIndex str)

findInnerMostParens :: String -> String
findInnerMostParens str = let parsed = parse str in
    if str == parsed then str else findInnerMostParens parsed

--processes an Abstraction, Application, or makes the input a Variable
processInput :: String -> Expression
processInput input = case head input of
    '\\' -> processAbstraction input
    '('  -> processApplication input
    any  -> Variable input

processAbstraction :: String -> Expression
processAbstraction str = case elemIndex '.' str of
    Nothing -> error "I thought your input was an abstraction but I couldn't find the period"
    Just n  -> Abstract (Variable (tail (take n str))) (processInput (drop (n + 1) str))

processApplication :: String -> Expression
processApplication str = let str1 = parse str; len = length str1; str2 = parse (drop (len + 3) str) in
    Apply (processInput str1) (processInput str2)

main = forever $ do
    input <- getLine
    let output = show (betaReduce (processInput input)) --show (betaReduce (processInput input))
    putStrLn output