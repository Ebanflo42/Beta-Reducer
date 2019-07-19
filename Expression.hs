module Expression where

import Data.List
import Data.Maybe

data Expression = Variable String
                | Abstraction String Expression
                | Application Expression Expression

instance Show Expression where
  show (Variable str)            = str
  show (Abstraction str expr)    = '\\':(str ++ '.':(show expr))
  show (Application expr1 expr2) = '(':((show expr1) ++ ") (" ++ (show expr2) ++ ")")

--find the index of the parantheses that closes the first open parantheses
--first argument is the number of open parantheses, and should be initialized to 0
findClosingParens :: Int -> String -> Int
findClosingParens 1 (')':_) = 0
findClosingParens n (')':s) = 1 + findClosingParens (n - 1) s
findClosingParens n ('(':s) = 1 + findClosingParens (n + 1) s
findClosingParens n (c:s)   = 1 + findClosingParens n s

{--}
parse :: String -> Expression
parse string =
  case head string of
    '\\' ->
      let dotIndex =
            fromMaybe (error "Parse error. Abstraction without \'.\'.") $ elemIndex '.' string
          str1     = tail $ take dotIndex string
          str2     = drop (dotIndex + 1) string
      in Abstraction str1 (parse str2)
    '('  ->
      let closIndex = findClosingParens 0 string
          str1      = tail $ take closIndex string
          str2      = init $ drop (closIndex + 3) string
      in Application (parse str1) (parse str2)
    _    -> Variable string
--}

replaceInstances :: (String, Expression) -> Expression -> Expression
replaceInstances (str, e) (Variable v)   = if v == str then e else Variable v
replaceInstances se (Abstraction v expr) = Abstraction v $ replaceInstances se expr
replaceInstances se (Application e1 e2)  =
  Application (replaceInstances se e1) (replaceInstances se e2)

betaReduce :: Expression -> Expression
betaReduce expr =
  case expr of
    Variable x              -> Variable x
    Abstraction var expr2   -> Abstraction var $ betaReduce expr2
    Application expr1 expr2 ->
      case betaReduce expr1 of
        Variable x        -> Application (Variable x) $ betaReduce expr2
        Abstraction v e   -> replaceInstances (v, expr2) e
        Application e1 e2 -> Application (Application e1 e2) $ betaReduce expr2
