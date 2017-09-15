import Data.List
import Data.List.Utils

unwrap :: Maybe Int -> Int
unwrap Nothing  = error "No dot found in abstraction"
unwrap (Just n) = n

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

findClosingParens :: Int -> String -> Int
findClosingParens _ []        = -1
findClosingParens 0 (')':str) = -1
findClosingParens 1 (')':str) = 1
findClosingParens n ('(':str) = 1 + findClosingParens (n + 1) str
findClosingParens n (')':str) = 1 + findClosingParens (n - 1) str
findClosingParens n (c:str)   = 1 + findClosingParens n str

{-
extract :: String -> String
extract str = case elemIndex '(' str of
    Nothing -> str
    Just n  -> let closing = findClosingParens 0 (drop n str) in
        drop (n + 1) (take (closing - 1) str)
        -}

extract :: Int -> String -> String
extract closing str = case elemIndex '(' of
    Nothing -> str
    Just n  -> drop (n + 1) (take (closing -1) str)

betaReduce :: String -> String
betaReduce str = let index = findClosingParens 0 str - 1; noParens = index == -2
                     expr1 = if noParens then str else take (index - 1) (tail str) in
                     if not noParens then 
                         if safeHead expr1 == Just '\\' then
                             let dotIndex = unwrap (elemIndex '.' expr1)
                                 var      = take (dotIndex - 1) (tail expr1)
                                 expr2    = drop (index + 2) str
                                 output   = drop (dotIndex + 1) expr1 in
                                 replace var expr2 output
                             else let brFst = betaReduce expr1 in
                                 if brFst /= expr1 then replace expr1 brFst str else expr1
                         else str

loop str = let br = betaReduce str in
    if br == str then putStrLn str >> main else putStrLn br >> loop br

main :: IO ()
main = getLine >>= loop