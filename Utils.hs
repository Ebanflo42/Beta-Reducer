module Utils where

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip s = case s of
                  []        -> []
                  ('\r':xs) -> lstrip xs
                  ('\n':xs) -> lstrip xs
                  ('\t':xs) -> lstrip xs
                  (' ':xs)  -> lstrip xs
                  xs        -> xs

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
