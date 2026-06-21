-- nofib imaginary/primes: the n-th prime via repeated trial-division filtering.
-- Faithful port of `prime n = map head (iterate the_filter [2..n*n]) !! n`.

main :: IO ()
main = do
  arg1 <- read_arg1
  let n = fromString arg1
  print ("prime " ++ toString n ++ " = " ++ toString (prime n))
  Ret ()

isdivs :: Integer -> Integer -> Bool
isdivs n x = not (x `mod` n == 0)

the_filter :: [Integer] -> [Integer]
the_filter l = case l of [] -> []
                         n:ns -> filter (isdivs n) ns

prime :: Integer -> Integer
prime n = idx n (map head (iterate the_filter (enumFromTo 2 (n * n))))


-- List helpers

not :: Bool -> Bool
not b = case b of True -> False
                  False -> True

head :: [Integer] -> Integer
head l = case l of [] -> 0
                   h:t -> h

idx :: Integer -> [Integer] -> Integer
idx n l = case l of [] -> 0
                    h:t -> if n == 0 then h else idx (n - 1) t

filter :: (a -> Bool) -> [a] -> [a]
filter f l = case l of [] -> []
                       h:t -> if f h then h : filter f t else filter f t

map :: (a -> b) -> [a] -> [b]
map f l = case l of [] -> []
                    h:t -> f h : map f t

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

enumFromTo :: Integer -> Integer -> [Integer]
enumFromTo a b = if b < a then [] else a : enumFromTo (a + 1) b


-- I/O helpers

f $ x = f x

s1 ++ s2 = #(__Concat) s1 s2

reverse :: [a] -> [a]
reverse l =
  let revA a l = case l of [] -> a
                           h:t -> revA (h:a) t
  in revA [] l

fromString :: String -> Integer
fromString s =
  let fromStringI i limit acc s =
        if limit == i then acc
        else if limit < i then acc
        else fromStringI (i + 1) limit (acc * 10 + (#(__Elem) s i - 48)) s
  in fromStringI 0 (#(__Len) s) 0 s

toString :: Integer -> String
toString i =
  let toString0 i =
        if i == 0 then []
        else (i `mod` 10 + 48) : toString0 (i `div` 10)
  in if i < 0 then "-" ++ (implode $ reverse $ toString0 (0 - i))
     else if i == 0 then "0"
     else implode $ reverse $ toString0 i

implode l =
  case l of
    [] -> ""
    h:t -> #(__Implode) h ++ implode t

read_arg1 = Act (#(cline_arg) " ")

print s = Act (#(stdout) (s ++ "\n"))
