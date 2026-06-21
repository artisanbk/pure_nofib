-- nofib imaginary/queens: count the number of solutions to the n-queens problem.
-- Faithful port of the LML formulation: nsoln nq = length (gen nq).

main :: IO ()
main = do
  arg1 <- read_arg1
  let nq = fromString arg1
  print ("nsoln " ++ toString nq ++ " = " ++ toString (nsoln nq))
  Ret ()

nsoln :: Integer -> Integer
nsoln nq =
  let safe x d l = case l of [] -> True
                             q:rest -> not (x == q) && not (x == q + d)
                                       && not (x == q - d) && safe x (d + 1) rest
      gen n = if n == 0 then [[]]
              else concatMap
                     (\b -> concatMap
                              (\q -> if safe q 1 b then [q : b] else [])
                              (enumFromTo 1 nq))
                     (gen (n - 1))
  in length (gen nq)


-- List helpers

b1 && b2 = case b1 of True -> b2
                      False -> False

not :: Bool -> Bool
not b = case b of True -> False
                  False -> True

length :: [a] -> Integer
length l = case l of [] -> 0
                     h:t -> 1 + length t

append :: [a] -> [a] -> [a]
append l1 l2 = case l1 of [] -> l2
                          h:t -> h : append t l2

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc l = case l of [] -> acc
                          h:t -> f h (foldr f acc t)

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr (\a -> append (f a)) []

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
