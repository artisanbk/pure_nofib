-- nofib spectral/sorting: a battery of sorting algorithms (Meira thesis et al.).
-- The original sorts the lines of a file with eight different sorts composed
-- together. PureLang has no file I/O, so the single argument n selects a
-- deterministic scrambled list of n integers; we run all eight sorts on it,
-- check they agree, and print a checksum.

main :: IO ()
main = do
  arg1 <- read_arg1
  let n = fromString arg1
      input = genList n
      ref = quickSort input
      ok = andAll (map (\srt -> listEq (srt input) ref) sorts)
  print ("sorted " ++ toString n ++ " elements: "
         ++ (if ok then "OK" else "FAIL") ++ ", checksum " ++ toString (sumL ref))
  Ret ()

sorts :: [[Integer] -> [Integer]]
sorts = [quickSort, quickSort2, quickerSort, insertSort,
         treeSort, treeSort2, heapSort, mergeSort]

-- A simple linear-congruential scramble for reproducible unsorted input.
genList :: Integer -> [Integer]
genList n = map (\i -> (i * 1103515245 + 12345) `mod` 1000003) (enumFromTo 0 (n - 1))


-- quickSort
quickSort :: [Integer] -> [Integer]
quickSort l =
  case l of
    [] -> []
    x:xs -> append (quickSort (filter (\y -> not (x < y)) xs))
                   (x : quickSort (filter (\y -> x < y) xs))

-- quickSort2, via partition
quickSort2 :: [Integer] -> [Integer]
quickSort2 l =
  case l of
    [] -> []
    x:xs -> case partition (\y -> not (x < y)) xs of
              (lo, hi) -> append (quickSort2 lo) (x : quickSort2 hi)

partition :: (Integer -> Bool) -> [Integer] -> ([Integer], [Integer])
partition p l =
  case l of
    [] -> ([], [])
    h:t -> case partition p t of
             (a, b) -> if p h then (h : a, b) else (a, h : b)

-- quickerSort (tail-recursive split)
quickerSort :: [Integer] -> [Integer]
quickerSort l =
  case l of
    [] -> []
    x:xs -> case xs of
              [] -> [x]
              y:ys -> qsplit x [] [] xs

qsplit :: Integer -> [Integer] -> [Integer] -> [Integer] -> [Integer]
qsplit x lo hi l =
  case l of
    [] -> append (quickerSort lo) (x : quickerSort hi)
    y:ys -> if not (x < y) then qsplit x (y : lo) hi ys
            else qsplit x lo (y : hi) ys

-- insertSort
insertSort :: [Integer] -> [Integer]
insertSort l = case l of [] -> []
                         x:xs -> trins [] [x] xs

trins :: [Integer] -> [Integer] -> [Integer] -> [Integer]
trins rev mid src =
  case src of
    [] -> append (reverse rev) mid
    y:ys -> case mid of
              [] -> trins [] (append (reverse rev) [y]) ys
              x:xs -> if x < y then trins (x : rev) xs (y : ys)
                      else trins [] (append (reverse rev) (y : x : xs)) ys

-- treeSort (binary search tree)
data Tree = Tip | Branch Integer Tree Tree

treeSort :: [Integer] -> [Integer]
treeSort l = readTree (foldr toTree Tip l)

toTree :: Integer -> Tree -> Tree
toTree x t = case t of
               Tip -> Branch x Tip Tip
               Branch y l r -> if not (y < x) then Branch y (toTree x l) r
                               else Branch y l (toTree x r)

readTree :: Tree -> [Integer]
readTree t = case t of Tip -> []
                       Branch x l r -> append (readTree l) (x : readTree r)

-- treeSort2 (bushier trees)
data Tree2 = Tip2 | Twig2 Integer | Branch2 Integer Tree2 Tree2

treeSort2 :: [Integer] -> [Integer]
treeSort2 l = readTree2 (foldr toTree2 Tip2 l)

toTree2 :: Integer -> Tree2 -> Tree2
toTree2 x t = case t of
                Tip2 -> Twig2 x
                Twig2 y -> if not (y < x) then Branch2 y (Twig2 x) Tip2
                           else Branch2 y Tip2 (Twig2 x)
                Branch2 y l r -> if not (y < x) then Branch2 y (toTree2 x l) r
                                 else Branch2 y l (toTree2 x r)

readTree2 :: Tree2 -> [Integer]
readTree2 t = case t of Tip2 -> []
                        Twig2 x -> [x]
                        Branch2 x l r -> append (readTree2 l) (x : readTree2 r)

-- heapSort (reuses Tree)
heapSort :: [Integer] -> [Integer]
heapSort l = clear (heap 0 l)

heap :: Integer -> [Integer] -> Tree
heap k l = case l of [] -> Tip
                     x:xs -> toHeap k x (heap (k + 1) xs)

toHeap :: Integer -> Integer -> Tree -> Tree
toHeap k x t =
  case t of
    Tip -> Branch x Tip Tip
    Branch y l r ->
      if andB (not (y < x)) (odd k) then Branch x (toHeap (div2 k) y l) r
      else if not (y < x) then Branch x l (toHeap (div2 k) y r)
      else if odd k then Branch y (toHeap (div2 k) x l) r
      else Branch y l (toHeap (div2 k) x r)

clear :: Tree -> [Integer]
clear t = case t of Tip -> []
                    Branch x l r -> x : clear (mix l r)

mix :: Tree -> Tree -> Tree
mix a b =
  case a of
    Tip -> b
    Branch x l1 r1 ->
      case b of
        Tip -> a
        Branch y l2 r2 -> if not (y < x) then Branch x (mix l1 r1) b
                          else Branch y a (mix l2 r2)

-- mergeSort
mergeSort :: [Integer] -> [Integer]
mergeSort l = merge_lists (runsplit [] l)

runsplit :: [Integer] -> [Integer] -> [[Integer]]
runsplit run src =
  case src of
    [] -> case run of [] -> []
                      h:t -> [run]
    x:xs -> case run of
              [] -> runsplit [x] xs
              r:rs -> case rs of
                        [] -> if r < x then runsplit (r : x : []) xs
                              else runsplit (x : run) xs
                        h:t -> if not (r < x) then runsplit (x : run) xs
                               else append [run] (runsplit (x : []) xs)

merge_lists :: [[Integer]] -> [Integer]
merge_lists ll = case ll of [] -> []
                            h:t -> merge h (merge_lists t)

merge :: [Integer] -> [Integer] -> [Integer]
merge a b =
  case a of
    [] -> b
    x:xs -> case b of
              [] -> a
              y:ys -> if x == y then x : y : merge xs ys
                      else if x < y then x : merge xs b
                      else y : merge a ys


-- Helpers

andB :: Bool -> Bool -> Bool
andB a b = case a of True -> b
                     False -> False

not :: Bool -> Bool
not b = case b of True -> False
                  False -> True

odd :: Integer -> Bool
odd k = k `mod` 2 == 1

div2 :: Integer -> Integer
div2 k = k `div` 2

sumL :: [Integer] -> Integer
sumL l = case l of [] -> 0
                   h:t -> h + sumL t

listEq :: [Integer] -> [Integer] -> Bool
listEq a b =
  case a of
    [] -> case b of [] -> True
                    h:t -> False
    x:xs -> case b of [] -> False
                      y:ys -> if x == y then listEq xs ys else False

andAll :: [Bool] -> Bool
andAll l = case l of [] -> True
                     h:t -> if h then andAll t else False

filter :: (a -> Bool) -> [a] -> [a]
filter f l = case l of [] -> []
                       h:t -> if f h then h : filter f t else filter f t

map :: (a -> b) -> [a] -> [b]
map f l = case l of [] -> []
                    h:t -> f h : map f t

append :: [a] -> [a] -> [a]
append l1 l2 = case l1 of [] -> l2
                          h:t -> h : append t l2

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc l = case l of [] -> acc
                          h:t -> f h (foldr f acc t)

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
