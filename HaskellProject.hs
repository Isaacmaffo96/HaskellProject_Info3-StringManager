removeWhiteSpaces :: String -> String
removeWhiteSpaces xs = filter(\x -> x /= ' ') xs

removeSymbols :: String -> String
removeSymbols xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") ]

isPalindrome :: String -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)
-- $ as "use everything after the $ as a single argument for the function before the $

isPalindromeToString :: Bool -> String
isPalindromeToString x | x == True  = "e' palindroma"
                       | otherwise  = "non e' palindroma"

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = group_loop [x] x xs
  where
  group_loop acc c [] = [acc]
  group_loop acc c (y:ys)  
   | y == c    = group_loop (acc ++ [y]) c ys
   | otherwise = acc : group_loop [y] y ys

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

func :: String ->  [(Char,Int)]
func xs = map (\a -> (head a, length a)) $ group $ quicksort xs
-- prendo la striga, la dispongo in ordine alfabetico tramitte il quicksort
-- tramite group divido la stringa in liste di caratteri uguali adiacenti
-- creo la tupla composta dal primo carattere della lista e dalla lunghezza di quella lista
-- tramite map applico la funzione a tutti gli elementi della lista

main = do

      let ys = ("avida di vita desiai ogni amore vero ma ingoiai sedativi da diva.")
      print(ys)
      putStrLn (isPalindromeToString (isPalindrome (removeWhiteSpaces (removeSymbols ys))))
      putStrLn ("e' composta da:")
      putStrLn (show [func (removeWhiteSpaces (removeSymbols ys))])

      let zs = ("amorale, d'ogni vin gode l'aroma.")
      print(zs)
      putStrLn (isPalindromeToString (isPalindrome (removeWhiteSpaces (removeSymbols zs))))
      putStrLn ("e' composta da:")
      putStrLn (show [func (removeWhiteSpaces (removeSymbols zs))])

      putStrLn ("inserire una parola o una frase:")
      xs <- getLine
      putStrLn (isPalindromeToString (isPalindrome (removeWhiteSpaces (removeSymbols xs))))
      putStrLn ("e' composta da:")
      putStrLn (show [func (removeWhiteSpaces (removeSymbols xs))])