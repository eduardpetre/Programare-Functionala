import Data.Char

countVocale :: [Char] -> Int
countVocale[] = 0
countVocale (h:t) = 
    if elem h "aeiouAEIOU"
    then 1 + countVocale t
    else countVocale t

nrVocale :: [String] -> Int 
nrVocale[] = 0
nrVocale(h:t) =
    if reverse h == h
    then countVocale h + nrVocale t
    else nrVocale t



f :: Int -> [Int] -> [Int]
f _ [] = []
f x(h:ls) = 
    if even h
    then h:x:f x ls
    else h:f x ls



divizori :: Int -> [Int]
divizori x = [y | y <- [1..x], mod x y == 0]



listaDiv :: [Int] -> [[Int]]
listaDiv ls = [divizori a | a <- ls]



inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y ls = [ a | a<- ls, a >= x, a <= y]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (h:t) = 
    if h>=a && h<=b
    then h:inIntervalRec a b t
    else inIntervalRec a b t



pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp ls = [ p | (p,z) <- zip[0..] ls, odd z]



pozitiiImpare :: [Int] -> Int -> [Int]
pozitiiImpare [] _ = []
pozitiiImpare (h:t) poz =
    if odd h
    then poz : pozitiiImpare t (poz + 1)
    else pozitiiImpare t (poz+1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec ls = pozitiiImpare ls 0 



multDigitsRec :: String -> Int
multDigitsRec "" = 0
multDigitsRec (h:t) = 
    if isDigit h 
    then digitToInt h * multDigitsRec t
    else multDigitsRec t

multDigitsComp :: String -> Int
multDigitsComp ls = product [digitToInt x | x <- ls, isDigit x]