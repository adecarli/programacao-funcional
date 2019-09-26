import Data.Char

-- Encoding functions
let2int :: Char -> Int
let2int char = ord char - 97

int2let :: Int -> Char
int2let num = chr (num + 97)

shift :: Int -> Char -> Char
shift n char = int2let $ mod (let2int char  + n) 26

encode :: Int -> String -> String
encode _ [] = []
encode n (x:xs) = encoded : encode n xs
    where encoded = if elem x ['a'..'z'] then (shift n x) else x

-- Decoding functions
percent :: Int -> Int -> Float
percent n1 n2 = (fromIntegral n1) / (fromIntegral n2) * 100

freqs :: String -> [Float]
freqs str = [ x | y <- ['a'..'z'], let z = charFrequency y str, let x = percent z $ length $ lowerChars str ]

lowerChars :: String -> String
lowerChars str = [x | x <- str, elem x ['a'..'z']]

charFrequency :: Char -> String -> Int
charFrequency c str = length [ x | x <- str, x == c ]

chisqr :: [Float] -> [Float] -> Float
chisqr xs ys = sum $ [ (x - y)^2 / y | (x,y) <- zip xs ys]

rotate :: Int -> [a] -> [a]
rotate 0 xs = xs
rotate n (x:xs) = rotate (n-1) (xs ++ [x])

crack :: String -> String
crack str = encode (-n) str
    where n = snd $ minimum $ zip freqTable [0..25]
          freqTable = [ chisqr (rotate n' table') table | n' <- [0..25]]
          table' = freqs str

-- Table for english alphabet
table :: [Float]
table = [ 8.167
        , 1.492
        , 2.782
        , 4.253
        , 12.702
        , 2.228
        , 2.015
        , 6.094
        , 6.966
        , 0.153
        , 0.772
        , 4.025
        , 2.406
        , 6.749
        , 7.507
        , 1.929
        , 0.095
        , 5.987
        , 6.327
        , 9.056
        , 2.758
        , 0.978
        , 2.360
        , 0.150
        , 1.974
        , 0.074 ]
