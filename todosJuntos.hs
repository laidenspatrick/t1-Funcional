
module Main where

-- 1. bin2dec
bin2dec :: [Int] -> Int
bin2dec st = foldl (\a x->2*a+x) 0 st

-- 2. dec2bin
dec2bin :: Int -> Int -> [Int]
dec2bin 0 0 = []
dec2bin nbits num
  | nbits > (2^num) = [-1]
  | nbits - (2 ^ (num-1)) < 0 = 0 : dec2bin nbits (num-1)
  | otherwise = 1 : dec2bin (nbits-(2 ^ (num-1))) (num-1)

-- 3. somarbin
somarbin :: [Int] -> [Int] -> Int -> [Int]
somarbin xs ys z = if and [((length xs) == z), ((length ys) == z)] then adicionarBinario (reverse xs) (reverse ys) 0 else error "Número de bits insuficiente"

-- 4. frac2bin
frac2bin :: Double -> ([Int], [Int])
frac2bin 0 = ([],[])
frac2bin v = if snd (separaint v) >= 1-0.5**16
            then (dec2bincompl (fst (separaint (v+1))) 16, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) 
            else (dec2bincompl (fst (separaint v)) 16, fracionario (snd (separaint v)) 16)

--metodos auxiliares

dec2bincompl :: Int -> Int -> [Int]
dec2bincompl v t
  | v < -(2^(t-1)) || v > 2^(t-1)-1 = [-1] 
  | v >= 0 = dec2bin v t
  | otherwise =
      let posBits = dec2bin (abs v) t
          inverted = map (1 -) posBits
      in soma1 inverted

soma1 :: [Int] -> [Int]
soma1 [] = []
soma1 b = dec2bin (somadec + 1) (length b)
            where somadec = bin2dec b


adicionarBinario :: [Int] -> [Int] -> Int -> [Int]
adicionarBinario[] [] 0 = []
adicionarBinario [] [] 1 = []
adicionarBinario (x:xs) (y:ys) z | (x + y + z == 3) = z : adicionarBinario xs ys 1
                          | (x + y + z == 2) = 0 : adicionarBinario xs ys 1
                          | x == y = 0 + z : adicionarBinario xs ys 0
                          | otherwise = 1 : adicionarBinario xs ys 0

separaint :: Double -> (Int, Double)
separaint 0.0 = (0, 0.0)
separaint v
    | v < 0 = let (intPart, fracPart) = separaint (-v)
              in (-intPart, fracPart)
    | otherwise = let intPart = floor v
                      fracPart = v - fromIntegral intPart
                  in (intPart, fracPart)
        

transformaremBinario :: Int -> [Int]
transformaremBinario 0 = [0]
transformaremBinario n = transformaremBinario (n `div` 2) ++ [n `mod` 2] 

fracionario :: Double -> Int -> [Int]
fracionario _ 0 = []
fracionario x n = let newX = x * 2
                      intPart = truncate newX
                      fracPart = fracionario (newX - fromIntegral intPart) (n - 1)
                  in intPart : fracPart

-- main para calcular
main :: IO ()
main = do
  putStrLn "calculando:"
  print $ bin2dec [0,1,1]
  print $ dec2bin 2 8
  print $ somarbin [1,0,1] [0,1,1] 3
  print $ frac2bin (-8.5)
  