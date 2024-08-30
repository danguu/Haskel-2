-- Ejercicio 1:`toDigits` y `toDigitsRev`
toDigits :: Int -> [Int]
toDigits n = map (read . (:[])) (show n)

toDigitsRev :: Int -> [Int]
toDigitsRev n = reverse (toDigits n)

-- Ejercicio 2:`doubleEveryOther`
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther digits = zipWith (*) digits (cycle [1, 2])

-- Ejercicio 3:`sumDigits`
sumDigits :: [Int] -> Int
sumDigits digits = sum (map (\d -> if d > 9 then d - 9 else d) digits)

-- Ejercicio 4:`validate`
validate :: Int -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0

-- Para probar las funciones :)
main :: IO ()
main = do
  putStrLn "Ingrese un número de tarjeta de crédito:"
  input <- getLine
  let n = read input :: Int
  putStrLn ("¿Es válido? " ++ show (validate n))
