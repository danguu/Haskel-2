hanoi :: Int -> String -> String -> String -> [(String, String)]
hanoi 0 _ _ _ = []
hanoi n from to temp =
  hanoi (n-1) from temp to ++
  [(from, to)] ++
  hanoi (n-1) temp to from

hanoi4 :: Int -> String -> String -> String -> String -> [(String, String)]
hanoi4 0 _ _ _ _ = []
hanoi4 n from to temp1 temp2 =
  hanoi4 (n-1) from temp1 to temp2 ++
  [(from, to)] ++
  hanoi4 (n-1) temp1 to from temp2

main :: IO ()
main = do
  putStrLn "Torres de Hanoi"
  putStrLn "----------------"

  putStrLn "Ejercicio 5: 3 discos, 3 estacas"
  let moves = hanoi 3 "A" "C" "B"
  putStrLn $ "Movimientos: " ++ show moves

  putStrLn ""
  putStrLn "Ejercicio 6: 3 discos, 4 estacas"
  let moves4 = hanoi4 3 "A" "D" "B" "C"
  putStrLn $ "Movimientos: " ++ show moves4
