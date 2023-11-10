esPrimo :: Integer -> Bool
esPrimo n | n <= 1    = False
          | n == 2    = True
          | n `mod` 2 == 0 = False
          | otherwise = not (any (\x -> n `mod` x == 0) [3, 5..limite])
          where limite = floor (sqrt (fromIntegral n))

numerosEntreDosYelSiguienteValor :: Integer -> [Integer]
numerosEntreDosYelSiguienteValor x = filter esPrimo [2..x]

main :: IO ()
main = do
  putStrLn "Ingrese un número entero mayor que 2:"
  input <- getLine
  let numero = read input :: Integer
  let primos = numerosEntreDosYelSiguienteValor numero
  putStrLn $ "Números primos entre 2 y " ++ show numero ++ ": " ++ show primos
