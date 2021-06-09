-- Prática 06 de Haskell
-- Nome: Álisson Braga Canabarro

-- 1) retorna o primeiro e último elemento de uma lista
ends :: [Int] -> [Int]
ends [] = []
ends [x] = [x]
ends list = (head list) : (last list) : []


-- 2) reescrita de 'deduzame' utilizando (x:xs)
deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame xs

-- 3) reescrita de 'deduzame2' utilizando (x:xs)
deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs 
  else deduzame2 xs


-- 4) produz uma lista com n tuplas, cada tupla com números de n a 1 e seus respectivos quadrados.
geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = [(0,0)]
geraTabela 1 = [(1,1)]
geraTabela x = (x,x^2) : geraTabela (x-1)


-- 5) verifica se um caracter está em uma sting usando recursão
contido :: Char -> String -> Bool
contido _ "" = False
contido char (x:xs) = if char == x
  then True || contido char xs
  else False || contido char xs


-- 6) pega uma lista de coordenadas e desloca x e y (de cada coord) em 2 unidades
translate :: [(Double,Double)] -> [(Double,Double)]
translate [] = []
translate (x:xs) = ((\(a,b) -> (a+2.0,b+2.0)) x) : translate xs


-- 7) retorna a quantidade de palavras da lista que possuem mais de 5 caracteres
countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = if length x > 5
  then 1 + countLongs xs
  else countLongs xs


-- 8) semelhante a anterior, mas nao retorna um valor e sim uma lsita com as palavras > 5 char
onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if length x > 5
  then x : onlyLongs xs
  else onlyLongs xs

