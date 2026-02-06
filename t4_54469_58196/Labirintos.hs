module Labirintos (
    EstadoJogo(..),
    inicializa,
    jogador,
    chaves,
    terminado,
    mover,
    introduzChar,
    posicaoChar
) where


import Data.List ( sort )
import Data.Char ( toLower )

data EstadoJogo = EstadoJogo{
                mapa :: [String],
                posicaoJogador :: (Int,Int),
                chavesAdequiridas :: String,
                estaTerminado :: Bool
    }

{-
Inicializa um EstadoJogo dado um labrinto
-}
inicializa :: [String] -> String -> String -> EstadoJogo
inicializa lab pos keys= EstadoJogo lab (convert pos) keys False

{-
Converte uma String com a posição de um jogaor num par (Int,Int)
-}
convert :: String -> (Int,Int)
convert x = (read $ head (split ',' (tail (init x))), read $ last (split ',' (tail (init x))))

split :: Char -> String -> [String]
split _ "" = []
split delimiter str = 
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
     in start : split delimiter remain

-- >>> split ',' "ola, adeus"

{-
substitui 'Char' na posição dada no labirinto
inicializar acc com []
-}
introduzChar :: [String] -> Char -> Int -> (Int, Int) -> [String] ->[String]
introduzChar [] _ _ _ _ = []
introduzChar (line:lab) c size (a,b) acc = if size - length (line:lab) == a 
  then acc ++ [replaceCharAtIndex b c line] ++ introduzChar lab c size (a,b) acc
  else acc ++ [line] ++ introduzChar lab c size (a,b) acc


{-
substitui um caracter numa Strings por outro num indice dado
-}
replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index replacement str = strHead ++ [replacement] ++ drop 1 strAfter
  where (strHead, strAfter) = splitAt index str


{-
Devolve a posição de um dado caracter numa lista de Strings
-}
posicaoChar :: [String] -> Char -> Int -> (Int, Int) -- funciona
posicaoChar [] _ _ = (0,0)
posicaoChar (line:lab) c size = if c `elem` line then head (zip [size - length (line:lab)] [indiceCaracter line c])  else posicaoChar lab c size

{-
Devolve o indice de um dado caracter numa String
-}
indiceCaracter :: String -> Char -> Int --funciona
indiceCaracter word char = sum [ y | (x, y) <- zip word [0..], x == char ]

{-
Devolve a posição de jogador num EstadoJogo
-}
jogador :: EstadoJogo -> (Int,Int)
jogador (EstadoJogo _ posicao _ _) = posicao


{-
Devolve a String com todas as chaves apanhadas
-}
chaves :: EstadoJogo -> String
chaves (EstadoJogo _ _ chaveString _) = chaveString

{-
Devolve um booleano que representa o final do jogo
-}
terminado :: EstadoJogo -> Bool
terminado (EstadoJogo _ _ _ terminou) = terminou

{-
Redefinição de Show para o Data EstadoJogo
-}
instance Show EstadoJogo where
  show (EstadoJogo lab (a,b) key _) = unlines (introduzChar lab 'P' (length lab) (a,b) []) ++ "chaves: " ++ key

{-
Devolve um EstadoJogo efetuando uma jogada sobre outro
-}
mover :: EstadoJogo -> String -> EstadoJogo
mover (EstadoJogo lab (line, col) key over) [] = EstadoJogo lab (line, col) key over
mover (EstadoJogo lab (line, col) key over) (dir:rest)
  |dir == 'u' && 
  getCharLab lab (length lab) (line - 1, col) /= '*' && 
  (getCharLab lab (length lab) (line-1, col) `notElem` ['A','B','C']) `xor`
  (toLower (getCharLab lab (length lab) (line -1, col)) `elem` key)
  = process (EstadoJogo lab (line - 1, col) key over) rest

  |dir == 'l' && 
  getCharLab lab (length lab) (line , col - 1) /= '*' && 
  (getCharLab lab (length lab) (line, col-1) `notElem` ['A','B','C']) `xor`
  (toLower (getCharLab lab (length lab) (line, col-1)) `elem` key)
  = process (EstadoJogo lab (line, col - 1) key over) rest

  |dir == 'r' &&
  getCharLab lab (length lab) (line, col + 1) /= '*' && 
  (getCharLab lab (length lab) (line, col+ 1) `notElem` ['A','B','C']) `xor`
  (toLower (getCharLab lab (length lab) (line , col+ 1)) `elem` key)
  = process (EstadoJogo lab (line, col + 1) key over) rest

  |dir == 'd' && 
  getCharLab lab (length lab) (line + 1, col) /= '*' && 
  (getCharLab lab (length lab) (line+1, col) `notElem` ['A','B','C']) `xor`
  (toLower (getCharLab lab (length lab) (line +1, col)) `elem` key)
  = process (EstadoJogo lab (line + 1, col) key over) rest

  |otherwise = mover (EstadoJogo lab (line, col) key over) rest

{-
Verifica a próxima casa e processa a jogada
-}
process :: EstadoJogo -> String -> EstadoJogo
process (EstadoJogo lab (line, col) key over) rest
  |getCharLab lab (length lab) (line, col) == 'F' = mover (EstadoJogo lab (line, col) key True) rest
  |getCharLab lab (length lab) (line, col) == '@' = movePortal (EstadoJogo lab (line, col) key over) rest
  |getCharLab lab (length lab) (line, col) `elem` ['a','b','c'] = moveChave (EstadoJogo lab (line, col) key over) rest
  |getCharLab lab (length lab) (line, col) `elem` ['A','B','C'] = mover (EstadoJogo (introduzChar lab ' ' (length lab) (line,col) []) (line, col) key over) rest
  |otherwise = mover (EstadoJogo lab (line, col) key over) rest

{-
Coloca o jogador no outro portal
-}
movePortal :: EstadoJogo -> String -> EstadoJogo
movePortal (EstadoJogo lab (line, col) key over) = mover (EstadoJogo lab a key over)
    where mapaAux = introduzChar lab ' ' (length lab) (line,col) []
          a = posicaoChar mapaAux '@' (length lab)

{-
Apanha a chave adicionando ao inventário do Jogador 
-}
moveChave :: EstadoJogo -> String -> EstadoJogo
moveChave (EstadoJogo lab (line, col) key over) = mover (EstadoJogo (introduzChar lab ' ' (length lab) (line,col) []) (line,col) a over) 
  where a = sort(getCharLab lab (length lab) (line, col) : key)

{-
Dado um labirinto, o seu tamanho e uma posição devolve o caracter desta
-}
getCharLab :: [String] -> Int -> (Int, Int) -> Char
getCharLab [] _ _ = '-'
getCharLab (line:lab) size (a,b) = if size - length (line:lab) == a then line !! b else getCharLab lab size (a,b)

{-
Operação booleana xor
-}
xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

