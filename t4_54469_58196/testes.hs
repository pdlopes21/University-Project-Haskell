module Testes(testes) where

import Test.QuickCheck
    ( Arbitrary(arbitrary), Gen, quickCheck, choose ) 
import Labirintos
    ( EstadoJogo(EstadoJogo, posicaoJogador, chavesAdequiridas, mapa),
      introduzChar,
      posicaoChar,
      chaves,
      mover ) 
import Functions () 

instance Arbitrary EstadoJogo where
    arbitrary :: Gen EstadoJogo
    arbitrary = do 
        size <- gerSize
        startFinish <- gerStartAndFinish size
        portais <- gerPortais size startFinish
        map <- gerMapa size startFinish
        let chavesAdequiridas = []
        let estaTerminado = False
        a <- choose(0,1)
        let mapa = introduzPortais map portais a           
        let posicaoJogador = posicaoChar mapa 'S' (length mapa)
        return $ EstadoJogo mapa posicaoJogador chavesAdequiridas estaTerminado

introduzPortais :: [String] -> [(Int,Int)]-> Int -> [String]
introduzPortais mapa [x,y] z = do
    if z == 0 then do
        a <- mapa 
        return a 
    else do
        a <- introduzChar mapa '@' (length mapa) x [] 
        b <- introduzChar mapa '@' (length mapa) y [] 
        return b

gerWidth :: Gen Int
gerWidth = do
    a <- choose(3,15)
    return a

gerSize :: Gen (Int,Int)
gerSize = do
    a <- gerWidth
    if a == 3 then do
        b <- choose (4,15)
        return (a,b) 
    else do
        b <- choose (4,15)
        return (a,b)


gerStartAndFinish :: (Int,Int) -> Gen [(Int,Int)]
gerStartAndFinish (width,heigth) = do
    sWidth <-choose (1, width -2)
    sHeigth <- choose (1, heigth -2)
    fWidth <-choose (1, width -2)
    fHeigth <- choose (1, heigth -2)
    if (sWidth, sHeigth) == (fWidth, fHeigth) then do
        a <- gerStartAndFinish (width, heigth)
        return a
    else 
        return [(sWidth, sHeigth),(fWidth, fHeigth)]

gerLine :: Int -> (Int,Int) -> [(Int,Int)] -> Gen String
gerLine lineNr (maxiW, maxiH) [(sW, sH),(fW, fH)] = do
    if elem lineNr [0,maxiH] then
        return $ replicate maxiW '*'
    else if lineNr == sH || lineNr == fW then do
        a <- gerLineAux 0 maxiW (sW,fW) []
        return a 
    else do 
        a <- gerLineAux2 0 maxiW []
        return a 

gerLineAux :: Int -> Int -> (Int,Int) -> [Char] -> Gen String
gerLineAux cNr maxi (sW,fW) x = do
    if cNr == 0 || cNr == (maxi -1) then
        gerLineAux (cNr + 1) maxi (sW,fW) (x ++ "*")
    else if cNr == maxi then
        return x
    else if cNr == sW then do
        gerLineAux (cNr + 1) maxi (sW,fW) (x ++ "S")
    else if cNr == fW then do
        gerLineAux (cNr + 1) maxi (sW,fW) (x ++ "F")
    else do
        i <- choose (0, 7)
        let a = ["*", " ", "a", "b", "c", "A", "B", "C"]
        (gerLineAux (cNr + 1) maxi (sW,fW) (x++(a!!i)))

gerLineAux2 :: Int -> Int -> [Char] ->Gen [Char]
gerLineAux2 cNr maxi x = do
    if cNr == 0 || cNr == (maxi -1) then
        (gerLineAux2 (cNr + 1) maxi (x++ "*"))
    else if cNr == maxi then 
        return x
    else do
        i <- choose (0, 7)
        let a = ["*", " ", "a", "b", "c", "A", "B", "C"]
        (gerLineAux2 (cNr + 1) maxi (x++(a!!i)))

gerPortais :: (Int,Int) -> [(Int, Int)] ->Gen [(Int,Int)] 
gerPortais (width,heigth) startAndFinishPos = do
    p1W <-choose (1, width -2)
    p1H <- choose (1, heigth -2)
    p2W <-choose (1, width -2)
    p2H <- choose (1, heigth -2)
    if (p1W, p1H) == (p2W, p2H) || elem (p1W, p1H) startAndFinishPos || elem (p2W, p2H) startAndFinishPos then do 
        a <- gerPortais (width, heigth) startAndFinishPos
        return a
    else return [(p1W, p1H),(p2W, p2H)]

gerMapa :: (Int,Int) -> [(Int,Int)] -> Gen [String]
gerMapa (maxiW, maxiH) [(sW, sH),(fW, fH)] = do
    a <- gerMapaAux (maxiW, maxiH) [(sW, sH),(fW, fH)] 0 []
    return a

gerMapaAux :: (Int,Int) -> [(Int,Int)] -> Int -> [String] -> Gen [String]
gerMapaAux (maxiW, maxiH) startAndFinishPos currentLn x = do
    if currentLn == maxiH then 
        return x 
    else do
        a <- gerLine currentLn (maxiW, maxiH) startAndFinishPos
        gerMapaAux (maxiW, maxiH) startAndFinishPos (currentLn + 1) (x ++ [a])


newtype Movimentos = Movimentos String
    deriving Show

instance Arbitrary Movimentos where
  arbitrary :: Gen Movimentos
  arbitrary = do 
    v <-choose(1,40)
    a <- genMove v ""
    return $ Movimentos a

genMove :: Int -> String -> Gen String
genMove x moves = do
    if x == 0 then
        return moves
    else do
        a <- choose(0,3)
        let b = ["l", "r", "u", "d"] !! a
        genMove (x - 1) (moves ++ b)
{-
Verifica se o tamanho do mapa se manteve
-}
afterMoveDimTest :: EstadoJogo -> Movimentos -> Bool
afterMoveDimTest state moves = 
    length (mapa state) == length (mapa (mover state (show moves))) && 
    length (head (mapa state)) == length (head (mapa (mover state (show moves))))


{-
Verifica que um jogador não acaba fora do mapa
-}
afterMovePlayerTest :: EstadoJogo -> Movimentos -> Bool
afterMovePlayerTest state moves = 
    fst (posicaoJogador (mover state (show moves))) < length (mapa state) &&
    snd (posicaoJogador (mover state (show moves))) < length (head (mapa state)) &&
    fst (posicaoJogador (mover state (show moves))) > 1 &&
    snd (posicaoJogador (mover state (show moves))) > 1

{-
Verifica que a lista de chaves não diminui após movimentos
-}
afterMoveKeys :: EstadoJogo -> Movimentos -> Bool
afterMoveKeys state moves =  length (chaves state) >= length(chaves (mover state(show moves)))

{-
Verifica que o número de portas se mantém constante
 -}
afterMoveDoors :: EstadoJogo -> Movimentos -> Bool
afterMoveDoors state moves = length (dropWhile (`notElem` ['A'..'Z']) (unwords (mapa state))) == length (dropWhile (`notElem`['A'..'Z'] ) (unwords (mapa (mover state (show moves)))))

{-
Verifica que o número de portais se mantém constante
 -}
afterMovePortais :: EstadoJogo -> Movimentos -> Bool
afterMovePortais state moves = 
    countLetters (mapa state) '@' == countLetters(mapa (mover state (show moves))) '@'

{-
Conta ocorrencias de um char numa lista destrings
 -}
countLetters :: [String] -> Char -> Int
countLetters str c = length $ filter (== c) (unlines str)

{-
Verifica não se adquirem chaves inválidas
 -}
noWeirdKeys :: EstadoJogo -> Movimentos -> Bool
noWeirdKeys state moves = null (dropWhile (`elem` ['a'..'c']) (chavesAdequiridas state))

{-
Verifica que o jogador não passa por paredes
 -}
noWalling :: EstadoJogo -> Movimentos -> Bool
noWalling state moves = (((mapa state)!!(fst (posicaoJogador state)))!!(snd(posicaoJogador state))  /= '*')    

{-
Verifica que o jogador não semexe se receber um move vazio
 -}
noMove :: EstadoJogo  -> Bool
noMove state = posicaoJogador state == posicaoJogador (mover state "")

testes :: IO()
testes = do
    quickCheck afterMoveDimTest
    --quickCheck afterMovePlayerTest
    --quickCheck afterMoveKeys
    --quickCheck afterMoveDoors
    --quickCheck afterMovePortais
    --quickCheck noWeirdKeys
    --quickCheck noWalling
    --quickCheck noMove