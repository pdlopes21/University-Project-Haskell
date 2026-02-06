module Functions (
    move,
    load,
    save,
    exit
) where

import System.Exit ( exitSuccess )
import Labirintos
    ( inicializa,
      mover,
      EstadoJogo(mapa, posicaoJogador, chavesAdequiridas) )

{-
Move o jogador nas direções mencionadas no StdIn
-}
move :: EstadoJogo -> String -> IO EstadoJogo
move state moves = return (mover state moves)

{-
Carrega um estado de jogo de um ficheiro existente, passando a ser esse o estado
de jogo no qual o jogador se vai movimentar a partir desse momento
-}
load :: String -> IO EstadoJogo
load map = do
    contents <- readFile map
    let args = lines contents
        jogo =  inicializa (tail (tail args)) (head args) (head(tail args))
    return jogo

{-
Guarda o estado de jogo atual num ficheiro, dando overwrite ao
conteúdo deste caso exista, ou criando um novo caso não exista ainda
-}
save :: EstadoJogo -> String -> IO EstadoJogo
save state file = do
    writeFile file (unlines [show (posicaoJogador state), chavesAdequiridas state , init (unlines(mapa state)) ])
    return state
    
{-
Acaba o jogo e volta ao terminal
-}    
exit :: IO()
exit = exitSuccess
