{-
Princípios de Programação 2022/2023
Trabalho 4 - Modelo de submissão

* A vossa submissão deverá ser composta por um único ficheiro zip
t4_XXXXX_YYYYY.zip onde XXXXX, YYYYY são os vossos números de aluno
por ordem crescente.
* O ficheiro zip deverá conter no mínimo um ficheiro com o nome Main.hs
* O vosso código deverá ser compilável com uma instrução do tipo

$ stack ghc Main.hs

A instrução acima produz um executável Main, que deverá ser executável
através de um dos seguintes três tipos de instruções:

$ ./Main [ficheiro] -- carrega um ficheiro para jogar
$ ./Main            -- carrega o ficheiro default.map
$ ./Main -t         -- corre os testes
-}

import System.Environment ( getArgs )
import System.IO ()
import Control.Monad ()
import Labirintos ( EstadoJogo )
import System.Directory ( doesFileExist )
import Functions ( move, load, save, exit )
import Testes



{-
Função que inicia o jogo e prepara o programa para recber input StdIn
-}
main :: IO()
main = do
    args <- getArgs
    if null args then do
        jogo <- load "default.map"
        print jogo
        afterMain jogo
    else if head (args) == "-t" then do
        testes
    else do
        fileStatus <- doesFileExist (head args)

        if fileStatus then do
            jogo <- load (head args)
            print jogo
            afterMain jogo
        else do
            putStrLn "Utilizacao:"
            putStrLn "\t ./Main [ficheiro]"
            putStrLn "\t ./Main"
            putStrLn "\t ./Main -t"     
    
{- 
Função que se segue à Main, recebe comandos através do StdIn e executa-os
-}
afterMain :: EstadoJogo -> IO()
afterMain jogo = do
    input <- getLine
    let inputList = words input
    argStatus <- doesFileExist $ last inputList
    
    if head inputList == "exit" && length inputList == 1 
        then exit 

    else if head inputList == "load" && argStatus
        then do 
            jogoLoad <- load (last inputList)
            print jogoLoad
            afterMain jogoLoad

    else if head inputList == "save" && length inputList == 2 
        then do 
            jogoSave <- save jogo (last inputList)
            print jogoSave
            afterMain jogoSave

    else if head inputList == "move" && length inputList == 2 
        then do
            jogoMove <- move jogo (last inputList)
            print jogoMove
            afterMain jogoMove
    else do 
        print inputList
        putStr "Comando ou argumento invalido \n"
        afterMain jogo
