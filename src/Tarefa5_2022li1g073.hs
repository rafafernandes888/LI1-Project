{- |
Module      : Tarefa5_2022li1g073
Description : Deslize do mapa
Copyright   : Ricardo Gomes de Sousa  <a104524 @alunos.uminho.pt>
              Pedro Miguel Araújo Gomes <a104540@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g073 where

import LI12223
import System.Random
import Data.Maybe
import Tarefa1_2022li1g073
import Tarefa2_2022li1g073

{- | Esta é a função principal que remove a última linha do mapa e adiciona uma nova linha válida no topo do mapa

@
deslizaJogo :: Int -> Jogo -> Jogo 
deslizaJogo r (Jogo (Jogador (x,y)) (Mapa l t)) = (Jogo (Jogador (x, y-1)) (nextmapa r (Mapa l tira)))
                                   where tira = init t
@

-}

deslizaJogo :: Int -> Jogo -> Jogo 
deslizaJogo r (Jogo (Jogador (x,y)) (Mapa l t)) = (Jogo (Jogador (x, y)) (nextmapa r (Mapa l tira)))
                                   where tira = init t


{- | Esta função usa a função "estendeMap" da Tarefa2 e na função "mapaValido" da Tarefa1. 
Esta faz correr a "estendeMapa" até gerar uma nova linha que é depois validada pelo "mapavalido",
se for válida, é adicionada ao mapa, se não for, repete a função com um valor Int diferente para gerar uma linha nova aleatória -}

nextmapa :: Int -> Mapa -> Mapa  
nextmapa x (Mapa l t) = if mapaValido (estendeMapa (Mapa l t) x) then estendeMapa (Mapa l t) x else nextmapa (x+1) (Mapa l t)
