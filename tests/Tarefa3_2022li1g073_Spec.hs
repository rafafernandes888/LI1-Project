module Tarefa3_2022li1g073_Spec where

import LI12223
import Tarefa3_2022li1g073
import Test.HUnit

testsT3 = test ["Teste 1" ~: Jogo (Jogador (3,0)) (Mapa 4 [(Rio 1, [Nenhum, Nenhum, Tronco, Tronco])]) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1, [Nenhum, Tronco, Tronco, Nenhum])])) (Move Direita),
 			  "Teste 2" ~: Jogo (Jogador (1,-1)) (Mapa 4 [(Rio 1,[Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,(-1))) (Mapa 4 [(Rio 1, [Nenhum, Tronco, Tronco, Nenhum]), (Relva, [Nenhum, Nenhum, Arvore, Nenhum])])) (Move Direita),
 			  "Teste 3" ~: Jogo (Jogador (2,-1)) (Mapa 4 [(Rio 1,[Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,(-1))) (Mapa 4 [(Rio 1, [Nenhum, Tronco, Tronco, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])) (Move Direita),
 			  "Teste 4" ~: Jogo (Jogador (3,0)) (Mapa 4 [(Rio 1,[Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,(-1))) (Mapa 4 [(Rio 1, [Nenhum, Tronco, Tronco, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])) (Move Cima),
 			  "Teste 5" ~: Jogo (Jogador (0,0)) (Mapa 4 [(Rio (-2),[Tronco,Nenhum,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,0)) (Mapa 4 [(Rio (-2), [Nenhum, Tronco, Tronco, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])) (Move Cima)
                 ]
