module Tarefa5_2022li1g073_Spec where

import LI12223
import Tarefa5_2022li1g073
import Test.HUnit


testsT1 = test [ "Teste 1" ~: Jogo (Jogador (1,-1)) (Mapa 5 [(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Nenhum])]) ~=? deslizaJogo 1 (Jogo (Jogador (1,0)) (Mapa 5 [(Relva,[Nenhum, Nenhum, Arvore, Arvore, Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 1,[Nenhum,Carro,Carro,Nenhum,Nenhum])])),
                 "Teste 2" ~: Jogo (Jogador (1,-4)) (Mapa 5 [(Estrada (-3),[Nenhum,Carro,Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Nenhum])]) ~=? deslizaJogo 23 (Jogo (Jogador (1,-3)) (Mapa 5 [(Relva,[Nenhum, Nenhum, Arvore, Arvore, Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 1,[Nenhum,Carro,Carro,Nenhum,Nenhum])])),
                 "Teste 2" ~: Jogo (Jogador (1,-2)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Nenhum])]) ~=? deslizaJogo 12 (Jogo (Jogador (1,-1)) (Mapa 5 [(Relva,[Nenhum, Nenhum, Arvore, Arvore, Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 1,[Nenhum,Carro,Carro,Nenhum,Nenhum])]))
               ]