module Tarefa4_2022li1g073_Spec where

import LI12223
import Tarefa4_2022li1g073
import Test.HUnit


testsT4 = test ["Teste 1" ~: True ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 3 [(Rio 0,[Nenhum,Tronco,Nenhum])])),
 			  "Teste 2" ~: False ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 0,[Nenhum,Tronco,Nenhum])])),
              "Teste 3" ~: True ~=? jogoTerminou (Jogo (Jogador (2,0)) (Mapa 3 [(Rio 0,[Nenhum,Tronco,Nenhum])])),
              "Teste 4" ~: True ~=? jogoTerminou (Jogo (Jogador (-1,0)) (Mapa 3 [(Rio 0,[Nenhum,Tronco,Nenhum])])),
              "Teste 5" ~: True ~=? jogoTerminou (Jogo (Jogador (3,0)) (Mapa 3 [(Rio 0,[Nenhum,Tronco,Nenhum])])),
              "Teste 6" ~: True ~=? jogoTerminou (Jogo (Jogador (-1,0)) (Mapa 3 [(Estrada 0,[Nenhum,Carro,Nenhum])])),
 			  "Teste 7" ~: True ~=? jogoTerminou (Jogo (Jogador (3,0)) (Mapa 3 [(Estrada 0,[Nenhum,Carro,Nenhum])])),
 			  "Teste 8" ~: True ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 0,[Nenhum,Carro,Nenhum])])),
 			  "Teste 9" ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 3 [(Estrada 0,[Nenhum,Carro,Nenhum])])),
 			  "Teste 10" ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum])]))
             ]
