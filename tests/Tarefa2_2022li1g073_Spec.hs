module Tarefa2_2022li1g073_Spec where

import LI12223
import Tarefa2_2022li1g073
import Test.HUnit


testsT2 = test [ "Teste 1" ~: Mapa 10 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Rio (-2),[Tronco,Tronco,Tronco,Tronco,Tronco])] ~=? estendeMapa (Mapa 10 [(Rio 0, [Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum])]) 18,
               "Teste 2" ~: Mapa 10 [(Rio (-2),[Tronco,Tronco,Tronco,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Rio (-5),[Tronco,Tronco,Tronco,Tronco,Tronco])] ~=? estendeMapa (Mapa 10 [(Rio 0, [Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum])]) 7,
               "Teste 3" ~: Mapa 10 [(Estrada (-4),[Carro,Carro,Carro,Carro,Carro,Carro,Carro,Carro,Carro]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-4),[Carro,Carro,Carro,Carro,Carro,Carro,Carro,Carro,Carro])] ~=? estendeMapa (Mapa 10 [(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Carro])]) 4,
               "Teste 4" ~: Mapa 4 [(Relva,[Nenhum]),(Rio 4,[Tronco]),(Relva,[Nenhum])] ~=? estendeMapa (Mapa 4 [(Relva,[Tronco,Tronco,Nenhum])]) 6
               ]
