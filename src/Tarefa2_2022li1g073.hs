{- |
Module      : Tarefa2_2022li1g073
Description : Geração contínua de um mapa
Copyright   : Ricardo Gomes de Sousa  <a104524 @alunos.uminho.pt>
              Pedro Miguel Araújo Gomes <a104540@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g073 where

import LI12223
import System.Random 

{- | Esta é a função principal que adiciona uma nova linha ao topo do mapa

@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ((ter, obs) : t)) seed = (Mapa l (( geraterreno (Mapa l ((ter, obs) : t)) seed , geralinha l ( geraterreno (Mapa l ((ter, obs) : t)) seed , []) seed) : (ter, obs) : t))
@

-}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ((ter, obs) : t)) seed = (Mapa l (( geraterreno (Mapa l ((ter, obs) : t)) seed , geralinha l ( geraterreno (Mapa l ((ter, obs) : t)) seed , []) seed) : (ter, obs) : t))



{- | Esta função auxiliar recebe o mapa da função estendeMapa e o número aleatório e indica os possíveis terrenos a acrescentar com velocidades aleatórias -}
proximosTerrenosValidos :: Mapa -> Int -> [Terreno]
proximosTerrenosValidos (Mapa l []) seed = [Estrada (head (aleatoriosv seed 1)), Rio (head (aleatoriosv seed 1)), Relva]



{- | Esta função auxiliar recebe o mapa da função estendeMapa e o número aleatório e gera um terreno válido indicado na função proximosTerrenosValidos -}
geraterreno :: Mapa -> Int -> Terreno
geraterreno (Mapa l []) seed = if head (aleatorios seed 1) == 0 then (!!) (proximosTerrenosValidos (Mapa l []) seed) 0
                                     else if head (aleatorios seed 1) == 1 then (!!) (proximosTerrenosValidos (Mapa l []) seed) 1
                                            else Relva
geraterreno (Mapa l ((Estrada _, _) : (Estrada _, _) : (Estrada _, _) : (Estrada _, _) : (Estrada _, _) : t)) seed = if head (aleatorios1 seed 1) == 0 then (!!) (proximosTerrenosValidos (Mapa l []) seed) 1
                                                                                                                       else Relva
geraterreno (Mapa l ((Rio _, _) : (Rio _, _) : (Rio _, _) : (Rio _, _) : t)) seed = if head (aleatorios1 seed 1) == 0 then (!!) (proximosTerrenosValidos (Mapa l []) seed) 0
                                                                                      else Relva
geraterreno (Mapa l ((Relva, _) : (Relva, _) : (Relva, _) : (Relva, _) : (Relva, _) : t)) seed = if head (aleatorios1 seed 1) == 0 then (!!) (proximosTerrenosValidos (Mapa l []) seed) 0
                                                                                                   else Rio 0
geraterreno (Mapa l _) seed = if head (aleatorios seed 1) == 0 then (!!) (proximosTerrenosValidos (Mapa l []) seed) 0
                                 else if head (aleatorios seed 1) == 1 then (!!) (proximosTerrenosValidos (Mapa l []) seed) 1
                                        else Relva



{- | Esta função auxiliar recebe a largura do mapa e uma linha do mapa e indica os possíveis obstáculos a acrescentar de acordo com o terreno da linha e a sua largura -}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (Rio _, []) = [Nenhum, Tronco]
proximosObstaculosValidos l (Estrada _, []) = [Nenhum, Carro]
proximosObstaculosValidos l (Relva, []) = [Nenhum, Arvore]



{- | Esta função auxiliar recebe a largura do mapa, uma linha do mapa e um número aleatório e gera um obstáculo indicado na função proximosObstaculosValidos -}
geraobstaculo :: Int -> (Terreno, [Obstaculo]) -> Int -> Obstaculo
geraobstaculo l (Rio _, []) seed = if head (aleatorios1 (seed+43) 1) == 0 then (!!) (proximosObstaculosValidos l (Rio 0, [])) 0 else (!!) (proximosObstaculosValidos l (Rio 0, [])) 1
geraobstaculo l (Estrada _, []) seed = if head (aleatorios1 (seed+34) 1) == 0 then (!!) (proximosObstaculosValidos l (Estrada 0, [])) 0 else (!!) (proximosObstaculosValidos l (Estrada 0, [])) 1
geraobstaculo l (Relva, []) seed = if head (aleatorios1 (seed+85) 1) == 0 then (!!) (proximosObstaculosValidos l (Relva, [])) 0 else (!!) (proximosObstaculosValidos l (Relva, [])) 1



{- | Esta função auxiliar recebe a largura do mapa, uma linha do mapa e um número aleatório e gera a linha de obstáculos respeitando a largura do Mapa -}
geralinha :: Int -> (Terreno, [Obstaculo]) -> Int -> [Obstaculo]
geralinha 0 (_, _) seed = []

geralinha l (Rio _, obs) seed 
 | obs == [] = [geraobstaculo l (Rio 0, []) seed] ++ geralinha (l-1) (Rio 0, obs) (seed+23)
 | length obs == l = []
 | otherwise = [geraobstaculo l (Rio 0, obs) seed] ++ geralinha (l-1) (Rio 0, obs) seed

geralinha l (Estrada _, obs) seed 
 | obs == [] = [geraobstaculo l (Estrada 0, []) seed] ++ geralinha (l-1) (Estrada 0, obs) (seed+23)
 | length obs == l = []
 | otherwise = [geraobstaculo l (Estrada 0, obs) seed] ++ geralinha (l-1) (Estrada 0, obs) seed

geralinha l (Relva, obs) seed 
 | obs == [] = [geraobstaculo l (Relva, []) seed] ++ geralinha (l-1) (Relva, obs) (seed+23)
 | length obs == l = []
 | otherwise = [geraobstaculo l (Relva, obs) seed] ++ geralinha (l-1) (Relva, obs) seed



{- | Esta função auxiliar gera um número aleatório de 0 a 2 que corresponde à posição de um terreno na lista que a função proximosTerrenosValido devolve -}
aleatorios :: Int -> Int -> [Int]
aleatorios seed len = take len $ randomRs (0,2) (mkStdGen seed)

{- | Esta função auxiliar gera um número aleatório de 0 a 1 que corresponde à posição de um terreno na lista que a função proximosTerrenosValido devolve e à posição de um obstáculo na lista que a função proximosObstaculosValidos devolve -}
aleatorios1 :: Int -> Int -> [Int]
aleatorios1 seed len = take len $ randomRs (0,1) (mkStdGen seed)

{- | Esta função auxiliar gera uma velocidade aleatória, diferente de 0 para os terrenos Rio e Estrada -}
aleatoriosv :: Int -> Int -> [Int]
aleatoriosv seed len = take len $ randomRs ((-2),2) (mkStdGen seed) 
