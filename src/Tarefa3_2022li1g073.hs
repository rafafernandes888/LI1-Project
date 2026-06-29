{- |
Module      : Tarefa3_2022li1g073
Description : Movimentação do personagem e obstáculos
Copyright   : Ricardo Gomes de Sousa  <a104524 @alunos.uminho.pt>
              Pedro Miguel Araújo Gomes <a104540@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g073 where

import LI12223

{- | A função "animaJogo" implementa a etapa relativa à movimentação do personagem e dos obstáculos.
Assim sendo, fazendo uso de algumas funções auxiliares, movimenta os obstáculos (de acordo com a velocidade
do terreno em que se encontram), e o personagem, de acordo com a jogada dada.

@
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l t)) (direcao) = (Jogo player novomapa)
                                  where novomapa = movelinha (Mapa l t) 
                                        posicaonova = move l (Jogador (x,y)) (direcao)
                                        player = posicaorio l (posicaonova) linh1 (direcao) 
                                        linh1 = (!!) t (abs y)
@

-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l t)) (direcao) = (Jogo player novomapa)
                                  where novomapa = movelinha (Mapa l t) 
                                        Jogador (x1,y1) = move (Jogo (Jogador (x,y)) (Mapa l t)) (direcao)
                                        player = posicaorio l (Jogador (x1,y1)) linh1 (direcao) 
                                        linh1 = (!!) t (abs y1)

{- | A função "movelinha" é o motor do "novo" mapa que será criado pela movimentação dos obstáculos,
assim, garante que uma estrada ou rio com velocidade v, os obstáculos devem mover-se
|v| unidades na direção determinada e garante ainda que ao deslocar os obstáculos de uma linha,
assim que desaparecerem por um dos lados do mapa, estes reapareçam no lado oposto dessa mesma linha.
-}

movelinha :: Mapa -> Mapa
movelinha (Mapa l ((terr, obs) :t)) = Mapa l (movelinhaaux ((terr, obs) : t))

{- | A função "movelinhaaux" auxilia a função "movelinha" a cumprir o seu propósito. 
-}

movelinhaaux :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
movelinhaaux [(Rio x, obs)] = if x > 0 then [(Rio x, dl)] else [(Rio x, el)] 
                          where dl = deslocaDireita x obs
                                el = deslocaEsq (abs x) obs
movelinhaaux ((Rio x, obs) : t) = if x > 0 then ((Rio x, dl) : recurs) else ((Rio x, el) :recurs)
                           where dl = deslocaDireita x obs
                                 recurs = movelinhaaux t 
                                 el = deslocaEsq (abs x) obs
movelinhaaux [(Estrada y, obs)] = if y > 0 then [(Estrada y, dl)] else [(Estrada y, el)] 
                          where dl = deslocaDireita y obs
                                el = deslocaEsq (abs y) obs
movelinhaaux ((Estrada y, obs) : t) = if y > 0 then ((Estrada y, dl) : recurs) else ((Estrada y, el) :recurs)
                           where dl = deslocaDireita y obs
                                 recurs = movelinhaaux t 
                                 el = deslocaEsq (abs y) obs
movelinhaaux [h] = [h]
movelinhaaux (h : t) = h : recurs 
                    where recurs = movelinhaaux t 


deslocaDireita :: Int -> [a] -> [a]
deslocaDireita n l = drop ((length l) - a) l ++ take ((length l) - a) l
                     where a = mod n (length l) 

{- | As funções "deslocaDireita" e "deslocaEsq" são essencias para o funcionamento da 
função "movelinhaaux" e, consequentemente, da "movelinha" e participam na deslocação
dos obstáculos numa dada linha. 
-}

deslocaEsq :: Int -> [a] -> [a]
deslocaEsq n l = let a = mod n (length l) 
                 in drop a l ++ take a l 

{- | A função "move" movimenta o jogador, isto é, altera a posição do jogador de 
acordo com a jogada que lhe é dada.
-}

move :: Jogo -> Jogada -> Jogador
move (Jogo (Jogador (x,y)) (Mapa k t)) dir | dir == Move Cima = if foramapa1 k (Jogador (x,y)) t || busca(Jogo (Jogador (x,y+1)) (Mapa k t)) == (Relva, Arvore)
                                                                then Jogador (x,y)
                                                                else Jogador (x,y+1)
                                           | dir == Move Baixo = if foramapa1 k (Jogador (x,y)) t || busca(Jogo (Jogador (x,y-1)) (Mapa k t)) == (Relva, Arvore)
                                                                 then Jogador (x,y)
                                                                 else Jogador (x,y-1)
                                           | dir == Move Direita = if foramapa1 k (Jogador (x,y)) t || busca(Jogo (Jogador (x+1,y)) (Mapa k t)) == (Relva, Arvore)
                                                                   then Jogador (x,y)
                                                                   else Jogador (x+1,y)
                                           | dir == Move Esquerda = if foramapa1 k (Jogador (x,y)) t || busca(Jogo (Jogador (x-1,y)) (Mapa k t)) == (Relva, Arvore)
                                                                    then Jogador (x,y)
                                                                    else Jogador (x-1,y)
                                           | dir == Parado = Jogador (x,y)
foramapa1 :: Largura -> Jogador -> [(Terreno, [Obstaculo])] -> Bool  
foramapa1 l (Jogador (x, y)) t | y <= 0 = if x < 0 || y>0 || x >= l || (abs y) >= a then True else False 
                                      where a = length t 

selectT :: [(Terreno, [Obstaculo])] -> Int -> (Terreno, [Obstaculo])
selectT mapa y = mapa !! abs y

busca :: Jogo -> (Terreno, Obstaculo)
busca (Jogo (Jogador (x,y)) (Mapa k t)) = (ter,obs)
            where ter = fst (selectT t y)
                  obs = snd (selectT t y) !! x

{- | A função "posicaorio" garante que se o personagem se encontrar em cima de um tronco,
o jogador acompanha o movimento tronco e ainda, que o jogador não pode estar na mesma 
posição que uma árvore, um obstáculo.
-}

posicaorio :: Largura -> Jogador -> (Terreno, [Obstaculo]) -> Jogada -> Jogador 
posicaorio l (Jogador (x, y)) (Rio z, obs) (direcao) = if elem x xs then (Jogador (x+z, y)) else (Jogador (x, y))   
                              where xs = xTronco Tronco obs 
posicaorio l (Jogador (x, y)) _ (direcao) = (Jogador (x, y))

{- | A função "xTronco" dá uma lista com o índice das abcissas de um dado obstáculo especificado em uma dada linha,
por exemplo na função "posicaorio" é especificado o obstáculo Tronco nos terrenos de Rio e Árvore nos terrenos de Relva.
-}

xTronco :: Obstaculo -> [Obstaculo] -> [Int]
xTronco o l = xTroncoaux o l 0

{- | A função "xTroncoaux" é uma função auxiliar essencial para o bom funcionamento da função "xTronco".
-}

xTroncoaux :: Obstaculo -> [Obstaculo] -> Int -> [Int]
xTroncoaux _ [] _ = []
xTroncoaux o (h:t) i
    | o == h = i : xTroncoaux o t (i+1)
    | otherwise = xTroncoaux o t (i+1)








        
