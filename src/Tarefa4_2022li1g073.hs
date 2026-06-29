{- |
Module      : Tarefa4_2022li1g073
Description : Determinar se o jogo terminou
Copyright   : Ricardo Gomes de Sousa  <a104524 @alunos.uminho.pt>
              Pedro Miguel Araújo Gomes <a104540@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g073 where

import LI12223

{- | A função "jogoTerminou" como o jogo indica irá traduzir aquilo que seria um "Game Over",
no nosso jogo, indica portanto se o jogador perdeu o jogo, onde True significa que sim. Para isso
deve testar se o jogador se encontra fora do mapa, na água, ou “debaixo”
de um carro, (ou seja, na mesma posição de um carro).

@
jogoTerminou :: Jogo -> Bool  
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l t)) = morreste (Jogador (x,y)) t 
                                             || foramapa l (Jogador (x,y)) t 
@

-}

jogoTerminou :: Jogo -> Bool  
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l t)) = morreste (Jogador (x,y)) t 
                                             || foramapa l (Jogador (x,y)) t 
                                             || atropelado (Jogador (x,y)) (Mapa l t)

{- A função "morreste" vai determinar se o jogador perdeu, onde True significa que sim, 
para os seguintes casos: caso o jogador "caia no rio e se afogue", isto é, se
se encontrar na mesma posição de um "obstáculo" Nenhum no Terreno de Rio e caso o jogador se
encontre "debaixo" de um carro, isto é, se se encontrar na mesma posição de um "obstáculo" 
carro no Terreno de Estrada.
-}
morreste :: Jogador -> [(Terreno, [Obstaculo])] -> Bool 
morreste (Jogador (x, y)) t = if morreste1 (Jogador (x,y)) h then True else False 
                              where h = (!!) t (abs y)

{- | A função "morreste1" é uma função auxiliar necessário para que a função "morreste" atinja
o seu propósito.
-}
morreste1 :: Jogador -> (Terreno, [Obstaculo]) -> Bool 
morreste1 (Jogador (x, y)) (Rio z, obs) = if elem x xs then True else False 
                              where xs = xNenhum4 Nenhum obs  
morreste1 (Jogador (x, y)) (Estrada z, obs) = if elem x ss then True else False 
                              where ss = xNenhum4 Carro obs
morreste1 (Jogador (x, y)) (Relva, _) = False 


{- | A função "xNenhum4" dá uma lista com o índice das abcissas de um dado obstáculo especificado em uma dada linha,
por exemplo na função "morreste1" é especificado o obstáculo Nenhum nos terrenos de Rio e Carro nos terrenos de Estrada.
Consequentemente, a função "morreste" não funcionaria sem a "xNenhum4".
-}

xNenhum4 :: Obstaculo -> [Obstaculo] -> [Int]
xNenhum4 o l = xNenhum4aux o l 0

{- A função "xNenhum4aux" é uma função auxiliar necessária para o bom funcionamento da função "xNenhum4".
-}

xNenhum4aux :: Obstaculo -> [Obstaculo] -> Int -> [Int]
xNenhum4aux _ [] _ = []
xNenhum4aux o (h:t) i
    | o == h = i : xNenhum4aux o t (i+1)
    | otherwise = xNenhum4aux o t (i+1)

{- | A função "foramapa" vai testar se o jogador se encontra fora do mapa, se sim então isto quererá dizer que o 
jogador perdeu, onde True significa que sim. 
-}
foramapa :: Largura -> Jogador -> [(Terreno, [Obstaculo])] -> Bool  
foramapa l (Jogador (x, y)) t | y <= 0 = if x < 0 || y>0 || x >= l || (abs y) >= a then True else False 
                                      where a = length t 

atropelado :: Jogador -> Mapa -> Bool  
atropelado (Jogador (x,y)) (Mapa l t) = atropeladoaux (Jogador (x,y)) linha  
                                        where linha = (!!) t (abs y)

atropeladoaux :: Jogador -> (Terreno, [Obstaculo]) -> Bool 
atropeladoaux (Jogador (x, y)) (Estrada v, obs) = testa x v i
                       where i = xNenhum4 Carro obs

atropeladoaux (Jogador (x,y)) (_, obs) = False  

testa :: Int -> Int -> [Int] -> Bool 
testa x v [] = False 
testa x v (c:i) = if v > 0 && c <= x && (c+v) >= x then True else 
                  if v < 0 && c >= x && (c+v) <= x then True else testa x v i 


move4 :: Jogador -> Jogada -> Jogador 
move4 (Jogador (x, y)) Parado = (Jogador (x, y)) 
move4 (Jogador (x, y)) (Move Cima) = if y == 0 then (Jogador (x, y)) else (Jogador (x, y+1))
move4 (Jogador (x, y)) (Move Baixo) = (Jogador (x, y-1))
move4 (Jogador (x, y)) (Move Direita) = (Jogador (x+1, y))
move4 (Jogador (x, y)) (Move Esquerda) = (Jogador (x-1, y))


movelinha4 :: Mapa -> Mapa
movelinha4 (Mapa l ((terr, obs) :t)) = Mapa l (movelinhaaux4 ((terr, obs) : t))

movelinhaaux4 :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
movelinhaaux4 [(Rio x, obs)] = if x > 0 then [(Rio x, dl)] else [(Rio x, el)] 
                          where dl = deslocaDireita x obs
                                el = deslocaEsq (abs x) obs
movelinhaaux4 ((Rio x, obs) : t) = if x > 0 then ((Rio x, dl) : recurs) else ((Rio x, el) :recurs)
                           where dl = deslocaDireita x obs
                                 recurs = movelinhaaux4 t 
                                 el = deslocaEsq (abs x) obs
movelinhaaux4 [(Estrada y, obs)] = if y > 0 then [(Estrada y, dl)] else [(Estrada y, el)] 
                          where dl = deslocaDireita y obs
                                el = deslocaEsq (abs y) obs
movelinhaaux4 ((Estrada y, obs) : t) = if y > 0 then ((Estrada y, dl) : recurs) else ((Estrada y, el) :recurs)
                           where dl = deslocaDireita y obs
                                 recurs = movelinhaaux4 t 
                                 el = deslocaEsq (abs y) obs
movelinhaaux4 [h] = [h]
movelinhaaux4 (h : t) = h : recurs 
                    where recurs = movelinhaaux4 t 

deslocaEsq :: Int -> [a] -> [a]
deslocaEsq n l = let a = mod n (length l) 
                 in drop a l ++ take a l 

deslocaDireita :: Int -> [a] -> [a]
deslocaDireita n l = drop ((length l) - a) l ++ take ((length l) - a) l
                     where a = mod n (length l) 