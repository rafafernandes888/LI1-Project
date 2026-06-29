{- |
Module      : Tarefa1_2022li1g073
Description : Validação de um mapa
Copyright   : Ricardo Gomes de Sousa  <a104524 @alunos.uminho.pt>
              Pedro Miguel Araújo Gomes <a104540@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g073 where

import LI12223

{- | A função "mapaValido", através da concordância entre as 10 funções auxiliares que a compõem, 
verifica se um dado mapa é válido, isto é, não viola nenhuma das restrições impostas pelas funções auxiliares.

@
mapaValido :: Mapa -> Bool
mapaValido (Mapa l t) = mapaValido1 (Mapa l t) 
                       && mapaValido2 (Mapa l t)
                       && mapaValido3 (Mapa l t)
                       && mapaValido3excecao (Mapa l t)
                       && mapaValido4 (Mapa l t)
                       && mapaValido4excecao (Mapa l t)
                       && mapaValido5 (Mapa l t)
                       && mapaValido5especial (Mapa l t)
                       && mapaValido6 (Mapa l t)
                       && mapaValido7 (Mapa l t)
                       && mapaValido8 l t
@

-}
mapaValido :: Mapa -> Bool
mapaValido (Mapa l t) = mapaValido1 (Mapa l t) 
                       && mapaValido2 (Mapa l t)
                       && mapaValido3 (Mapa l t)
                       && mapaValido3excecao (Mapa l t)
                       && mapaValido4 (Mapa l t)
                       && mapaValido4excecao (Mapa l t)
                       && mapaValido5 (Mapa l t)
                       && mapaValido5especial (Mapa l t)
                       && mapaValido6 (Mapa l t)
                       && mapaValido7 (Mapa l t)
                       && mapaValido8 l t
                     


{- | A função "mapaValido1" garante que não existem obstáculos em terrenos impróprios, e.g. troncos em
estradas ou relvas, árvores em rios ou estradas, etc.
-}
mapaValido1 :: Mapa -> Bool 
mapaValido1 (Mapa l ((Relva, (a:[])) : [])) = a == Arvore || a == Nenhum 
mapaValido1 (Mapa l ((Relva, (a:[])) : t)) = (a == Arvore || a == Nenhum) && mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Relva, (a:as)) : t)) = (a == Arvore || a == Nenhum) && mapaValido1 (Mapa l ((Relva, as):t))

mapaValido1 (Mapa l ((Rio x, (m:[])) : [])) = m == Tronco || m == Nenhum 
mapaValido1 (Mapa l ((Rio x, (m:[])) : t)) = (m == Tronco || m == Nenhum) && mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Rio x, (m:ms)) : t)) = (m == Tronco || m == Nenhum) && mapaValido1 (Mapa l ((Rio x, ms):t))

mapaValido1 (Mapa l ((Estrada y, (c:[])) : [])) = c == Carro || c == Nenhum 
mapaValido1 (Mapa l ((Estrada y, (c:[])) : t)) = (c == Carro || c == Nenhum) && mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Estrada y, (c:cs)) : t)) = (c == Carro || c == Nenhum) && mapaValido1 (Mapa l ((Estrada y, cs):t))

{- | A função "mapaValido2" garante que rios contíguos têm direções opostas.
-}
mapaValido2 :: Mapa -> Bool 
mapaValido2 (Mapa l (h:[])) = True 
mapaValido2 (Mapa l ((Rio x, l1) : (Rio y, l2) : t)) = ((x > 0 && y < 0) || (x < 0 && y > 0)) 
                                                       && mapaValido2 (Mapa l ((Rio y, l2) : t))
mapaValido2 (Mapa l ((_, _) : t)) = mapaValido2 (Mapa l t)

{- | A função "mapaValido3" garante que troncos têm, no máximo, 5 unidades de comprimento.
-} 
mapaValido3 :: Mapa -> Bool
mapaValido3 (Mapa l ((Rio x, (m:ms)) : [])) = mapaValido3aux (m:ms) 0
mapaValido3 (Mapa l ((Rio x, (m:ms)) : t)) = mapaValido3aux  (m:ms) 0 && mapaValido3 (Mapa l t)
mapaValido3 (Mapa l (_ : t)) = mapaValido3  (Mapa l t)
mapaValido3 (Mapa l []) = True

{- | A função "mapaValido3aux" é uma função necessária para alcançar o propósito da função "mapaValido3". 
-}
mapaValido3aux :: [Obstaculo] -> Int -> Bool
mapaValido3aux [] y = y <= 5
mapaValido3aux (m:ms) y = if y>5 then False else if m == Tronco then mapaValido3aux ms (y+1)
                                                               else mapaValido3aux ms 0

{- | Tal como o nome indica, a função "mapaValido3excecao" não é mais do que uma exceção, exceção essa que tem em conta não 
só a regra imposta pela função "mapaValido3", como também o metódo funcional do jogo a um nível futuro, garantindo assim que
não ocorreram erros inesperados.
-}
mapaValido3excecao :: Mapa -> Bool 
mapaValido3excecao (Mapa 7 ((Rio x, [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco]):t)) = False
mapaValido3excecao (Mapa 8 ((Rio x, [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco]):t)) = False
mapaValido3excecao (Mapa 9 ((Rio x, [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco]):t)) = False
mapaValido3excecao (Mapa 10 ((Rio x, [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]):t)) = False
mapaValido3excecao (Mapa 11 ((Rio x, [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]):t)) = False
mapaValido3excecao (Mapa 7 ((Rio x, [Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]):t)) = False
mapaValido3excecao (Mapa 8 ((Rio x, [Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]):t)) = False
mapaValido3excecao (Mapa 9 ((Rio x, [Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]):t)) = False
mapaValido3excecao (Mapa 10 ((Rio x, [Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]):t)) = False
mapaValido3excecao (Mapa _ _) = True

{- | A função "mapaValido4" garante que carros têm, no máximo, 3 unidades de comprimento.
-}
mapaValido4 :: Mapa -> Bool
mapaValido4 (Mapa l ((Estrada z, (c:cs)) : [])) = mapaValido4aux (c:cs) 0
mapaValido4 (Mapa l ((Estrada z, (c:cs)) : t)) = mapaValido4aux  (c:cs) 0 && mapaValido4 (Mapa l t)
mapaValido4 (Mapa l (_ : t)) = mapaValido4  (Mapa l t)
mapaValido4 (Mapa l []) = True

{-| A função "mapaValido4aux" é uma função necessária para alcançar o propósito da função "mapaValido4".
-}
mapaValido4aux :: [Obstaculo] -> Int -> Bool
mapaValido4aux [] y = y <= 3
mapaValido4aux (m:ms) y = if y>3 then False else if m == Carro then mapaValido4aux ms (y+1)
                                                               else mapaValido4aux ms 0

{- | À semelhança da "mapaValido3excecao", a função "mapaValido4excecao tem também em conta fases futuras
da construção do nosso jogo, revela-se por isso necessária quando tendo em conta a regra imposta pela 
função "mapaValido4".
-}
mapaValido4excecao :: Mapa -> Bool 
mapaValido4excecao (Mapa 5 ((Estrada x, [Carro,Carro,Carro,Nenhum,Carro]):t)) = False
mapaValido4excecao (Mapa 6 ((Estrada x, [Carro,Carro,Carro,Nenhum,Carro,Carro]):t)) = False
mapaValido4excecao (Mapa 7 ((Estrada x, [Carro,Carro,Carro,Nenhum,Carro,Carro,Carro]):t)) = False
mapaValido4excecao (Mapa 5 ((Estrada x, [Carro,Nenhum,Carro,Carro,Carro]):t)) = False
mapaValido4excecao (Mapa 6 ((Estrada x, [Carro,Carro,Nenhum,Carro,Carro,Carro]):t)) = False
mapaValido4excecao (Mapa _ _) = True 


{- | A função "mapaValido5" garante que em qualquer linha existe, no mínimo, um "obstáculo" Nenhum. Ou
seja, uma linha não pode ser composta exclusivamente por obstáculos, precisando de haver pelo menos um espaço livre.
-}
mapaValido5 :: Mapa -> Bool
mapaValido5 (Mapa l (h:[])) = elem Nenhum (obs)
            where (obs) = snd h
mapaValido5 (Mapa l (h:t)) = elem Nenhum (obs) && mapaValido5 (Mapa l t) 
            where (obs) = snd h 

{- | A função "mapaValido5especial" foi criada com base no raciocínio conceptual abordado na função "mapaValido5", 
assim como tem de existir no mínimo um "obstáculo" Nenhum para a existência de espaços livres, tem também de existir, 
pelo menos um "obstáculo" Tronco quando nos encontramos no Terreno "Rio" para que o jogador consiga atravessar esse
mesmo rio.
-}
mapaValido5especial :: Mapa -> Bool 
mapaValido5especial (Mapa l [(Rio x, obs)]) = elem Tronco obs
mapaValido5especial (Mapa l ((Rio x, obs):t)) = elem Tronco obs && mapaValido5especial (Mapa l t)
mapaValido5especial (Mapa l (_:[])) = True  
mapaValido5especial (Mapa l (_:t)) = mapaValido5especial (Mapa l t)   

{- | A função "mapaValido6" garante que o comprimento da lista de obstáculos de cada linha corresponde 
exatamente à largura do mapa.
-}
mapaValido6 :: Mapa -> Bool 
mapaValido6 (Mapa l (h:[])) = l == length (obs) 
                   where (obs) = snd h
mapaValido6 (Mapa l (h:t)) = if l == length (obs) then mapaValido6 (Mapa l t) else False  
                       where (obs) = snd h
{- | A função "mapaValido7" garante que contiguamente, não devem existir mais do que 4 rios, nem 5 estradas
ou relvas.
-}
mapaValido7 :: Mapa -> Bool 
mapaValido7 (Mapa l []) = True 
mapaValido7 (Mapa l ((Relva, l1) : (Relva, l2) : (Relva, l3) : (Relva, l4) : (Relva, l5) : (Relva, l6) : t)) = False
mapaValido7 (Mapa l ((Rio x, l1) : (Rio y, l2) : (Rio z, l3) : (Rio w, l4) : (Rio k, l5) : t)) = False
mapaValido7 (Mapa l ((Estrada x, l1) : (Estrada y, l2) : (Estrada z, l3) : (Estrada w, l4) : (Estrada k, l5) : (Estrada c, l6) : t)) = False
mapaValido7 (Mapa l ((q,qs) : t)) = mapaValido7 (Mapa l t)


{- | A função "mapaValido8" limita o número de obstáculos numa linha para evitar linhas seguidas com vários obstáculos a obstruir o caminho.
-}
mapaValido8 :: Largura -> [(Terreno, [Obstaculo])] -> Bool
mapaValido8 k t = mapaValido8' t
            where mapaValido8' [] =True
                  mapaValido8' ((_,obs):ts) = contaObs obs <= 4 && mapaValido8' ts
                  contaObs obs = length [o | o <- obs, o/= Nenhum]