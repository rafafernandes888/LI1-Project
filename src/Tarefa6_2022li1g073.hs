{- |
Module      : Tarefa6_2022li1g073
Description : Movimentação do personagem e obstáculos
Copyright   : Ricardo Gomes de Sousa  <a104524 @alunos.uminho.pt>
              Pedro Miguel Araújo Gomes <a104540@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-} 

module Tarefa6_2022li1g073 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Tarefa1_2022li1g073
import Tarefa2_2022li1g073
import Tarefa3_2022li1g073
import Tarefa4_2022li1g073
import Tarefa5_2022li1g073
import LI12223

-- | Estas serão as opções disponíveis no menu inicial do jogo.
data Opcao = Jogar
            | Sair

data Personagens = Galinha
                  | Ronaldo
                  | Vaca
                  | Voltar
-- | Estas serão todas as telas que podem ser mostradas no nosso jogo.
data Menu = Opcoes Opcao
          | EscolhaJogador Personagens 
          | ModoJogo Personagens 
          | GameOver Personagens

type Images = [Picture]

type Time = Float

type Mundo = (Menu, Mundojogo, Time)

type Mundojogo = (Jogo, Jogada, Images, Float)


{- | "drawOption" é uma função que recebe uma String e retorna uma imagem. Essa imagem é um Texto com o conteúdo da String que foi passada como parâmetro.
-}
drawOption :: String -> Picture
drawOption str = Text str

{- | "janela" é uma constante que representa uma janela de exibição para o jogo. 
Ela tem o nome "CrossyRoad", mede 900x900 pixels na posição (500, 100).
Esta é uma das funções principais do jogo.
-}
janela :: Display 
janela = InWindow "CrossyRoad" (900, 900) (500, 100)

{- | "desenharmapa" é uma função que recebe um mapa e uma lista de imagens e retorna uma imagem. 
A imagem é composta pelo resultado da função "escolhelinha", que é aplicada ao mapa e à lista de imagens.
-}
desenharmapa :: Mapa -> [Picture] -> Picture
desenharmapa (Mapa l t) images = Pictures (escolhelinha l t 0 300 0 images)

{- | "desenharJogador" é uma função que recebe um jogador, a largura do mapa e uma imagem e retorna uma imagem. 
Essa imagem é a imagem do jogador "translated" para a posição correta no mapa de acordo com as coordenadas x e y do jogador. 
A largura do mapa é usada para calcular a posição correta da imagem do jogador na tela.
-}
desenharJogador1 :: Jogador -> Largura -> [Picture] -> Picture
desenharJogador1 (Jogador (x,y)) l images = (translate ((x1*(900/l1)+((900/l1)/2))-450) (((y1*100)+400))) $ galinha
                  where x1 = fromIntegral x
                        y1 = fromIntegral y
                        l1 = fromIntegral l
                        [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images 

desenharJogador2 :: Jogador -> Largura -> [Picture] -> Picture
desenharJogador2 (Jogador (x,y)) l images = (translate ((x1*(900/l1)+((900/l1)/2))-450) (((y1*100)+400))) $ ronaldo
                  where x1 = fromIntegral x
                        y1 = fromIntegral y
                        l1 = fromIntegral l
                        [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images 

desenharJogador3 :: Jogador -> Largura -> [Picture] -> Picture
desenharJogador3 (Jogador (x,y)) l images = (translate ((x1*(900/l1)+((900/l1)/2))-450) (((y1*100)+400))) $ vaca
                  where x1 = fromIntegral x
                        y1 = fromIntegral y
                        l1 = fromIntegral l
                        [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images 


{- | "desenharMundo" é uma função que recebe um mundo, que é um tipo composto por um estado, um par de um jogo e imagens, e um número. 
Ela retorna uma imagem que representa o mundo de acordo com o estado. 
Se o estado for GameOver, a imagem exibida será um fundo e um texto informando o número de pontos e segundos sobrevividos pelo jogador. 
Se o estado for Opções Jogar, a imagem exibida será um fundo com as opções "Jogar" e "Sair". 
Se o estado for Opções Sair, a imagem exibida será o mesmo que no estado anterior, mas a opção "Sair" ficará em vermelho. 
Se o estado for ModoJogo, a imagem exibida será o resultado da aplicação da função desenharmapa ao mapa e à lista de imagens, com a imagem do jogador sobreposta.
Esta é uma das funções principais do jogo.
-}
desenharMundo :: Mundo -> Picture
desenharMundo (GameOver Ronaldo, (jogo, jgd, images, b), n) = Pictures [fundo4, Translate (20) 170 $ Color white $ scale 0.14 0.14 $ Text  (show (round b)++ " golos!"), Translate (20) 140 $ Color white $ scale 0.14 0.14 $ Text  ("Mesmo assim, o Messi levou a taca!") ] 
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenharMundo (GameOver p, (jogo, jgd, images, b), n) = Pictures [fundo2, Translate (-200) 230 $ Color black $ scale 0.2 0.2 $ Text  (show (round b)++ " pontos! Sobreviveu "++show (round b) ++ " segundos!")] 
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenharMundo (Opcoes Jogar, (jogo, jgd, images, b), n) = Pictures [fundo1, Translate (-125) (-170) $ Color blue $ scale 0.7 0.7 $ drawOption "Jogar", Translate (-90) (-280) $ scale 0.7 0.7 $ drawOption "Sair"]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenharMundo (Opcoes Sair, (jogo, jgd, images,b), n) = Pictures [fundo1, Translate (-125) (-180) $ scale 0.7 0.7 $ drawOption "Jogar", Color red $ Translate (-90) (-280) $ scale 0.7 0.7 $ drawOption "Sair"]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenharMundo (EscolhaJogador Galinha, (jogo, jgd, images, b), n) = Pictures [fundo3, Translate (-340) (-95) $ Color red $ scale 0.3 0.3 $ drawOption "GALINHA", Translate (-88) (-100) $ color black $ scale 0.3 0.3 $ drawOption "RONALDO", Translate (232) (-100) $ color black $ scale 0.3 0.3 $ drawOption "VACA", Translate (-88) (-300) $ color black $ scale 0.3 0.3 $ drawOption "Voltar"]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenharMundo (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n) = Pictures [fundo3, Translate (-340) (-100) $ Color black $ scale 0.3 0.3 $ drawOption "GALINHA", Translate (-88) (-95) $ color red $ scale 0.3 0.3 $ drawOption "RONALDO", Translate (232) (-100) $ color black $ scale 0.3 0.3 $ drawOption "VACA", Translate (-88) (-300) $ color black $ scale 0.3 0.3 $ drawOption "Voltar"]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenharMundo (EscolhaJogador Vaca, (jogo, jgd, images, b), n) = Pictures [fundo3, Translate (-340) (-100) $ Color black $ scale 0.3 0.3 $ drawOption "GALINHA", Translate (-88) (-100) $ color black $ scale 0.3 0.3 $ drawOption "RONALDO", Translate (232) (-95) $ color red $ scale 0.3 0.3 $ drawOption "VACA", Translate (-88) (-300) $ color black $ scale 0.3 0.3 $ drawOption "Voltar"]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenharMundo (EscolhaJogador Voltar, (jogo, jgd, images, b), n) = Pictures [fundo3, Translate (-340) (-100) $ Color black $ scale 0.3 0.3 $ drawOption "GALINHA", Translate (-88) (-100) $ color black $ scale 0.3 0.3 $ drawOption "RONALDO", Translate (232) (-100) $ color black $ scale 0.3 0.3 $ drawOption "VACA", Translate (-88) (-295) $ color red $ scale 0.3 0.3 $ drawOption "Voltar"]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images 
desenharMundo (ModoJogo Galinha, ((Jogo (Jogador (x,y)) (Mapa l t)), jgd, images, b), n) = Pictures [desenharmapa (Mapa l t) images, desenharJogador1 (Jogador (x,y)) l images]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images 
desenharMundo (ModoJogo Ronaldo, ((Jogo (Jogador (x,y)) (Mapa l t)), jgd, images, b), n) = Pictures [desenharmapa (Mapa l t) images, desenharJogador2 (Jogador (x,y)) l images]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images                         
desenharMundo (ModoJogo Vaca, ((Jogo (Jogador (x,y)) (Mapa l t)), jgd, images,b), n) = Pictures [desenharmapa (Mapa l t) images, desenharJogador3 (Jogador (x,y)) l images]
                        where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images 

{- | "escolhelinha" é uma função recursiva que recebe a largura do mapa, uma lista de tuplas contendo um terreno e uma lista de obstáculos, um índice, uma coordenada y e outro índice. 
Ela retorna uma lista de imagens, cada uma representando uma linha do mapa. A função aplica a função "linhagrafica" a cada tupla da lista e adiciona o resultado à lista de imagens.
Quando o índice atingir o final da lista, a função retorna a lista de imagens.
-}
escolhelinha :: Largura -> [(Terreno, [Obstaculo])] -> Int -> Float -> Int -> [Picture] -> [Picture]
escolhelinha l t x y o images
   | x == ((length t)-1) = (translate 0 y (linhagrafica l ((!!) t ((length t)-1)) 0 images)) : [] 
   | otherwise = (translate 0 y (linhagrafica l ((!!) t x) 0 images)) : escolhelinha l t (x+1) (y-100) 0 images

{- | "linhagrafica" é uma função que recebe a largura do mapa, uma tupla contendo um terreno e uma lista de obstáculos e um índice, e retorna uma imagem. 
A imagem é composta pelo resultado da função "linhagrafica1", que é aplicada à tupla, e pelo resultado da função "linhagrafica2", que é aplicada à largura do mapa, à tupla e ao índice.
-}
linhagrafica :: Largura -> (Terreno, [Obstaculo]) -> Int -> [Picture] -> Picture
linhagrafica l (Estrada v, obs) o images = Pictures [linhagrafica1 (Estrada v, obs) images, Pictures (linhagrafica2 l (Estrada v, obs) 0 images)]
linhagrafica l (Rio v, obs) o images = Pictures [linhagrafica1 (Rio v, obs) images, Pictures (linhagrafica2 l (Rio v, obs) 0 images)]
linhagrafica l (Relva, obs) o images = Pictures [linhagrafica1 (Relva, obs) images, Pictures (linhagrafica2 l (Relva, obs) 0 images)]

{- | "linhagrafica1" é uma função que recebe uma tupla contendo um terreno e uma lista de obstáculos e uma lista de imagens, e retorna uma imagem. 
A imagem é composta pelas imagens dos obstáculos presentes na lista de obstáculos, sobrepostas à imagem do terreno. 
A imagem do terreno é escolhida da lista de imagens de acordo com o tipo de terreno presente na tupla.
-}
linhagrafica1 :: (Terreno, [Obstaculo]) -> [Picture] -> Picture
linhagrafica1 (Estrada v, obs) images = translate 0 100 estrada
                       where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
linhagrafica1 (Relva, obs) images = translate 0 100 relva
                       where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
linhagrafica1 (Rio v, obs) images = translate 0 100 rio
                       where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images

{- | "linhagrafica2" é uma função recursiva que recebe a largura do mapa, uma tupla contendo um terreno e uma lista de obstáculos, um índice e uma lista de imagens, e retorna uma lista de imagens. 
A lista é composta pelas imagens dos obstáculos presentes na lista de obstáculos, que são posicionadas de acordo com a largura do mapa e o índice. 
Quando o índice atingir o final da lista de obstáculos, a função retorna a lista de imagens.
-}
linhagrafica2 :: Largura -> (Terreno, [Obstaculo]) -> Int -> [Picture] -> [Picture]
linhagrafica2 l (Estrada v, obs) o images
          | (l-1) == o = if (!!) obs o == Carro then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Carro images)) : [] 
                                                else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Nenhum images)) : []
          | otherwise = if (!!) obs o == Carro then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Carro images)) : linhagrafica2 l (Estrada v, obs) (o+1) images
                                               else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Nenhum images)) : linhagrafica2 l (Estrada v, obs) (o+1) images
               where l1 = fromIntegral l
                     o1 = fromIntegral o

linhagrafica2 l (Relva, obs) o images
          | (l-1) == o = if (!!) obs o == Arvore then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo 0 Arvore images)) : []
                                                 else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo 0 Nenhum images)) : [] 
          | otherwise = if (!!) obs o == Arvore then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo 0 Arvore images)) : linhagrafica2 l (Relva, obs) (o+1) images
                                                else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo 0 Nenhum images)) : linhagrafica2 l (Relva, obs) (o+1) images
                 where l1 = fromIntegral l
                       o1 = fromIntegral o

linhagrafica2 l (Rio v, obs) o images
          | (l-1) == o = if (!!) obs o == Tronco then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Tronco images)) : [] 
                                                 else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Nenhum images)) : []
          | otherwise = if (!!) obs o == Tronco then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Tronco images)) : linhagrafica2 l (Rio v, obs) (o+1) images
                                                else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Nenhum images)) : linhagrafica2 l (Rio v, obs) (o+1) images
                 where l1 = fromIntegral l
                       o1 = fromIntegral o 

{- | A função "desenhaObstaculo" é uma função que recebe uma velocidade, um obstáculo e uma lista de imagens e retorna uma imagem. 
A imagem retornada é escolhida da lista de imagens de acordo com o tipo de obstáculo passado como parâmetro. 
Se o obstáculo for Nenhum, a função retorna a imagem Blank.
-}
desenhaobstaculo :: Velocidade -> Obstaculo -> [Picture] -> Picture
desenhaobstaculo v Nenhum images = Blank 
desenhaobstaculo v Arvore images = arvore
                where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenhaobstaculo v Tronco images = tronco
                where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images
desenhaobstaculo v Carro images 
     | v >= 0 = carrop
     | otherwise = mota
                where [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca] = images

{- | A função "eventmove" é uma função que recebe um evento, um mundo e retorna um novo mundo. 
Ela verifica o tipo do evento e atualiza o estado ou as coordenadas do jogador de acordo. Se o evento for um movimento, ela atualiza as coordenadas do jogador usando a função move e retorna o novo mundo. 
Se o evento for uma tecla de entrada, ela altera o estado do mundo de acordo. Se o evento for qualquer outro, ela retorna o mundo original.
Esta é uma das funções principais do jogo.
-}
eventmove :: Event -> Mundo -> Mundo 
eventmove (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, n) = (Opcoes Sair, jogo, n)
eventmove (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, n) = (Opcoes Sair, jogo, n)
eventmove (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, n) = (Opcoes Jogar, jogo, n)
eventmove (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo, n) = (Opcoes Jogar, jogo, n)
eventmove (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, (jogo, jgd, images, b), n) = (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, n) = error "Saiu do Jogo"
eventmove (EventKey (SpecialKey KeyLeft) Down _ _ ) (EscolhaJogador Vaca, (jogo, jgd, images, b), n) =  (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyRight) Down _ _ ) (EscolhaJogador Vaca, (jogo, jgd, images, b), n) =  (EscolhaJogador Galinha, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyLeft) Down _ _ ) (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n) =  (EscolhaJogador Galinha, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyRight) Down _ _ ) (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n) =  (EscolhaJogador Vaca, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyLeft) Down _ _ ) (EscolhaJogador Galinha, (jogo, jgd, images, b), n) =  (EscolhaJogador Vaca, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyRight) Down _ _ ) (EscolhaJogador Galinha, (jogo, jgd, images, b), n) =  (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyDown) Down _ _ ) (EscolhaJogador Vaca, (jogo, jgd, images, b), n) =  (EscolhaJogador Voltar, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyDown) Down _ _ ) (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n) =  (EscolhaJogador Voltar, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyDown) Down _ _ ) (EscolhaJogador Galinha, (jogo, jgd, images, b), n) = (EscolhaJogador Voltar, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyUp) Down _ _ ) (EscolhaJogador Voltar, (jogo, jgd, images, b), n) = (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyEnter) Down _ _) (EscolhaJogador Ronaldo, (jogo, jgd, images, b), n) = (ModoJogo Ronaldo, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyEnter) Down _ _) (EscolhaJogador Vaca, (jogo, jgd, images, b), n) = (ModoJogo Vaca, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyEnter) Down _ _) (EscolhaJogador Galinha, (jogo, jgd, images, b), n) = (ModoJogo Galinha, (jogo, jgd, images, b), n)
eventmove (EventKey (SpecialKey KeyEnter) Down _ _) (EscolhaJogador Voltar, (jogo, jgd, images, b), n) = (Opcoes Jogar, (jogo, jgd, images, b), n)

eventmove (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo p, ((Jogo (Jogador (x,y)) (Mapa l t)), jgd, images, b), n) = (ModoJogo p, ((Jogo ((move (Jogo (Jogador (x,y)) (Mapa l t))) jgd) (Mapa l t)), jgd, images,b), n)
                                                                                               where jgd = (Move Direita)
eventmove (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo p, ((Jogo (Jogador (x,y)) (Mapa l t)), jgd, images, b), n) = (ModoJogo p, ((Jogo ((move (Jogo (Jogador (x,y)) (Mapa l t))) jgd) (Mapa l t)), jgd, images,b), n)
                                                                                               where jgd = (Move Esquerda)
eventmove (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo p, ((Jogo (Jogador (x,y)) (Mapa l t)), jgd, images, b), n) = (ModoJogo p, ((Jogo ((move (Jogo (Jogador (x,y)) (Mapa l t))) jgd) (Mapa l t)), jgd, images,b), n)
                                                                                               where jgd = (Move Cima)
eventmove (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo p, ((Jogo (Jogador (x,y)) (Mapa l t)), jgd, images,b), n) = (ModoJogo p, ((Jogo ((move (Jogo (Jogador (x,y)) (Mapa l t))) jgd) (Mapa l t)), jgd, images,b), n)
                                                                                               where jgd = (Move Baixo)
eventmove (_) (ModoJogo p, ((Jogo (Jogador (x,y)) (Mapa l t)), jgd, images,b), n) = (ModoJogo p, ((Jogo ((move (Jogo (Jogador (x,y)) (Mapa l t))) jgd) (Mapa l t)), jgd, images,b), n) 
                                                   where jgd = (Parado)
eventmove (EventKey (SpecialKey KeyEnter) Down _ _) (GameOver p, (jogo, jgd, images,b), n) = (Opcoes Jogar, jogoi, 0)
                                           where jogoi = ((Jogo (Jogador (2,(-7))) (Mapa 9 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]), 
                                                                                            (Estrada 1, [Carro, Nenhum, Nenhum, Nenhum, Carro, Nenhum, Nenhum, Carro, Nenhum]), 
                                                                                            (Rio (-1), [Tronco, Nenhum, Nenhum, Nenhum, Tronco, Tronco, Tronco, Nenhum, Nenhum]), 
                                                                                            (Rio 1, [Tronco, Nenhum, Nenhum, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Tronco]), 
                                                                                            (Relva, [Arvore, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]), 
                                                                                            (Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]), 
                                                                                            (Estrada 1, [Carro, Carro, Nenhum, Nenhum, Nenhum, Carro, Nenhum, Nenhum, Nenhum]), 
                                                                                            (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]), 
                                                                                            (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum])])), (Parado), images, 0)
eventmove _ m = m 

{- |  "fr" será o que determinará a "velocidade" de atualização do nosso jogo, isto é, a frequência das dinâmicas do jogo.
Esta é uma das funções principais do jogo.
-}
fr :: Int
fr = 10

{- | "mundoinicial" define-se como o estado incial do jogo.
Esta é uma das funções principais do jogo.
-}
mundoinicial :: [Picture] -> Mundo
mundoinicial images = (Opcoes Jogar, ((Jogo (Jogador (2,(-7))) (Mapa 9 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]), 
                                                (Estrada 1, [Carro, Nenhum, Nenhum, Nenhum, Carro, Nenhum, Nenhum, Carro, Nenhum]), 
                                                (Rio (-1), [Tronco, Nenhum, Nenhum, Nenhum, Tronco, Tronco, Tronco, Nenhum, Nenhum]), 
                                                (Rio 1, [Tronco, Nenhum, Nenhum, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Tronco]), 
                                                (Relva, [Arvore, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]), 
                                                (Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]), 
                                                (Estrada 1, [Carro, Carro, Nenhum, Nenhum, Nenhum, Carro, Nenhum, Nenhum, Nenhum]), 
                                                (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]), 
                                                (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum])])), (Parado), images, 0), 0)

{- | "aumenta" proporciona aleatoriadade à geração do mapa.
-}
aumenta :: Int -> Int -> Int
aumenta x y = (x*y)

{- | "time" é a função base por trás do sistema de pontos do jogo.
-}
time :: Float -> Mundo -> Mundo
time t (GameOver p, m, n) = (GameOver p, m, n)
time t (o, m, n) = (o, m, n+1)

{- | "reageTempo" é o "motor" do jogo, a função que mantém o jogo a atualizar ao longo do tempo.
Esta é uma das funções principais do jogo.
-}
reageTempo :: Float -> Mundo -> Mundo 
reageTempo n (ModoJogo p, ((Jogo (Jogador (x,y)) m), jgd, images, b), t)
                             | mod milissecondsPassed 1000<10 = if jogoTerminou (Jogo (Jogador (x,y)) m) 
                                                                then time t (GameOver p, ((Jogo (Jogador (x,y)) m), jgd, images, b), t)
                                                                else time t (ModoJogo p, ((animaJogo (deslizaJogo (t1*x*y) (Jogo (Jogador (x, y-1)) m)) jgd), jgd, images, b+n), t)
                             | otherwise = if jogoTerminou (Jogo (Jogador (x,y)) m) 
                                           then time t (GameOver p, ((Jogo (Jogador (x,y)) m), jgd, images, b), t)
                                           else time t (ModoJogo p, ((Jogo (Jogador (x,y)) m), jgd, images, b+n), t)
                                    where t1 = round t
                                          milissecondsPassed = round ((b+n)*1000)

                                        
reageTempo _ m = m

