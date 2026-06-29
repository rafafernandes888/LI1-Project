module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import LI12223
import Tarefa1_2022li1g073
import Tarefa2_2022li1g073
import Tarefa3_2022li1g073
import Tarefa4_2022li1g073
import Tarefa5_2022li1g073
import Tarefa6_2022li1g073

main :: IO ()
main = do 
          galinha1 <- loadBMP "galinha.bmp"
          relva1 <- loadBMP "relva.bmp"
          rio1 <- loadBMP "rio.bmp"
          estrada1 <- loadBMP "estrada.bmp"
          carrop1 <- loadBMP "carro.bmp"
          arvore1 <- loadBMP "arvore.bmp"
          tronco1 <- loadBMP "tronco.bmp"
          mota1 <- loadBMP "mota.bmp"
          fundo11 <- loadBMP "fundo1.bmp"
          fundo22 <- loadBMP "fundo2.bmp"
          fundo33 <- loadBMP "fundo3.bmp"
          fundo44 <- loadBMP "fundo4.bmp"
          vaca1 <- loadBMP "vaca.bmp"
          ronaldo1 <- loadBMP "ronaldo.bmp"
          let images = [galinha, relva, rio, estrada, carrop, mota, arvore, tronco, fundo1, fundo2, fundo3, fundo4, ronaldo, vaca]
              galinha = (scale 1.3 1.3 galinha1)
              relva = (scale 1 1.2 relva1)
              rio = (scale 1 1.05 rio1)
              estrada = (scale 1 1.2 estrada1)
              carrop = (scale 1.5 1.6 carrop1)
              arvore = (scale 1.1 1.3 arvore1)
              tronco = (scale 1 1.2 tronco1)
              mota = (scale 0.6 0.8 mota1)
              fundo1 = (scale 1 1 fundo11)
              fundo2 = (scale 1 1 fundo22)
              fundo3 = (scale 1 1 fundo33)
              fundo4 = (scale 1.15 1.18 fundo44)
              vaca = (scale 0.9 0.9 vaca1)
              ronaldo = (scale 0.8 0.8 ronaldo1)
          play janela 
               white
               fr
               (mundoinicial images)
               desenharMundo
               eventmove
               reageTempo
