import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

largura, altura, offset :: Int
largura = 600
altura = 300
offset = 100

larguraF, alturaF, offsetF :: Float
larguraF = 600
alturaF = 300
offsetF = 100

larguraBor = (larguraF - 30)
alturaBor = 10

diametroBola = 10

larguraRaq = 26
alturaRaq = 86

fps = 60

window :: Display
window = InWindow "Ping Pong" (largura, altura) (offset, offset)

background :: Color
background = black
-----------------------------------------------------------------------------------------------------------------------
-- Área referente às Posições, Velocidade e Num do Placar
data Movimento = Move { localBola :: (Float, Float), velBola :: (Float, Float), raqueteDir :: Float,
                        raqueteEsq :: Float, placarDir :: Int, placarEsq :: Int, batida :: Float, sobeEsq :: Bool,
                        desceEsq :: Bool, sobeDir :: Bool, desceDir :: Bool }
 deriving Show

estadoInicial :: Movimento
estadoInicial = Move { localBola = (-larguraF/2 + diametroBola/2 + larguraRaq, (-30)), velBola = (55, 45) ,
                       raqueteDir = 0 , raqueteEsq = 0 , placarDir = 0, placarEsq = 0, batida = 0, sobeEsq = False,
                       desceEsq = False, sobeDir = False, desceDir= False }
-----------------------------------------------------------------------------------------------------------------------
-- Área referente ao Movimento da Bola, Movimento da Raquete
moveBola :: Float -> Movimento -> Movimento
moveBola t move = move { localBola = (x1, y1) }
 where (x, y) = localBola move
       (vx, vy) = velBola move
       y1 = y + vy * t
       x1 = x + vx * t

colideBorda :: Movimento-> Movimento
colideBorda move = move { velBola = (vx,vy1) }
  where (vx, vy) = velBola move
        (x, y) = localBola move
        vy1 = if colideCima || colideBaixo then (-vy) else vy
        colideBaixo = y - diametroBola <= -alturaF/2 + alturaBor
        colideCima = y + diametroBola >= alturaF/2 - alturaBor

colideRaq :: Movimento -> Movimento
colideRaq move = muda
 where (vx, vy) = velBola move
       (x, y) = localBola move
       posEsq = raqueteEsq move
       posDir = raqueteDir move
       muda = if colideEsq then eixoYE posEsq else (if colideDir then eixoYD posDir else move)
       colideEsq = x - diametroBola/2 <= -larguraF/2 + larguraRaq/2
       colideDir = x + diametroBola/2 >= larguraF/2 - larguraRaq/2
       eixoYE pos = if colideEY pos y then move { batida = (batida move) + 1, velBola = (confereBat move) } else move
       eixoYD pos = if colideEY pos y then move { batida = (batida move) + 1, velBola = (confereBat move) } else move

colideEY :: Float -> Float -> Bool
colideEY p b = if b > p - alturaRaq/2 && b < p + alturaRaq/2 then True else False

confereBat :: Movimento -> (Float, Float)
confereBat move = if (b) `mod` 10 == 0 && b /= 0 then ( vel25 (-vx, vy)) else (-vx, vy)
 where (vx, vy) = velBola move
       b = 1 + (fromEnum (batida move))
       vel25 (x, y) = ((x * 1.25), (y * 1.25))

gol :: Movimento -> Movimento
gol move =  if golNaEsq then move { localBola = (0, 0), velBola = (55, 45), placarDir = 1 + (placarDir move), batida=0 }
            else (if golNaDir
            then move { localBola = (0, 0), velBola = (-55, 45), placarEsq = 1 + (placarEsq move), batida = 0 }
            else move { localBola = localBola move})
 where (x, y) = localBola move
       golNaEsq = x + (diametroBola/2) <= (-larguraF/2)
       golNaDir = x - (diametroBola/2) >= (larguraF/2)

colideObst :: Movimento -> Movimento
colideObst move = if (batida move) >= 2 then muda else move
 where (vx, vy) = velBola move
       (x, y) = localBola move
       raio = (diametroBola/2)
       l = (ladoObst/2)
       muda
        | x + raio <= -larguraObst = quadrEsq
        | x - raio <= larguraObst = quadrDir
        | otherwise = move
       quadrEsq = if





       {-vx1
        | x1 == True = (-vx)
        | otherwise = vx
       x1 = if x + raio <= -(larguraF/4) - l then eixoY else x2
       x2 = if x - raio >= -(larguraF/4)+ l then eixoY else x3
       x3 = if x + raio >= (larguraF/4) - l then eixoY else x4
       x4 = if x - raio <= (larguraF/4) + l then eixoY else False
       eixoY = if y1 || y2 || y3  then True else False
       y1 = y - raio <= (alturaF/4) + l && y + raio >= (alturaF/4) - l
       y2 = y - raio <= l && y + raio >= (- l)
       y3 = y - raio <= (-alturaF/4) + l && y + raio >= (-alturaF/4) - l-}

estadoRaqEsq :: Movimento -> Movimento
estadoRaqEsq move
 | (sobeEsq move) == True = if confereCima then move { raqueteEsq = (raqueteEsq move) + 2 } else move
 | (desceEsq move) == True = if confereBaixo then move { raqueteEsq = (raqueteEsq move) - 2 } else move
 | otherwise = move
 where confereCima = (raqueteEsq move) + (alturaRaq/2) <= (alturaF/2)
       confereBaixo = (raqueteEsq move) - (alturaRaq/2) >= -(alturaF/2)

estadoRaqDir :: Movimento -> Movimento
estadoRaqDir move
 | (sobeDir move) == True = if confereCima then move { raqueteDir = (raqueteDir move) + 2 } else move
 | (desceDir move) == True = if confereBaixo then move { raqueteDir = (raqueteDir move) - 2 } else move
 | otherwise = move
 where confereCima = (raqueteDir move) + (alturaRaq/2) <= (alturaF/2)
       confereBaixo = (raqueteDir move) - (alturaRaq/2) >= -(alturaF/2)


update :: Float -> Movimento -> Movimento
update t =  colideBorda . colideRaq . colideObst . gol . moveBola t . estadoRaqEsq . estadoRaqDir
--------------------------------------------------------------------------------------------------------------------
-- Área referente aos controles do Jogo
controle :: Event -> Movimento -> Movimento
controle (EventKey (Char 'r') _ _ _) move = move { localBola = (0, 0), velBola = (0, 0) }
controle (EventKey (Char 'f') _ _ _) move = if (localBola move) == (0, 0) then move { velBola = (50, 40) }
                                                                          else move { velBola = (velBola move)}
controle (EventKey (Char 'u') _ _ _) move = move { localBola = (0, 0), velBola = (0, 0) }
controle (EventKey (Char 'j') _ _ _) move = if (localBola move) == (0, 0) then move { velBola = (-50, 40) }
                                                                          else move { velBola = (velBola move)}
-- Controle raquete Esquerda
controle (EventKey (Char 'w') Down _ _) move = move { sobeEsq = True }
controle (EventKey (Char 'w') Up _ _) move = move { sobeEsq = False }
controle (EventKey (Char 's') Down _ _) move = move { desceEsq = True }
controle (EventKey (Char 's') Up _ _) move = move { desceEsq = False }
-- Controle raquete Direita
controle (EventKey (Char 'o') Down _ _) move = move { sobeDir = True }
controle (EventKey (Char 'o') Up _ _) move = move { sobeDir = False }
controle (EventKey (Char 'l') Down _ _) move = move { desceDir = True }
controle (EventKey (Char 'l') Up _ _) move = move { desceDir = False }

controle _ move = move
--------------------------------------------------------------------------------------------------------------------
-- Área referente aos Desenhos do Jogo
bolaImagem :: Picture
bolaImagem = color cor $ circleSolid diametroBola
 where cor = (dark white)

borda :: Float -> Picture
borda offset = translate 0 offset $ color corBorda $ rectangleSolid (larguraBor) (alturaBor)
 where corBorda = greyN 0.5

raquete:: Color -> Float -> Float -> Picture
raquete corExt x y = pictures [ translate (x) (y) $ color corExt $ rectangleSolid (larguraRaq) (alturaRaq),
                               translate (x) (y) $ color corInterna $ rectangleSolid (larguraRaq - 6) (alturaRaq - 6) ]
 where corInterna = light (light black)

placar :: Int -> Float -> Float -> Picture
placar n x y = scale 0.3 0.3 (translate (x) (y) $ color white $ text (show (n)))

ladoObst = 20
quadObst = rectangleSolid (ladoObst) (ladoObst)

obstImagem :: Color -> Picture
obstImagem cor = pictures [deCima, deBaixo]
 where deCima = translate 0 (alturaF/4) $ color cor $ rectangleSolid (larguraObst) (alturaObst)
       deBaixo = translate 0 (alturaF/4) $ color cor $ rectangleSolid (larguraObst) (alturaObst)

desenho :: Movimento -> Picture
desenho move = pictures [ bola, bordas, raqEsq, raqDir, placares, obstaculos  ]
 where bordas = pictures [borda (alturaF/2), borda (-alturaF/2)]
       bola = uncurry translate (localBola move) $ bolaImagem
       raqEsq = raquete green (-larguraF/2) $ raqueteEsq move
       raqDir = raquete red (larguraF/2) $ raqueteDir move
       placares = pictures [ placar (placarDir move) (100) (-50), placar (placarEsq move) (-175) (-50) ]
       obstaculos = if (batida move) >= 2 then obstImagem (greyN 0.5) else obstImagem black

main :: IO ()
main = play window background fps estadoInicial desenho controle update
