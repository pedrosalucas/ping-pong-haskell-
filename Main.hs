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

bolaImagem :: Picture
bolaImagem = color cor $ circleSolid diametroBola
 where cor = (dark white)

borda :: Float -> Picture
borda offset = translate 0 offset $ color corBorda $ rectangleSolid (larguraBor) (alturaBor)
 where corBorda = greyN 0.5

data Movimento = Move { localBola :: (Float, Float), velBola :: (Float, Float),
                        raqueteDir :: Float, raqueteEsq :: Float}
 deriving Show

estadoInicial :: Movimento
estadoInicial = Move { localBola = (-larguraF/2 + diametroBola/2 + larguraRaq, (-30)),
                       velBola = (55, 45) , raqueteDir = 0 , raqueteEsq = 0 }
-----------------------------------------------------------------------------------------------------------------------
moveBola :: Float -> Movimento -> Movimento
moveBola t move = move { localBola = (x1, y1) }
 where (x, y) = localBola move
       (vx, vy) = velBola move
       x1 = x + vx * t
       y1 = y + vy * t

colideBorda :: Movimento-> Movimento
colideBorda move = move { velBola = (vx,vy1) }
  where (vx, vy) = velBola move
        (x, y) = localBola move
        vy1 = if colideCima || colideBaixo then (-vy) else vy
        colideBaixo = y - diametroBola <= -alturaF/2 + alturaBor
        colideCima = y + diametroBola >= alturaF/2 - alturaBor

colideRaq :: Movimento -> Movimento
colideRaq move = move { velBola = (vx1, vy) }
 where (vx, vy) = velBola move
       (x, y) = localBola move
       posEsq = raqueteEsq move
       posDir = raqueteDir move
       vx1 = if colideEsq then eixoYE posEsq else (if colideDir then eixoYD posDir else vx)
       colideEsq = x - diametroBola/2 <= -larguraF/2 + larguraRaq/2
       colideDir = x + diametroBola/2 >= larguraF/2 - larguraRaq/2
       eixoYE pos = if colideEY pos y then (-vx)+ 20 else vx
       eixoYD pos = if colideEY pos y then (-vx)- 20 else vx

colideEY :: Float -> Float -> Bool
colideEY p b = if b > p - alturaRaq/2 && b < p + alturaRaq/2 then True else False

update :: Float -> Movimento -> Movimento
update t =  colideBorda . moveBola t . colideRaq
--------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = play window background fps estadoInicial desenho controle update

raquete:: Color -> Float -> Float -> Picture
raquete corExt x y = pictures [ translate (x) (y) $ color corExt $ rectangleSolid (larguraRaq) (alturaRaq),
                               translate (x) (y) $ color corInterna $ rectangleSolid (larguraRaq - 6) (alturaRaq - 6) ]
 where corInterna = light (light black)

controle :: Event -> Movimento -> Movimento
controle (EventKey (Char 'r') _ _ _) move = move { localBola = (0, 0), velBola = (0, 0) }
controle (EventKey (Char 'f') _ _ _) move = if (localBola move) == (0, 0) then move { velBola = (-50, 40) } else move { velBola = (velBola move)}
controle (EventKey (Char 'o') Up _ _) move = if (raqueteDir move) + alturaRaq/2 <= alturaF/2
         then move { raqueteDir = 15 + (raqueteDir move) } else move { raqueteDir = (raqueteDir move) }
controle (EventKey (Char 'w') Up _ _) move = if (raqueteEsq move) + alturaRaq/2 <= alturaF/2
         then move { raqueteEsq = 15 +(raqueteEsq move) } else move { raqueteEsq = (raqueteEsq move) }
controle (EventKey (Char 's') Down _ _) move = if (raqueteEsq move) - alturaRaq/2 >= - alturaF/2
         then move { raqueteEsq = -15 +(raqueteEsq move) } else move { raqueteEsq = (raqueteEsq move) }
controle (EventKey (Char 'l') Down _ _) move = if (raqueteDir move) - alturaRaq/2 >= - alturaF/2
         then move { raqueteDir = -15 +(raqueteDir move) } else move { raqueteDir = (raqueteDir move) }
controle _ move = move


desenho :: Movimento -> Picture
desenho move = pictures [ bola, bordas, raqEsq, raqDir ]
 where bordas = pictures [borda (alturaF/2), borda (-alturaF/2)]
       bola = uncurry translate (localBola move) $ bolaImagem
       raqEsq = raquete green (-larguraF/2) $ raqueteEsq move
       raqDir = raquete red (larguraF/2) $ raqueteDir move
