-- | Transición pura de un frame: orquesta física y colisiones.
module Domain.Logic.Step (
  step,
)
where

import Domain.Logic.Collision (resolvePlayerPlatforms)
import Domain.Logic.Physics (
  applyGravity,
  applyHorizontalInput,
  applyJump,
  integrateEnemies,
  integratePlayer,
 )
import Domain.Model.Platform (Platform, platformHeight)
import Domain.Model.Player (Player (..), playerVel)
import Domain.Model.World (World (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime, seconds)
import Domain.ValueObjects.Input (Input)
import Domain.ValueObjects.PhysicsParams (PhysicsParams)
import Domain.ValueObjects.Velocity (velY)

{- | Avanza el mundo un frame: input → gravedad → salto → integración → colisiones.

Con @dt = 0@ devuelve el mundo sin cambios (identidad temporal).

Los enemigos sólo reciben cinemática M2 (@pos += vel * dt@); sin gravedad ni
colisiones hasta el DSL (M6).
-}
step :: PhysicsParams -> DeltaTime -> Input -> World -> World
step _ dt _ w | seconds dt == 0 = w
step params dt input w =
  let p0 = worldPlayer w
      wasOnGround = playerOnGround p0
      p1 = applyHorizontalInput params input p0
      p2 = applyGravity params dt p1
      p3 = applyJump params input wasOnGround p2
      vyAtCollide = velY (playerVel p3)
      plats = worldPlatforms w
      p4 = integrateAndCollide dt p3 plats vyAtCollide
   in w
        { worldPlayer = p4
        , worldEnemies = integrateEnemies dt (worldEnemies w)
        }

{- | Integra y resuelve colisiones en sub-pasos para reducir túnel AABB en sólidos finos.

La física (input, gravedad, salto) corre una vez por frame; sólo cinemática y
colisión se subdividen cuando @|vy| * dt@ supera la mitad del sólido más bajo.
-}
integrateAndCollide :: DeltaTime -> Player -> [Platform] -> Float -> Player
integrateAndCollide dt p plats vyBefore =
  let n = substeps dt p plats
      dtSub = deltaTimeSub dt n
   in runSubsteps n dtSub plats vyBefore p

{- | Aplica integración + colisión @n@ veces de forma /estricta/.

POR QUÉ recursión explícita con @seq@ y no @iterate f p !! n@ o @foldl@ sobre
@[1..n]@: ambas alternativas construyen una lista intermedia y dejan @p@ como una
cadena de thunks anidados (@f (f (f … p))@) que sólo se fuerza al final. Con @n@
grande (sólidos finos / velocidad alta) eso aloca de más y arriesga acumular
thunks. @seq@ fuerza cada @Player@ a WHNF antes de la siguiente iteración, así el
estado se reduce paso a paso sin lista ni cadena de thunks.
-}
runSubsteps :: Int -> DeltaTime -> [Platform] -> Float -> Player -> Player
runSubsteps n _ _ _ p | n <= 0 = p
runSubsteps n dtSub plats vyBefore p =
  let pInt = integratePlayer dtSub p
      p' = resolvePlayerPlatforms plats vyBefore pInt
   in p' `seq` runSubsteps (n - 1) dtSub plats vyBefore p'

-- | Tope de sub-pasos por frame: cota dura del trabajo aun con sólidos
--   extremadamente finos o velocidades muy altas (evita @n@ patológico).
maxSubsteps :: Int
maxSubsteps = 16

{- | Tamaño mínimo de sub-paso (px): evita @maxStep == 0@ en 'substeps'.

Si una plataforma tuviera @platformHeight <= 0@ (sólido degenerado), @minH * 0.5@
sería @0@ y @dy / 0@ daría @Infinity@/@NaN@, corrompiendo @ceiling@. Este piso
positivo garantiza una división bien definida.
-}
minSubstep :: Float
minSubstep = 1e-3

substeps :: DeltaTime -> Player -> [Platform] -> Int
substeps dt p plats =
  let t = seconds dt
      dy = abs (velY (playerVel p)) * t
      minH = minimumPlatformHeight plats
      maxStep = max minSubstep (minH * 0.5) -- nunca 0: evita ceiling de Infinity/NaN
      raw = ceiling (dy / maxStep) :: Int
   in max 1 (min maxSubsteps raw) -- acota a [1, maxSubsteps]

deltaTimeSub :: DeltaTime -> Int -> DeltaTime
deltaTimeSub dt n =
  deltaTime (seconds dt / fromIntegral (max 1 n))

minimumPlatformHeight :: [Platform] -> Float
minimumPlatformHeight [] = 1e6
minimumPlatformHeight plats = minimum (platformHeight <$> plats)
