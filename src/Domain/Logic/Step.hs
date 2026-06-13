-- | Transición pura de un frame: orquesta behaviour, física y colisiones.
module Domain.Logic.Step (
  advanceFrame,
  step,
)
where

import Domain.Logic.Collision (resolvePlayerPlatforms)
import Domain.Logic.Health (resolveLifeLoss)
import Domain.Logic.Physics (
  applyGravity,
  applyHorizontalInput,
  applyJump,
  integrateEnemies,
  integratePlayer,
 )
import Domain.Logic.RunBehaviour (runBehaviourStep)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Platform (Platform, platformHeight)
import Domain.Model.Player (Player (..), playerVel)
import Domain.Model.World (World (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime, seconds)
import Domain.ValueObjects.Input (Input)
import Domain.ValueObjects.PhysicsParams (PhysicsParams)
import Domain.ValueObjects.Velocity (velY)

{- | Transición completa de un frame: behaviour step y luego física, o identidad si @dt = 0@.

Única declaración de la política de "frame congelado" del motor: con @dt = 0@ no
avanza ninguna fase (ni behaviour ni física). Si @dt > 0@, primero el DSL fija la
velocidad de los enemigos ('runBehaviourStep') y después 'step' integra física y
colisiones. 'UseCases.UpdateGame.updateGame' sólo eleva esta función a 'GameM'.
-}
advanceFrame :: PhysicsParams -> DeltaTime -> Input -> World -> World
advanceFrame params dt input w
  | seconds dt == 0 = w
  | worldPhase w == GameOver = w
  | otherwise =
      resolveLifeLoss (step params dt input (runBehaviourStep w))

{- | Avanza la física del mundo un frame: input → gravedad → salto → integración → colisiones.

Con @dt = 0@ devuelve el mundo sin cambios: es la identidad temporal /propia/ de
'step' como función pura (verificada en 'Domain.StepTest'). La política a nivel de
frame la posee 'advanceFrame'; esta guarda protege a 'step' cuando se la llama aislada.

Los enemigos reciben cinemática (@pos += vel * dt@); la velocidad la fija el DSL
(M6) antes de este paso. Sin gravedad ni colisiones enemigo–plataforma en M6.
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
   in runSubsteps n (deltaTimeSub dt n) plats vyBefore p

{- | Aplica integración + colisión @n@ veces de forma /estricta/.

POR QUÉ recursión explícita con @seq@ y no @iterate f p !! n@ o @foldl@ sobre
@[1..n]@: ambas alternativas construyen una lista intermedia y dejan @p@ como una
cadena de thunks anidados (@f (f (f … p))@) que sólo se fuerza al final. Con @n@
grande (sólidos finos / velocidad alta) eso aloca de más y arriesga acumular
thunks. @seq@ fuerza cada @Player@ a WHNF antes de la siguiente iteración, así el
estado se reduce paso a paso sin lista ni cadena de thunks.
-}
runSubsteps :: Int -> DeltaTime -> [Platform] -> Float -> Player -> Player
runSubsteps n dtSub plats vyBefore p
  | n <= 0 = p
  | otherwise =
      let p' = resolvePlayerPlatforms plats vyBefore (integratePlayer dtSub p)
       in p' `seq` runSubsteps (n - 1) dtSub plats vyBefore p'

{- | Tope de sub-pasos por frame: cota dura del trabajo aun con sólidos
  extremadamente finos o velocidades muy altas (evita @n@ patológico).
-}
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
      maxStep = max minSubstep (minPlatformHeight plats * 0.5)
      raw = ceiling (dy / maxStep) :: Int
   in clampSubsteps raw

deltaTimeSub :: DeltaTime -> Int -> DeltaTime
deltaTimeSub dt n =
  deltaTime (seconds dt / fromIntegral (max 1 n))

clampSubsteps :: Int -> Int
clampSubsteps n = max 1 (min maxSubsteps n)

-- | Sin plataformas, un paso grueso evita subdividir en exceso.
minPlatformHeight :: [Platform] -> Float
minPlatformHeight [] = 1e6
minPlatformHeight plats = minimum (platformHeight <$> plats)
