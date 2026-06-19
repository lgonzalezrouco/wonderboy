-- | Transición pura de un frame: orquesta behaviour, física y colisiones.
module Domain.Logic.Step (
  advanceFrame,
  step,
)
where

import Domain.Logic.Collision (resolveEnemyPlatforms, resolvePlayerPlatforms)
import Domain.Logic.CrumblingPlatforms (
  advanceCrumblingPlatforms,
  appendEnemySolidCrumbling,
  appendPlayerSolidCrumbling,
 )
import Domain.Logic.MovingPlatforms (
  advanceMovingPlatforms,
  allCollisionPlatforms,
  applyPrePhysicsCarry,
  mpaPlatform,
 )
import Domain.Logic.Physics (
  applyEnemyGravity,
  applyGravity,
  applyHorizontalInput,
  applyJump,
  integrateEnemy,
  integratePlayer,
 )
import Domain.Logic.RunBehaviour (runBehaviourStep)
import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EnemyKind (isFlyingKind)
import Domain.Model.Platform (Platform, platformHeight)
import Domain.Model.Player (Player, playerOnGround, playerVel)
import Domain.Model.World (World (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime, isFrozen, seconds)
import Domain.ValueObjects.Input (Input)
import Domain.ValueObjects.LifeParams (LifeParams)
import Domain.ValueObjects.PhysicsParams (PhysicsParams)
import Domain.ValueObjects.Velocity (velY)

{- | Transición completa de un frame: behaviour step y luego física, o identidad si el frame está congelado.

La política de "frame congelado" la define 'Domain.ValueObjects.DeltaTime.isFrozen' y
la aplica 'UseCases.UpdateGame.updateGame' a nivel de frame (behaviour + física + combate +
peligros). Aquí 'isFrozen' actúa como /identidad defensiva/ para llamadas aisladas: con el
frame congelado no avanza ninguna fase (ni behaviour ni física). Si avanza, primero el DSL
fija la velocidad de los enemigos ('runBehaviourStep') y después 'step' integra física y
colisiones.
-}
advanceFrame :: PhysicsParams -> LifeParams -> DeltaTime -> Input -> World -> World
advanceFrame params life dt input w
  | isFrozen dt = w
  | otherwise = step params life dt input (runBehaviourStep w)

{- | Avanza la física del mundo un frame: input → gravedad → salto → integración → colisiones.

Con el frame congelado devuelve el mundo sin cambios: es la identidad temporal /propia/ de
'step' como función pura (verificada en 'Domain.StepTest'). La política a nivel de
frame la define 'Domain.ValueObjects.DeltaTime.isFrozen'; esta guarda protege a 'step'
cuando se la llama aislada.

Los enemigos terrestres integran cinemática y resuelven colisión AABB; los
voladores (@BatKind@, @BossBatKind@) ignoran plataformas. La velocidad la fija el DSL antes de
este paso.
-}
step :: PhysicsParams -> LifeParams -> DeltaTime -> Input -> World -> World
step _ _ dt _ w | isFrozen dt = w
step params life dt input w =
  let p0 = worldPlayer w
      wCrumble = advanceCrumblingPlatforms life dt p0 w
      advances = advanceMovingPlatforms dt (worldMovingPlatforms wCrumble)
      moving = map mpaPlatform advances
      w' = wCrumble{worldMovingPlatforms = moving}
      p0carried = applyPrePhysicsCarry p0 advances
      wasOnGround = playerOnGround p0carried
      p1 = applyHorizontalInput params input p0carried
      p2 = applyGravity params dt p1
      p3 = applyJump params input wasOnGround p2
      vyAtCollide = velY (playerVel p3)
      crumbling = worldCrumblingPlatforms w'
      basePlats = allCollisionPlatforms (worldPlatforms w') moving
      playerPlats = appendPlayerSolidCrumbling basePlats crumbling
      enemyPlats = appendEnemySolidCrumbling basePlats crumbling
      p4 = integrateAndCollide dt p3 playerPlats vyAtCollide
      enemies' =
        map (integrateAndCollideEnemy params dt enemyPlats) (worldEnemies w')
   in w'
        { worldPlayer = p4
        , worldEnemies = enemies'
        }

{- | Integra y resuelve colisiones en sub-pasos para reducir túnel AABB en sólidos finos.

La física (input, gravedad, salto) corre una vez por frame; sólo cinemática y
colisión se subdividen cuando @|vy| * dt@ supera la mitad del sólido más bajo.
-}
integrateAndCollide :: DeltaTime -> Player -> [Platform] -> Float -> Player
integrateAndCollide dt p plats vyBefore =
  let n = substeps dt p plats
   in runSubsteps n (deltaTimeSub dt n) plats vyBefore p

integrateAndCollideEnemy ::
  PhysicsParams -> DeltaTime -> [Platform] -> Enemy -> Enemy
integrateAndCollideEnemy params dt plats e
  | isFlyingKind (enemyKind e) = integrateEnemy dt e
  | otherwise =
      let e1 = applyEnemyGravity params dt e
          vyBefore = velY (enemyVel e1)
          e2 = integrateEnemy dt e1
       in resolveEnemyPlatforms plats vyBefore e2

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
