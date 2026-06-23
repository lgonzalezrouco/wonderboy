{- | Programas de comportamiento compuestos (catálogo DSL).

Construidos con los primitivos de @Domain.Model.EntityBehaviour@.
-}
module Domain.Logic.EntityBehaviours (
  patrolHorizontal,
  reactiveFsm,
  flyingReactiveFsm,
  airPatrolFacePlayer,
  archerProgram,
  defaultProgramForKind,
  programForArchetype,
  motionForArchetype,
)
where

import Data.Function (fix)

import Domain.Model.EnemyKind (
  EnemyKind,
  EnemyKindStats (eksMotion),
  EnemyMotionStats (..),
  enemyKindStats,
  isFlyingKind,
 )
import Domain.Model.EntityBehaviour (
  BehaviourProgram,
  facePlayer,
  idleProgram,
  ifNearSpawn,
  ifPlayerWithinRange,
  moveToward,
  moveTowardPlayer,
  moveTowardSpawn,
  setFacingTowardPlayer,
  setVelocity,
  shoot,
  waitFrames,
  (>>>),
 )
import Domain.Model.LevelDefinition (BehaviourArchetype (..))
import Domain.ValueObjects.Frames (Frames, frames, hasFramesLeft)
import Domain.ValueObjects.Velocity (velocity)

{- | Patrulla horizontal indefinidamente: velocidad @±speed@ durante @frames + 1@
  frames por tramo. Son @frames + 1@ y no @frames@ porque 'setVelocity' consume un
  behaviour step propio (fija la velocidad) y luego 'waitFrames' la mantiene @frames@
  frames más. Sobre suelo plano, cinemática M6. Requiere @speed > 0@ y @frames > 0@.
-}
patrolHorizontal :: Float -> Frames -> BehaviourProgram
patrolHorizontal speed legFrames
  | speed > 0 && hasFramesLeft legFrames =
      fix $ \p ->
        setVelocity (velocity (-speed) 0)
          >>> waitFrames legFrames
          >>> setVelocity (velocity speed 0)
          >>> waitFrames legFrames
          >>> p
  | otherwise = idleProgram

{- | FSM reactivo: chase en rango, idle en spawn, retorno fuera de rango.

Re-evalúa sensores cada behaviour step vía @fix@.
-}
reactiveFsm ::
  Float ->
  Float ->
  Float ->
  Float ->
  BehaviourProgram
reactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius
  | chaseRange > 0 && chaseSpeed > 0 && returnSpeed > 0 && spawnRadius > 0 =
      fix $ \loop ->
        ifPlayerWithinRange
          chaseRange
          (moveTowardPlayer chaseSpeed >>> waitFrames (frames 1) >>> loop)
          ( ifNearSpawn
              spawnRadius
              (facePlayer >>> waitFrames (frames 1) >>> loop)
              (moveTowardSpawn returnSpeed >>> waitFrames (frames 1) >>> loop)
          )
  | otherwise = idleProgram

{- | FSM reactivo aéreo: persigue y regresa en horizontal; en spawn patrulla en X.

Mantiene la altitud de spawn (sin planeo vertical). Los murciélagos ignoran
colisión con plataformas en @Domain.Logic.Collision@.
-}
flyingReactiveFsm ::
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Frames ->
  BehaviourProgram
flyingReactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius patrolSpeed patrolLeg
  | chaseRange > 0
      && chaseSpeed > 0
      && returnSpeed > 0
      && spawnRadius > 0
      && patrolSpeed > 0
      && hasFramesLeft patrolLeg =
      fix $ \loop ->
        ifPlayerWithinRange
          chaseRange
          (moveToward chaseSpeed >>> waitFrames (frames 1) >>> loop)
          ( ifNearSpawn
              spawnRadius
              (airPatrolFacePlayer patrolSpeed patrolLeg >>> loop)
              (moveTowardSpawn returnSpeed >>> waitFrames (frames 1) >>> loop)
          )
  | otherwise = idleProgram

-- | Patrulla horizontal en el aire mirando al jugador (sin desplazamiento vertical).
airPatrolFacePlayer :: Float -> Frames -> BehaviourProgram
airPatrolFacePlayer patrolSpeed patrolLeg
  | patrolSpeed > 0 && hasFramesLeft patrolLeg =
      setFacingTowardPlayer
        >>> setVelocity (velocity (-patrolSpeed) 0)
        >>> waitFrames patrolLeg
        >>> setFacingTowardPlayer
        >>> setVelocity (velocity patrolSpeed 0)
        >>> waitFrames patrolLeg
  | otherwise = facePlayer

-- | Archer: dispara en rango, mira al jugador y espera cooldown entre disparos.
archerProgram :: Float -> Frames -> BehaviourProgram
archerProgram shootRange cooldown
  | shootRange > 0 && hasFramesLeft cooldown =
      fix $ \loop ->
        ifPlayerWithinRange
          shootRange
          (facePlayer >>> shoot >>> waitFrames cooldown >>> loop)
          (facePlayer >>> waitFrames (frames 1) >>> loop)
  | otherwise = idleProgram

{- | Programa de comportamiento para una variante de movimiento.

El arquetipo chase y el arquetipo guard comparten la misma máquina reactiva
('reactiveFsm'), que en su rama de spawn ya mira al jugador; hoy no se distinguen
en comportamiento (la diferencia real vive en los stats por clase).
-}
programForMotion :: EnemyMotionStats -> BehaviourProgram
programForMotion (PatrolMotion speed legFrames) = patrolHorizontal speed legFrames
programForMotion (ReactiveMotion chaseSpeed returnSpeed chaseRange spawnRadius) =
  reactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius
programForMotion (FlyingReactiveMotion chaseSpeed returnSpeed chaseRange spawnRadius patrolSpeed patrolLeg) =
  flyingReactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius patrolSpeed patrolLeg
programForMotion (ArcherMotion shootRange cooldown _ _ _ _) =
  archerProgram shootRange cooldown

-- | Programa por defecto según clase de enemigo (su arquetipo natural de movimiento).
defaultProgramForKind :: EnemyKind -> BehaviourProgram
defaultProgramForKind = programForMotion . eksMotion . enemyKindStats

{- | Programa de comportamiento para un arquetipo explícito sobre /cualquier/ clase.

A diferencia del esquema anterior —que solo "confirmaba" el movimiento natural de
la clase y caía a 'idleProgram' cuando el arquetipo no encajaba (p. ej. un caracol
con @chase@ quedaba quieto)— ahora cualquier clase puede expresar cualquier
arquetipo. El truco es no escribir un programa nuevo, sino /sintetizar/ el
'EnemyMotionStats' adecuado al arquetipo ('motionForArchetype') y reutilizar toda
la maquinaria de 'programForMotion' (los FSM ya existentes). Así no se duplica
lógica de movimiento y la diferencia entre arquetipos vive donde el propio código
decía que debía vivir: en los stats.
-}
programForArchetype :: EnemyKind -> BehaviourArchetype -> BehaviourProgram
programForArchetype kind = programForMotion . motionForArchetype kind

{- | Stats de movimiento que materializan un arquetipo sobre una clase concreta.

Deriva la /velocidad/ del repertorio natural de la clase ('baseSpeed') —para que un
golem lento siga siendo lento y un murciélago veloz siga siendo veloz— y elige
locomoción terrestre o aérea según 'isFlyingKind'. Los /rangos/ y /tramos/ son
constantes por arquetipo, y son justo lo que distingue 'ChaseArchetype' de
'GuardArchetype' (antes compartían FSM y eran indistinguibles):

  * 'PatrolArchetype': patrulla ida y vuelta sin reaccionar al jugador. Si la clase
    ya patrulla, conserva su tramo natural; si no, sintetiza uno estándar.
  * 'ChaseArchetype': detecta al jugador de lejos ('chaseDetectRange') y lo persigue,
    deambulando ('chaseRoamRadius') antes de volver al spawn anchor.
  * 'GuardArchetype': custodia su puesto — solo reacciona de cerca ('guardDetectRange')
    y vuelve enseguida al spawn anchor ('guardHoldRadius').

El arquero es la excepción: su esencia es el ataque a distancia, así que conserva su
'ArcherMotion' e ignora los arquetipos de movimiento.
-}
motionForArchetype :: EnemyKind -> BehaviourArchetype -> EnemyMotionStats
motionForArchetype kind archetype =
  case naturalMotion of
    ArcherMotion{} -> naturalMotion
    _ -> case archetype of
      PatrolArchetype -> patrolMotion
      ChaseArchetype -> reactiveMotion chaseDetectRange chaseRoamRadius
      GuardArchetype -> reactiveMotion guardDetectRange guardHoldRadius
 where
  naturalMotion = eksMotion (enemyKindStats kind)
  speed = baseSpeed naturalMotion

  -- Patrulla: preserva el tramo natural de las clases que ya patrullan; para las
  -- reactivas sintetiza un tramo estándar a su velocidad base.
  patrolMotion = case naturalMotion of
    PatrolMotion s legs -> PatrolMotion s legs
    _ -> PatrolMotion speed defaultPatrolLeg

  -- Reactivo (chase/guard): aéreo si la clase vuela, terrestre si no. El @range@
  -- (detección) y el @hold@ (radio de spawn) los fija el arquetipo que llama; la
  -- velocidad de retorno es algo menor que la de persecución.
  reactiveMotion range hold
    | isFlyingKind kind =
        FlyingReactiveMotion speed (speed * returnSpeedFactor) range hold speed homePatrolLeg
    | otherwise =
        ReactiveMotion speed (speed * returnSpeedFactor) range hold

{- | Velocidad de movimiento característica de una clase, leída de su motion natural.

El caso 'ArcherMotion' es inalcanzable en la práctica: 'motionForArchetype' corta en
@ArcherMotion@ y nunca llega a forzar @speed@ para un arquero. Se define en @0@ solo
para que 'baseSpeed' sea total bajo @-Wall@; ese @0@ no representa una velocidad real.
-}
baseSpeed :: EnemyMotionStats -> Float
baseSpeed (PatrolMotion s _) = s
baseSpeed (ReactiveMotion s _ _ _) = s
baseSpeed (FlyingReactiveMotion s _ _ _ _ _) = s
baseSpeed ArcherMotion{} = 0

-- ---------------------------------------------------------------------------
-- Constantes de los arquetipos sintetizados (píxeles lógicos y frames)
-- ---------------------------------------------------------------------------

-- | Velocidad de retorno relativa a la de persecución (vuelve un poco más lento).
returnSpeedFactor :: Float
returnSpeedFactor = 0.8

-- | Chase: rango de detección amplio — persigue desde lejos.
chaseDetectRange :: Float
chaseDetectRange = 140

-- | Chase: radio alrededor del spawn dentro del cual deambula sin volver aún.
chaseRoamRadius :: Float
chaseRoamRadius = 28

-- | Guard: rango de detección corto — solo reacciona cuando el jugador está cerca.
guardDetectRange :: Float
guardDetectRange = 60

-- | Guard: radio chico — vuelve enseguida a custodiar su puesto.
guardHoldRadius :: Float
guardHoldRadius = 10

-- | Tramo de patrulla sintetizado para clases que no traen el suyo.
defaultPatrolLeg :: Frames
defaultPatrolLeg = frames 90

-- | Tramo de patrulla aérea en la fase "en casa" de un perseguidor volador.
homePatrolLeg :: Frames
homePatrolLeg = frames 60
