module Domain.StepTest where

import Domain.Fixtures (
  ceilingPlatform,
  dtFrame,
  floorWorld,
  testLifeParams,
  testParams,
  wallPlatform,
  worldGrounded,
  worldWithCeiling,
  worldWithWall,
 )
import Domain.Logic.BehaviourCatalog (defaultProgramForKind)
import Domain.Logic.Step (step)
import Domain.Model.Enemy (enemyPos, spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (GolemKind))
import Domain.Model.Platform (platform, platformAabb)
import Domain.Model.Player (
  playerAabb,
  playerOnGround,
  playerPos,
  playerVel,
  playerWidth,
  spawnPlayer,
 )
import Domain.Model.World (World (..), defaultMaxHealth, initialWorld)
import Domain.ValueObjects.Aabb (aabbMaxX, aabbMaxY, aabbMinX, aabbMinY)
import Domain.ValueObjects.DeltaTime (deltaTime)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.PhysicsParams (ppJumpSpeed, ppMoveSpeed)
import Domain.ValueObjects.Position (posX, posY, position)
import Domain.ValueObjects.Velocity (velX, velY, velocity)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

unit_stepZeroIsIdentityAtSpawn :: Assertion
unit_stepZeroIsIdentityAtSpawn =
  step testParams testLifeParams (deltaTime 0) noInput initialWorld @?= initialWorld

unit_stepZeroIsIdentityMoving :: Assertion
unit_stepZeroIsIdentityMoving =
  let p0 = spawnPlayer defaultMaxHealth (position 10 50)
      moving =
        initialWorld
          { worldPlayer =
              p0
                { playerVel = velocity 150 (-30)
                , playerOnGround = False
                }
          }
   in step testParams testLifeParams (deltaTime 0) noInput moving @?= moving

unit_gravityMonotoneInAir :: Assertion
unit_gravityMonotoneInAir = do
  let w1 = step testParams testLifeParams dtFrame noInput initialWorld
      vy0 = velY (playerVel (worldPlayer initialWorld))
      vy1 = velY (playerVel (worldPlayer w1))
  vy1 < vy0 @?= True

unit_landingSetsOnGroundAndZeroesVy :: Assertion
unit_landingSetsOnGroundAndZeroesVy = do
  w <- worldGrounded
  let p = worldPlayer w
  assertBool "player should be on ground" (playerOnGround p)
  velY (playerVel p) @?= 0

unit_jumpImpulseMatchesPpJumpSpeed :: Assertion
unit_jumpImpulseMatchesPpJumpSpeed = do
  wGround <- worldGrounded
  posX (playerPos (worldPlayer wGround)) @?= 0
  let wJump = step testParams testLifeParams dtFrame (noInput{inputJump = True}) wGround
      vy = velY (playerVel (worldPlayer wJump))
  vy @?= ppJumpSpeed testParams

unit_jumpGatingInAir :: Assertion
unit_jumpGatingInAir = do
  let wAir =
        initialWorld
          { worldPlayer =
              (spawnPlayer defaultMaxHealth (position 0 80))
                { playerVel = velocity 0 (-100)
                , playerOnGround = False
                }
          }
      wNoJump = step testParams testLifeParams dtFrame noInput wAir
      wJump = step testParams testLifeParams dtFrame (noInput{inputJump = True}) wAir
  worldPlayer wJump @?= worldPlayer wNoJump

unit_horizontalInputLeft :: Assertion
unit_horizontalInputLeft = do
  let w = step testParams testLifeParams dtFrame (noInput{inputLeft = True}) initialWorld
  velX (playerVel (worldPlayer w)) @?= (-ppMoveSpeed testParams)

unit_horizontalInputRight :: Assertion
unit_horizontalInputRight = do
  let w = step testParams testLifeParams dtFrame (noInput{inputRight = True}) initialWorld
  velX (playerVel (worldPlayer w)) @?= ppMoveSpeed testParams

unit_ceilingBumpZeroesVy :: Assertion
unit_ceilingBumpZeroesVy = do
  let w0 = worldWithCeiling
      vyBefore = velY (playerVel (worldPlayer w0))
  assertBool "setup: player moving upward" (vyBefore > 0)
  let w1 = step testParams testLifeParams dtFrame noInput w0
      p1 = worldPlayer w1
      solid = platformAabb ceilingPlatform
  velY (playerVel p1) @?= 0
  assertBool
    "player stays below ceiling underside"
    (aabbMaxY (playerAabb p1) <= aabbMinY solid + 1e-3)

unit_wallBlockNoPenetration :: Assertion
unit_wallBlockNoPenetration = do
  let w1 = step testParams testLifeParams dtFrame (noInput{inputRight = True}) worldWithWall
      p1 = worldPlayer w1
      wallFace = aabbMinX (platformAabb wallPlatform)
      maxFootX = wallFace - playerWidth / 2
  assertBool
    "player does not pass wall inner face"
    (posX (playerPos p1) <= maxFootX + 1e-3)

{- | A downward velocity large enough to clear a thin platform in one coarse step
  (@|vy| * dt ≈ 64 px ≫ 8 px@) must still land on it: the sub-stepping prevents tunneling.
-}
unit_substepPreventsTunnelingThroughThinPlatform :: Assertion
unit_substepPreventsTunnelingThroughThinPlatform = do
  let thinPlatform = platform (position (-100) 0) 200 8 -- borde superior en y = 8
      faller =
        (spawnPlayer defaultMaxHealth (position 0 12))
          { playerVel = velocity 0 (-4000)
          , playerOnGround = False
          }
      w0 =
        initialWorld
          { worldPlayer = faller
          , worldPlatforms = [thinPlatform]
          , worldMovingPlatforms = []
          }
      p1 = worldPlayer (step testParams testLifeParams dtFrame noInput w0)
      platTop = aabbMaxY (platformAabb thinPlatform)
  assertBool "player lands on the thin platform instead of tunneling through it" (playerOnGround p1)
  assertBool
    "player foot rests on the platform top, not below it"
    (abs (posY (playerPos p1) - platTop) <= 1e-3)

-- | Un enemigo terrestre en el aire cae y apoya sobre la plataforma de abajo.
unit_groundEnemyFallsOntoPlatform :: Assertion
unit_groundEnemyFallsOntoPlatform = do
  let ledge = platform (position 0 0) 120 8
      w0 =
        floorWorld
          { worldPlatforms = [ledge]
          , worldEnemies =
              [spawnEnemy 1 GolemKind (position 60 40) (defaultProgramForKind GolemKind)]
          }
      wN = iterate (step testParams testLifeParams dtFrame noInput) w0 !! 80
      platTop = aabbMaxY (platformAabb ledge)
  case worldEnemies wN of
    e : _ ->
      assertBool "golem lands on platform top" (abs (posY (enemyPos e) - platTop) <= 1e-2)
    [] -> assertBool "expected golem to remain in world" False

{- | Saltar pegado a una pared alta no debe "teletransportar" al jugador.

QUÉ reproduce: el jugador, apoyado en el suelo y pegado a la cara de una pared
alta, salta (@inputJump@) mientras empuja contra ella (@inputRight@).

POR QUÉ: con la resolución que prioriza el eje Y, una colisión /lateral/ durante
el ascenso (@vyBefore > 0@) entra por la rama de "techo" (@bumpCeiling@) y clava al
jugador hacia abajo @cabeza − base_pared@ px, atravesando el piso (en estos
fixtures, a @y ≈ -48@). Resolviendo por el eje de menor penetración, la
penetración horizontal (~px del paso) es mucho menor que la vertical, así que se
bloquea de costado y el jugador sigue por encima del piso.
-}
unit_jumpAgainstTallWallDoesNotTeleport :: Assertion
unit_jumpAgainstTallWallDoesNotTeleport = do
  let wall = platform (position 50 0) 8 200 -- pared alta: cara izquierda en x = 50
      grounded =
        (spawnPlayer defaultMaxHealth (position 33 8)){playerOnGround = True}
      w0 =
        floorWorld
          { worldPlayer = grounded
          , worldPlatforms = worldPlatforms floorWorld ++ [wall]
          }
      input = noInput{inputJump = True, inputRight = True}
      p1 = worldPlayer (step testParams testLifeParams dtFrame input w0)
      floorTop = floorWorldTop
  assertBool
    "player must not be slammed below the floor when jumping against a wall"
    (posY (playerPos p1) >= floorTop - 1e-3)
  assertBool
    "player stays outside the wall (no horizontal penetration)"
    (aabbMaxX (playerAabb p1) <= aabbMinX (platformAabb wall) + 1e-3)

{- | Chocar de costado contra un bloque no debe "auto-subir" al jugador encima.

QUÉ reproduce: el jugador, apoyado en el suelo, corre (@inputRight@) contra la cara
de un bloque cuya cima queda por encima de sus pies.

POR QUÉ: con la resolución que prioriza el eje Y, una colisión /lateral/ con
@vyBefore ≤ 0@ y @pushUp ≤ pushDown@ entra por la rama de "aterrizar arriba"
(@landOnTop@) y sube al jugador de golpe a la cima del bloque (auto-salto).
Resolviendo por el eje de menor penetración, la penetración horizontal manda y el
jugador queda bloqueado de costado, a ras del suelo.
-}
unit_runIntoBlockSideDoesNotAutoClimb :: Assertion
unit_runIntoBlockSideDoesNotAutoClimb = do
  let block = platform (position 50 8) 32 40 -- bloque sobre el suelo: cima en y = 48
      grounded =
        (spawnPlayer defaultMaxHealth (position 33 8)){playerOnGround = True}
      w0 =
        floorWorld
          { worldPlayer = grounded
          , worldPlatforms = worldPlatforms floorWorld ++ [block]
          }
      input = noInput{inputRight = True}
      p1 = worldPlayer (step testParams testLifeParams dtFrame input w0)
      floorTop = floorWorldTop
  assertBool
    "player must not auto-climb onto the block from the side"
    (posY (playerPos p1) <= floorTop + 1e-3)
  assertBool
    "player is blocked at the block's left face"
    (aabbMaxX (playerAabb p1) <= aabbMinX (platformAabb block) + 1e-3)

-- | Cima del suelo de 'floorWorld' (su primera plataforma); 0 si no hubiera.
floorWorldTop :: Float
floorWorldTop = case worldPlatforms floorWorld of
  fp : _ -> aabbMaxY (platformAabb fp)
  [] -> 0
