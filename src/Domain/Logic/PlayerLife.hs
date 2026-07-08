module Domain.Logic.PlayerLife (
  applyDamage,
  deathLineY,
  isPlayerOutOfBounds,
  resolveHazardsAndDeath,
)
where

import Domain.Model.CrumblingPlatform (
  CrumblingPlatform,
  crumblingPlatformIsAnchored,
  crumblingPlatformPos,
 )
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.MovingPlatform (movingPlatformPos)
import Domain.Model.Platform (Platform (..))
import Domain.Model.Player (
  Player (..),
  playerHealth,
  playerPos,
  spawnPlayer,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Damage (Damage)
import Domain.ValueObjects.Health (health, isDepleted, reduceHealth)
import Domain.ValueObjects.LifeParams (LifeParams (..))
import Domain.ValueObjects.Lives (Lives, livesCount, loseLife, noLives)
import Domain.ValueObjects.Position (Position, posY)

applyDamage :: Damage -> Player -> Player
applyDamage amount p =
  p{playerHealth = reduceHealth amount (playerHealth p)}

lowestPlatformBottomY :: World -> Float
lowestPlatformBottomY w =
  let ys =
        map (posY . platformPos) (worldPlatforms w)
          ++ map (posY . movingPlatformPos) (worldMovingPlatforms w)
          ++ map (posY . crumblingPlatformPos) (anchoredCrumblingPlatforms w)
   in if null ys then 0 else minimum ys

-- Solo las plataformas todavía ancladas cuentan para la línea de muerte. Una que cae la arrastraría hacia abajo.
anchoredCrumblingPlatforms :: World -> [CrumblingPlatform]
anchoredCrumblingPlatforms w =
  filter crumblingPlatformIsAnchored (worldCrumblingPlatforms w)

deathLineY :: LifeParams -> World -> Float
deathLineY lp w = lowestPlatformBottomY w - lpDeathMargin lp

isPlayerOutOfBounds :: LifeParams -> World -> Bool
isPlayerOutOfBounds lp w =
  posY (playerPos (worldPlayer w)) < deathLineY lp w

respawnPlayerAt :: LifeParams -> Position -> World -> World
respawnPlayerAt lp spawn w =
  let p = spawnPlayer (lpMaxHealth lp) spawn
   in w
        { worldPlayer =
            p{playerInvincibilityFrames = lpRespawnInvincibilityFrames lp}
        , worldProjectiles = []
        , worldBossArenaEngaged = False
        }

resolveDeath :: LifeParams -> Lives -> World -> (World, Lives, GamePhase)
resolveDeath lp lives w
  | livesCount lives > 1 =
      ( respawnPlayerAt lp (worldSpawnPoint w) w
      , loseLife lives
      , Playing
      )
  | otherwise = (w, noLives, GameOver)

resolveHazardsAndDeath ::
  LifeParams ->
  Lives ->
  GamePhase ->
  World ->
  (World, Lives, GamePhase)
resolveHazardsAndDeath lp lives phase w =
  case phase of
    Playing ->
      let w'
            | isPlayerOutOfBounds lp w =
                w{worldPlayer = (worldPlayer w){playerHealth = health 0}}
            | otherwise = w
       in if isDepleted (playerHealth (worldPlayer w'))
            then resolveDeath lp lives w'
            else (w', lives, Playing)
    _ -> (w, lives, phase)
