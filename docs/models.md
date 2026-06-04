# Modelos de Dominio — Diseño y decisiones técnicas

Este documento explica las decisiones detrás de `Domain.Model.{Player, Enemy, World}`
y los nuevos value objects `Domain.ValueObjects.{DeltaTime, Input}`.
Es un complemento a los comentarios inline de cada módulo.

---

## Entidades vs Value Objects

El proyecto distingue dos categorías de tipos en `Domain/`:

| Categoría        | Ejemplos                                     | Igualdad                                                        | ¿Identidad propia? |
| ---------------- | -------------------------------------------- | --------------------------------------------------------------- | ------------------ |
| **Value Object** | `Position`, `Velocity`, `DeltaTime`, `Input` | Por valor (mismas coordenadas = misma posición)                 | No                 |
| **Entity**       | `Player`, `Enemy`                            | Por identidad (dos enemigos en la misma posición son distintos) | Sí                 |

En Haskell ambas se implementan como valores inmutables — no hay mutación en `Domain/`.
La diferencia es conceptual: una entidad "persiste" a través del tiempo; un frame nuevo
tiene un `Player` actualizado que _representa_ al mismo jugador, aunque sea un valor nuevo.

---

## Por qué records y no constructores posicionales

```haskell
-- ❌ Constructor posicional: ¿qué es el tercer campo?
data Player = Player Position Velocity Bool Int

-- ✓ Record: cada campo dice lo que representa
data Player = Player
  { playerPos      :: Position
  , playerVel      :: Velocity
  , playerOnGround :: Bool
  , playerHealth   :: Int
  }
```

Con records:

- GHC genera selectores automáticos (`playerPos :: Player -> Position`).
- Las actualizaciones son expresivas: `p { playerHealth = playerHealth p - 1 }`.
- Si se agrega un campo, el compilador advierte en todos los sitios de construcción
  donde no se especificó (con `{-# LANGUAGE RecordWildCards #-}` o GHC2021).

---

## Identidad de enemigos: por qué `enemyId`

Dos value objects con los mismos datos son indistinguibles:

```haskell
position 5 5 == position 5 5   -- True: misma posición, mismo punto
```

Pero dos enemigos en la misma posición son entidades distintas:

```haskell
Enemy { enemyId = 1, enemyPos = position 5 5, ... }
-- ≠ (conceptualmente)
Enemy { enemyId = 2, enemyPos = position 5 5, ... }
```

El `enemyId` es lo que hace que `Enemy` sea una entidad y no un value object.
En la detección de colisiones (M3) permite ignorar la colisión de una entidad consigo misma.
En el DSL (M6), las instrucciones de comportamiento referencian enemigos por id.

---

## `DeltaTime`: el único value object con invariante

A diferencia de `Position` y `Velocity`, `DeltaTime` tiene un **invariante de clase**:
el tiempo transcurrido nunca puede ser negativo.

```haskell
-- ✓ Smart constructor que garantiza dt >= 0
deltaTime :: Float -> DeltaTime
deltaTime t = DeltaTime (max 0 t)
```

Por eso el constructor de datos `DeltaTime` **no se exporta**: forzamos el uso del
smart constructor. `Position` y `Velocity` sí exportan sus constructores porque
cualquier par de `Float` es una posición/velocidad válida (no hay invariante).

---

## `Input`: record de booleanos vs ADT

```haskell
-- Alternativa considerada: ADT + lista/Set
data Action = MoveLeft | MoveRight | Jump
type Input  = [Action]   -- o Set Action

-- Elección actual: record de booleanos
data Input = Input { inputLeft, inputRight, inputJump :: Bool }
```

El record es más simple para el uso principal: pattern matching en la física.

```haskell
-- Con record:
vx' = case (inputLeft input, inputRight input) of ...

-- Con ADT + Set:
vx' = if MoveRight `Set.member` input && MoveLeft `notMember` input then speed else ...
```

El ADT tiene la ventaja de que `Set.fromList [MoveLeft, MoveLeft]` no duplica la acción
(idempotente). Pero el record de booleanos es igualmente idempotente: `True || True = True`.

---

## `World` en Milestone 2 vs Milestones siguientes

```text
M2: World = Player + [Enemy]
M3: World = Player + [Enemy] + [Platform]   (geometría del nivel — actual)
M8: World + carga desde JSON (Aeson)
```

`World` crece gradualmente. El alias `type GameState = World` en `UseCases.GameMonad`
hace que el `StateT` de la pila monádica maneje el tipo correcto desde M2.

`initialWorld` incluye un suelo de prueba y el jugador en `(0, 80)` para el demo de caída.

---

## `step` y `PhysicsParams` (Milestone 3)

La transición de frame vive en `Domain.Logic.Step`:

```haskell
step :: PhysicsParams -> DeltaTime -> Input -> World -> World
```

`PhysicsParams` (gravedad, velocidad horizontal, impulso de salto) se construye en
`UseCases` con `physicsParamsFromConfig` desde `GameConfig`, sin importar la pila
monádica desde `Domain/`.

Pipeline dentro de `step` (el orden importa):

1. Input horizontal → `vx` (`applyHorizontalInput`)
2. Gravedad sobre `vy` (`applyGravity`)
3. Salto (`applyJump`): si el jugador estaba en el suelo al inicio del frame,
   **sobrescribe** `vy` con `ppJumpSpeed`. Se aplica *después* de la gravedad,
   por eso un salto desde el suelo deja `vy` exactamente en `ppJumpSpeed`
   (es lo que verifica `Domain.StepTest`).
4. Integración de posición del jugador + colisión AABB Y-then-X contra
   `worldPlatforms`, en sub-pasos (`integrateAndCollide`)
5. Cinemática M2 de enemigos (`pos += vel * dt`, sin gravedad ni colisión)

Convenciones de hitbox: `playerPos` = centro inferior (pies); plataformas = esquina
inferior izquierda con altura hacia arriba. Ver `Domain.ValueObjects.Aabb`.

---

## Relación entre capas para el update de un frame

```text
Input del usuario (teclado/gamepad)
        ↓  [Adapters/Input — M7]
Domain.ValueObjects.Input
        ↓
UseCases.UpdateGame.updateGame :: DeltaTime -> Input -> GameM ()
   │
   ├── ask / physicsParamsFromConfig
   └── modify (step params dt input)
        ↓
Domain.Model.World (estado actualizado)
        ↓  [Adapters/Rendering — M8]
Pantalla
```
