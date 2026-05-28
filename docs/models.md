# Modelos de Dominio — Diseño y decisiones técnicas

Este documento explica las decisiones detrás de `Domain.Model.{Player, Enemy, World}`
y los nuevos value objects `Domain.ValueObjects.{DeltaTime, Input}`.
Es un complemento a los comentarios inline de cada módulo.

---

## Entidades vs Value Objects

El proyecto distingue dos categorías de tipos en `Domain/`:

| Categoría | Ejemplos | Igualdad | ¿Identidad propia? |
|-----------|----------|----------|--------------------|
| **Value Object** | `Position`, `Velocity`, `DeltaTime`, `Input` | Por valor (mismas coordenadas = misma posición) | No |
| **Entity** | `Player`, `Enemy` | Por identidad (dos enemigos en la misma posición son distintos) | Sí |

En Haskell ambas se implementan como valores inmutables — no hay mutación en `Domain/`.
La diferencia es conceptual: una entidad "persiste" a través del tiempo; un frame nuevo
tiene un `Player` actualizado que *representa* al mismo jugador, aunque sea un valor nuevo.

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

Two value objects con los mismos datos son indistinguibles:

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

```
M2: World = Player + [Enemy]          (este milestone)
M3: World = Player + [Enemy] + [Platform]   (geometría del nivel)
M8: World + carga desde JSON (Aeson)
```

`World` crece gradualmente. El alias `type GameState = World` en `UseCases.GameMonad`
hace que el `StateT` de la pila monádica maneje el tipo correcto desde M2.

---

## El placeholder cinemático `advance` y su relación con `step`

En M2, `advance :: DeltaTime -> World -> World` integra `pos += vel * dt` sin
gravedad ni colisiones. Es un **placeholder explícito** del futuro
`step :: DeltaTime -> Input -> World -> World` de `Domain.Logic.Physics` (M3).

La diferencia de firmas:

```haskell
-- M2: avance puro sin input (el input lo procesa UpdateGame antes de llamar advance)
advance :: DeltaTime -> World -> World

-- M3: step con input y física completa (reemplaza advance en UpdateGame)
step :: DeltaTime -> Input -> World -> World
```

La separación actual entre `applyInput` (ajusta velocidades según input) y `advance`
(integra posiciones) anticipa la estructura de M3: `step` hará ambas cosas en el dominio puro.

---

## Relación entre capas para el update de un frame

```
Input del usuario (teclado/gamepad)
        ↓  [Adapters/Input — M7]
Domain.ValueObjects.Input
        ↓
UseCases.UpdateGame.updateGame :: DeltaTime -> Input -> GameM ()
   │
   ├── asks gcMoveSpeed           ← lee config (MonadReader)
   ├── modify (mapPlayer applyInput)   ← ajusta vel del player (MonadState)
   └── modify (advance dt)             ← integra posiciones (MonadState)
        ↓
Domain.Model.World (estado actualizado)
        ↓  [Adapters/Rendering — M8]
Pantalla
```

En M3, `modify (advance dt)` se reemplaza por `modify (step dt input)` donde `step`
incorpora gravedad y colisiones desde `Domain.Logic.Physics`.
