# GameMonad — Diseño y decisiones técnicas

Este documento explica _por qué_ `UseCases.GameMonad` tiene la forma que tiene.
Está pensado como complemento a los comentarios inline del módulo.

---

## ¿Por qué una pila de mónadas?

En Haskell, una función pura no puede leer configuración, modificar estado ni
lanzar errores: sólo transforma entrada en salida. En el motor necesitamos las
tres cosas durante el ciclo de actualización (`UpdateGame`):

| Necesidad                                    | Solución funcional       |
| -------------------------------------------- | ------------------------ |
| Leer la gravedad, los límites del mundo      | `MonadReader GameConfig` |
| Actualizar el estado del juego por frame     | `MonadState GameState`   |
| Abortar si algo falla (nivel corrupto, etc.) | `MonadError GameError`   |

La solución es un **transformer stack**: apilamos transformadores de mónadas,
cada uno sumando un efecto, sobre una base puramente funcional (`Identity`).

---

## El orden de las capas y por qué importa

```haskell
ReaderT GameConfig
  (StateT GameState
    (ExceptT GameError
      Identity))
```

El orden afecta el tipo de retorno de `runGameM`:

```haskell
Either GameError (a, GameState)
```

Con `ExceptT` _debajo_ de `StateT`, si ocurre un error el estado del momento
del error **se pierde** (sólo devolvemos `Left err`). Si lo invirtiéramos
(`StateT` dentro de `ExceptT`), el tipo sería `(Either GameError a, GameState)`
y recuperaríamos el estado incluso ante un error.

**Elegimos el orden actual** porque para el motor de juego no tiene sentido
continuar con un estado parcial después de un error: si el nivel es inválido
o hay un bug, queremos abortar limpiamente.

`ReaderT` va siempre más afuera porque la configuración nunca cambia durante
la partida — no hay motivo para que interactúe con el manejo de errores.

---

## Por qué `newtype GameM` y no un alias de tipo

```haskell
-- ❌ type alias — GHC expande esto en mensajes de error, haciéndolos ilegibles
type GameM a = ReaderT GameConfig (StateT GameState (ExceptT GameError Identity)) a

-- ✓ newtype — tipo nominativo, mensajes de error legibles, permite instancias propias
newtype GameM a = GameM { unGameM :: ReaderT ... a }
```

Con el `newtype`, los errores del compilador dicen `GameM Int` en lugar de
`ReaderT GameConfig (StateT World (ExceptT GameError Identity)) Int`.

---

## Por qué `mtl` y no `transformers` directamente

`transformers` provee los tipos (`ReaderT`, `StateT`, `ExceptT`).
`mtl` provee las **typeclasses** (`MonadReader`, `MonadState`, `MonadError`)
y sus instancias para stacks arbitrarios.

La ventaja es que el código de negocio puede escribirse contra las typeclasses:

```haskell
-- No sabe que la mónada concreta es GameM; funciona con cualquier pila que tenga MonadState
damagePlayer :: MonadState GameState m => Int -> m ()
damagePlayer amount =
  modify $ \world ->
    let p = worldPlayer world
     in world {worldPlayer = p {playerHealth = playerHealth p - amount}}
```

Esto facilita el testing (se puede correr en un stack de tests distinto) y
hace explícitas las dependencias de cada función.

---

## Estado por Milestone

| Campo        | M1                | M2 (actual)                                    | M3+                                         |
| ------------ | ----------------- | ---------------------------------------------- | ------------------------------------------- |
| `GameState`  | `()` — vacío      | `World` con `Player` y `[Enemy]` ✓             | `+ [Platform]`                              |
| `GameConfig` | Constructor vacío | `gcGravity`, `gcMoveSpeed` + `defaultConfig` ✓ | Carga desde JSON                            |
| `GameError`  | `newtype String`  | `newtype String` (sin errores reales todavía)  | Tipo suma: `OutOfBounds`, `InvalidInput`, … |

El stack compila, `UpdateGame` corre en `GameMonad`, y `runGameM` devuelve
`Right ((), world')` con el `World` real. Ver `docs/models.md` para las
decisiones de diseño de `Player`, `Enemy`, `World`, `DeltaTime` e `Input`.
