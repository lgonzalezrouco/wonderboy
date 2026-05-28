{-# LANGUAGE DerivingStrategies #-}

-- `DerivingStrategies` permite usar `deriving stock` y `deriving newtype`
-- explícitamente (requerido por `-Wmissing-deriving-strategies`).
-- GHC2021 ya trae `GeneralisedNewtypeDeriving` (que hace posible `deriving newtype`)
-- pero no la sintaxis de estrategias.

{- | Pila monádica central del motor.

'GameM' es el contexto en el que corre toda la lógica de 'UseCases/'.
Combina tres efectos apilados sobre una base pura:

  * 'MonadReader' 'GameConfig' — acceso de solo lectura a la configuración global.
  * 'MonadState'  'GameState'  — estado mutable del juego (el 'World' en M2+).
  * 'MonadError'  'GameError'  — manejo de errores recuperables sin lanzar excepciones.

Nada en este módulo es 'IO'. Toda la impureza vive en @Adapters/@ y @Frameworks/@.
Ver @docs\/gamemonad.md@ para la justificación de cada capa y su orden.
-}
module UseCases.GameMonad (
  GameConfig (..),
  GameError (..),
  GameState,
  GameM (..),
  runGameM,
)
where

-- Grupo 1 — stdlib / base
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Generics (Generic)

-- Grupo 2 — terceros (mtl)
-- `mtl` provee transformadores de mónadas y sus typeclasses.
-- Cada import es explícito para documentar exactamente qué usamos de cada módulo.
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, runStateT)

-- ---------------------------------------------------------------------------
-- Tipos de la pila
-- ---------------------------------------------------------------------------

{- | Configuración global del juego, inmutable durante una partida.

Por ahora es un constructor vacío (nullary): no tiene campos.
En Milestone 2 agregaremos, por ejemplo, la constante de gravedad y
los límites del mundo. Usamos `data` (no `newtype`) porque no hay nada
que envolver — `newtype` requiere exactamente un campo.
-}
data GameConfig = GameConfig
  deriving stock (Eq, Show, Generic)

{- | Errores recuperables del motor.

`newtype` sobre 'String' para tener un tipo distinto a nivel de compilación.
Así el compilador rechaza pasar un `String` genérico donde se espera un `GameError`.
En Milestone 3+ crecerá a un tipo suma con constructores específicos
(ej. @OutOfBounds@, @InvalidInput@), lo que permitirá hacer pattern-matching
sobre el tipo de error en los manejadores.
-}
newtype GameError = GameError String
  deriving stock (Eq, Show, Generic)

{- | Estado mutable del juego, compartido a lo largo de una ejecución.

Actualmente es un alias de @()@ (la tupla vacía, el único valor del tipo Unit).
@()@ es el tipo de "no hay información" en Haskell: existe el slot en la pila,
pero no carga datos todavía.
En Milestone 2 este alias apuntará a @Domain.Model.World@.

Usamos `type` (alias) en lugar de `newtype` porque no necesitamos un tipo
distinto — en M2 simplemente cambiaremos el lado derecho del alias.
-}
type GameState = () -- → World en M2

-- ---------------------------------------------------------------------------
-- La mónada GameM
-- ---------------------------------------------------------------------------

{- | Mónada del motor: @ReaderT GameConfig (StateT GameState (ExceptT GameError Identity))@.

La pila se lee de afuera hacia adentro. Cada capa agrega un efecto:

@
  Identity                        ← base pura (sin efectos; no es un transformer sino el fondo)
  ExceptT GameError Identity       ← agrega manejo de errores sobre Identity
  StateT  GameState (ExceptT ...)  ← agrega estado mutable sobre la capa anterior
  ReaderT GameConfig (StateT ...)  ← agrega configuración de solo lectura encima de todo
@

Al ejecutar ('runGameM'), se desenvuelven de afuera hacia adentro:
primero el Reader (se suministra el Config), luego el State (se da el estado inicial),
luego el ExceptT (se "abre" la posibilidad de error), luego Identity (extrae el valor puro).

Por qué `newtype` y no un alias de tipo:

  * El alias sería @type GameM a = ReaderT ... a@ — GHC lo expandiría en cada
    mensaje de error, haciendo los tipos ilegibles.
  * El `newtype` le da un nombre corto ('GameM') a algo complejo, y permite
    agregar instancias propias en el futuro si hiciera falta.
-}
newtype GameM a = GameM
  { -- `unGameM` es el accessor del campo del newtype.
    -- Nos permite "desempaquetar" el newtype cuando necesitamos operar
    -- directamente sobre la pila interna (por ejemplo, en `runGameM`).
    unGameM ::
      ReaderT GameConfig (StateT GameState (ExceptT GameError Identity)) a
  }
  deriving newtype
    -- `deriving newtype` coerciona las instancias del tipo interno al `newtype`.
    -- Es seguro porque `newtype` y su tipo interno tienen la misma representación.
    -- Sin esta derivación, tendríamos que escribir a mano instancias como:
    --   instance Functor GameM where fmap f (GameM m) = GameM (fmap f m)
    -- que son completamente mecánicas.
    ( Functor
    -- ^ Permite aplicar una función al resultado: `fmap (+1) :: GameM Int -> GameM Int`.
    , Applicative
    -- ^ Permite `pure :: a -> GameM a` (envuelve un valor puro) y `<*>` (aplicación en contexto).
    , Monad
    -- ^ Permite `>>=` (bind) para secuenciar acciones: `accionA >>= \resultado -> accionB resultado`.
    , MonadReader GameConfig
    -- ^ Habilita `ask :: GameM GameConfig` (leer la config completa) y
    --   `asks f :: GameM b` (leer un campo: `asks gcGravity`).
    , MonadState GameState
    -- ^ Habilita `get :: GameM GameState`, `put :: GameState -> GameM ()`,
    --   y `modify :: (GameState -> GameState) -> GameM ()`.
    , MonadError GameError
    -- ^ Habilita `throwError :: GameError -> GameM a` y
    --   `catchError :: GameM a -> (GameError -> GameM a) -> GameM a`.
    )

-- ---------------------------------------------------------------------------
-- Intérprete / runner
-- ---------------------------------------------------------------------------

{- | Ejecuta una acción en 'GameM' y devuelve el resultado o un error.

Cada @run*@ desenvuelve (peela) una capa del transformer stack,
suministrando los valores necesarios para eliminar ese efecto:

@
  unGameM action
    :: ReaderT GameConfig (StateT GameState (ExceptT GameError Identity)) a

  flip runReaderT cfg
    :: StateT GameState (ExceptT GameError Identity) a
       (suministramos la configuración; el Reader desaparece)

  flip runStateT st
    :: ExceptT GameError Identity (a, GameState)
       (suministramos el estado inicial; el State pasa a devolver (resultado, estado_final))

  runExceptT
    :: Identity (Either GameError (a, GameState))
       (el ExceptT se convierte en un Either; los errores quedan capturados)

  runIdentity
    :: Either GameError (a, GameState)
       (quitamos la envoltura Identity; queda el valor puro)
@

Usamos `flip` porque `runReaderT :: ReaderT r m a -> r -> m a` toma
primero el transformer y luego el entorno. `flip runReaderT cfg` invierte
el orden, dejando el transformer como último argumento para poder componer
con `.` (composición de funciones).
-}
runGameM
  :: GameConfig
  -> GameState
  -> GameM a
  -> Either GameError (a, GameState)
runGameM cfg st =
  runIdentity . runExceptT . flip runStateT st . flip runReaderT cfg . unGameM
