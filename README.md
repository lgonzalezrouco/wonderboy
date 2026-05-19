# Wonder Boy (Haskell) — Trabajo Final

Propuesta de trabajo final para la materia de **Programación Funcional** (ITBA).

## Integrantes

- Lucas Gonzalez Rouco
- Emilio Pablo Neme

## Objetivo

Desarrollar un **videojuego de plataformas 2D** inspirado en _Wonder Boy_, implementado como un **motor de juego modular** en Haskell. El jugador interactúa con un entorno dinámico: plataformas (incluidas móviles), recolección de ítems y combate básico.

Además del juego:

1. Un **motor modular** con capas claras (Domain, UseCases, Adapters, Frameworks).
2. Un **DSL** para modelar comportamiento de entidades y su interacción con el entorno.

## Desafíos de programación funcional

- **Estado global e interacción en tiempo real**: colisiones, entrada y actualización por cuadro sin perder el modelo denotacional en el núcleo.
- **Física reactiva** modelada con transformaciones puras en `Domain/`, orquestadas por una **pila monádica** en `UseCases/`.
- **Separación** entre descripción de comportamientos (DSL / Free monad) y ejecución (motor + Gloss).
- **Diseño denotacional**: lógica del juego como funciones puras en `Domain/`, sin perder
  transparencia referencial por efectos ocultos.

## Características de PF a utilizar

| Tema                                         | Aplicación en el proyecto                                                                             |
| -------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| **Inmutabilidad y funciones puras**          | Física, colisiones AABB y modelos en `Domain/` como transformaciones puras de datos.                  |
| **Composición y ADTs**                       | Estados de enemigos, eventos y AST del DSL de entidades.                                              |
| **Abstracción**                              | Funciones de orden superior en diseño de niveles y comportamientos.                                   |
| **Mónadas** (`StateT`, `ReaderT`, `ExceptT`) | Estado del juego, configuración y errores recuperables en `UseCases/` desde el inicio del desarrollo. |
| **Free monads**                              | IA y comportamientos de entidades: separar _descripción_ de acciones de su _interpretación_.          |

## Arquitectura

| Área                | Enfoque                                                 |
| ------------------- | ------------------------------------------------------- |
| Física y colisiones | Motor **AABB**, desacoplado de la representación visual |
| Entidades           | Comportamientos vía **DSL** (Free monad + intérpretes)  |
| IA                  | **Máquinas de estado** simples sobre el DSL             |
| Contenido           | Carga de **niveles y configuración** desde JSON (Aeson) |
| Efectos             | `IO` solo en **Adapters** y **Frameworks** (Gloss)      |

Firma orientativa del motor (denotación pura del paso de simulación):

```haskell
step :: DeltaTime -> Input -> World -> World
```

La actualización en tiempo real se expresa en `UseCases` con una pila monádica que interpreta `step`, entrada y estado global.

### Estructura del repositorio

```text
wonderboy-hs/
├── app/
│   └── Main.hs
├── src/
│   ├── Domain/                 # 100% puro (sin IO)
│   │   ├── Model/
│   │   ├── ValueObjects/
│   │   └── Logic/
│   ├── UseCases/               # GameMonad, UpdateGame, mónadas abstractas
│   │   └── Ports/
│   ├── Adapters/               # Gloss (input, render, tiempo)
│   └── Frameworks/
│       └── Gloss/
├── test/
│   ├── Domain/
│   └── UseCases/
│
├── wonderboy-hs.cabal
└── cabal.project
```

## Stack tecnológico

| Biblioteca                               | Rol                                                                                    |
| ---------------------------------------- | -------------------------------------------------------------------------------------- |
| **Gloss**                                | Gráficos y game loop (Input / Update / Draw)                                           |
| **mtl** (`StateT`, `ReaderT`, `ExceptT`) | Estado, entorno y errores en `UseCases/`                                               |
| **free**                                 | DSL de entidades e IA                                                                  |
| **Aeson**                                | Niveles y parámetros en JSON                                                           |
| **tasty** + **tasty-hunit**              | Tests unitarios                                                                        |
| **lens** (opcional)                      | Actualizaciones profundas de estado en capas con efectos, si la anidación lo justifica |

## Funcionalidades del juego

- [ ] Movimiento y física del jugador (gravedad, plataformas, AABB)
- [ ] Plataformas móviles
- [ ] Recolección de ítems
- [ ] Combate básico
- [ ] IA de enemigos (Free monad)
- [ ] Carga de niveles desde JSON

## Entregables del curso

- [ ] Código del motor y **juego jugable** (`cabal run wonderboy-hs`)
- [ ] Arquitectura por capas con núcleo puro en `Domain/` y mónadas en `UseCases/`
- [ ] Tests de lógica donde aporten valor (`Domain`, `UseCases`) — opcionales pero recomendados
- [ ] Informe del trabajo final (según consigna del curso; sin exigencia de demostraciones formales en el repo)

## División de trabajo

| Responsable  | Ámbito                                |
| ------------ | ------------------------------------- |
| Integrante A | Motor, física y colisiones            |
| Integrante B | DSL de entidades, IA y carga de datos |
| Ambos        | Integración, pruebas e informe        |

## Cómo ejecutar

Requisitos: **GHC** y **Cabal** ([GHCup](https://www.haskell.org/ghcup/)).

```bash
cabal build all --enable-tests
cabal run wonderboy-hs
cabal test all
fourmolu --mode check src app test
hlint src app test
```

## Editor y HLint

La extensión **Haskell** (HLS) usa HLint si el ejecutable está en el `PATH` (p. ej. `~/.cabal/bin`):

```bash
cabal install hlint
```

Si no aparecen hints, verificá `hlint --version` y que el proyecto compile para HLS.
