# Wonder Boy (Haskell) — Trabajo Final

Propuesta de trabajo final para la materia de Programación Funcional.

## Integrantes

- Lucas Gonzalez Rouco
- Emilio Pablo Neme

## Descripción general

Se propone desarrollar un **videojuego de plataformas 2D** (estilo _Wonder Boy_). El foco no está solo en el juego en sí, sino en:

1. El diseño de un **motor modular**.
2. La definición de un **pequeño DSL** (Domain Specific Language) para modelar el comportamiento de las entidades y su interacción con el entorno.

## Desafíos en programación funcional

- Modelar un **sistema interactivo** (juego en tiempo real) de manera funcional y pura.
- Manejar **estado mutable** (mundo del juego) sin perder pureza, utilizando **abstracciones monádicas**.
- **Separar** la definición de comportamientos (lógica declarativa) de su ejecución (motor).
- Diseñar un **DSL composable** para expresar reglas del juego.

## Características de PF a utilizar

- **Mónadas** (`StateT`, `ReaderT`, `ExceptT`) para modelar estado, entorno y errores.
- **Free monads** para definir la lógica de entidades de forma abstracta.
- **Composición funcional** para construir el motor de juego de forma modular.
- **Inmutabilidad** y separación entre lógica pura y efectos.

## Arquitectura (puntos clave)

| Área                | Enfoque                                                           |
| ------------------- | ----------------------------------------------------------------- |
| Física y colisiones | Motor basado en **AABB**, desacoplado de la representación visual |
| Entidades           | Comportamientos definidos mediante **DSL**                        |
| IA                  | **Máquinas de estado** simples                                    |
| Contenido           | Carga de **niveles y configuraciones** desde archivos externos    |

## Estructura del proyecto

Organización prevista del repositorio (capas **Domain** pura, **UseCases**, **Adapters** y **Frameworks**):

```text
wonderboy-hs/
├── app/
│   └── Main.hs
│
├── src/
│   ├── Domain/                 # 100% PURO
│   │   ├── Model/
│   │   │   ├── Player.hs
│   │   │   ├── Enemy.hs
│   │   │   └── World.hs
│   │   │
│   │   ├── ValueObjects/
│   │   │   ├── Position.hs
│   │   │   └── Velocity.hs
│   │   │
│   │   └── Logic/
│   │       ├── Physics.hs
│   │       └── Collision.hs
│   │
│   ├── UseCases/               # aplicación (usa mónadas abstractas)
│   │   ├── GameMonad.hs        # definición abstracta (typeclass o newtype)
│   │   ├── UpdateGame.hs
│   │   └── Ports/              # interfaces (MUY importante)
│   │       ├── InputPort.hs
│   │       ├── RenderPort.hs
│   │       └── TimePort.hs
│   │
│   ├── Adapters/               # implementación de ports
│   │   ├── Input/
│   │   │   └── GlossInput.hs
│   │   ├── Rendering/
│   │   │   └── GlossRenderer.hs
│   │   └── Time/
│   │       └── SystemClock.hs
│   │
│   └── Frameworks/             # detalles externos
│       └── Gloss/
│           └── GameLoop.hs
│
├── test/
│   ├── Domain/
│   └── UseCases/
│
├── wonderboy-hs.cabal
└── cabal.project
```

## Bibliotecas previstas

| Biblioteca     | Uso                                             |
| -------------- | ----------------------------------------------- |
| **Gloss**      | Interfaz gráfica                                |
| **Aeson**      | Carga de niveles desde JSON                     |
| **Lens**       | Manipulación de estructuras de estado complejas |
| **MTL / Free** | Arquitectura monádica                           |

## Cómo ejecutar el proyecto

Requisitos: **GHC** y **Cabal** (por ejemplo instalados con [GHCup](https://www.haskell.org/ghcup/)).

Desde la raíz del repositorio:

```bash
cabal build
cabal run wonderboy-hs
```

El ejecutable se llama `wonderboy-hs` (definido en `wonderboy-hs.cabal`). Para correr la suite de tests:

```bash
cabal test
```

## Editor y HLint

La extensión **Haskell** (Haskell Language Server) integra **HLint**, pero necesitás el ejecutable en el `PATH` del entorno desde el que arranca el editor (o que `hlint` esté en el directorio por defecto de Cabal, p. ej. `~/.cabal/bin`).

Instalación (una vez):

```bash
cabal install hlint
```

- **Subrayado / color en el código** (advertencias o infos, según la regla).
- Panel **Problems** (⌘⇧M en macOS, Ctrl+Shift+M en Windows/Linux): listado por archivo y mensaje; podés filtrar por “Haskell” o buscar el texto del hint.
- **Code actions** (💡 o menú contextual / ⌘.**): en algunos hints ofrece “Apply HLint hint” o similar para aplicar el cambio automáticamente.

Si no aparece nada, comprobá en terminal que `hlint --version` funcione y que el archivo esté guardado; HLint se integra vía HLS cuando el proyecto compila para el servidor.

## División de tareas

El trabajo se reparte de forma equitativa:

- **Un integrante:** núcleo del motor y sistema de física/colisiones.
- **El otro:** DSL, IA y carga de datos.

Ambos participan en la **integración final** y el **informe**.

## Alcance

El alcance puede ajustarse según comentarios del docente; este documento refleja la propuesta enviada inicialmente.
