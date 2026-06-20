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

- [x] Movimiento y física del jugador (gravedad, plataformas, AABB)
- [x] Plataformas móviles
- [x] Recolección de ítems
- [x] Combate básico
- [x] IA de enemigos (Free monad)
- [x] Carga de niveles desde JSON

## Entregables del curso

- [x] Código del motor y **juego jugable** (`cabal run wonderboy-hs`)
- [x] Arquitectura por capas con núcleo puro en `Domain/` y mónadas en `UseCases/`
- [x] Tests de lógica donde aporten valor (`Domain`, `UseCases`) — opcionales pero recomendados
- [ ] Informe del trabajo final (según consigna del curso; sin exigencia de demostraciones formales en el repo)

## División de trabajo

| Responsable  | Ámbito                                |
| ------------ | ------------------------------------- |
| Integrante A | Motor, física y colisiones            |
| Integrante B | DSL de entidades, IA y carga de datos |
| Ambos        | Integración, pruebas e informe        |

## Jugar (sin instalar Haskell)

Descargá el juego desde [itch.io](https://lgonzalezrouco.itch.io/wonderboy) o, alternativamente,
el archivo para tu sistema operativo en
[GitHub Releases](https://github.com/lgonzalezrouco/wonderboy/releases). Descomprimí el
archivo (si aplica) y ejecutá:

| Plataforma                | Archivo                               | Cómo iniciar       |
| ------------------------- | ------------------------------------- | ------------------ |
| **Linux**                 | `wonderboy-hs-v*-linux-x86_64.tar.gz` | `./wonderboy-hs`   |
| **macOS** (Apple Silicon) | `wonderboy-hs-v*-macos-arm64.tar.gz`  | `./wonderboy-hs`   |
| **Windows**               | `wonderboy-hs-v*-windows-x86_64.zip`  | `wonderboy-hs.cmd` |

En macOS, si Gatekeeper bloquea el binario la primera vez: clic derecho → **Abrir**, o
`xattr -cr wonderboy-hs-bin` desde la carpeta descomprimida.

**Requisitos de runtime**

- **Linux:** OpenGL y GLUT (`sudo apt install libgl1-mesa-glx freeglut3` en Debian/Ubuntu).
- **macOS / Windows:** drivers gráficos del sistema (sin pasos extra en la mayoría de los casos).

### Controles

| Acción                   | Teclas                           |
| ------------------------ | -------------------------------- |
| Mover                    | ← → o **A** / **D**              |
| Saltar                   | ↑ o **W** (borde al presionar)   |
| Atacar (espada)          | **Espacio** (borde al presionar) |
| Arrojar arma             | **X** (borde al presionar)       |
| Confirmar menú           | **Enter** o **Espacio**          |
| Salir                    | **Esc**                          |
| Mostrar hitboxes (debug) | **F1**                           |

### Publicar una nueva versión

Creá y pusheá un tag de versión; el workflow `release.yml` compila en Linux, macOS y Windows,
corre los tests, sube los bundles a GitHub Releases y (si está configurado) publica en itch.io:

```bash
git tag v0.1.0
git push origin v0.1.0
```

**Configuración de itch.io (una sola vez)**

1. Creá la página del juego en [itch.io](https://itch.io/game/new) (Kind of project: **Downloadable**).
2. En itch.io → **Settings** → **API**, generá una clave y guardala como secreto de repositorio
   `BUTLER_API_KEY` en GitHub (**Settings** → **Secrets and variables** → **Actions**).
3. En el mismo menú, agregá variables de repositorio:
   - `ITCH_USER` — tu usuario de itch.io (p. ej. `lgonzalezrouco`)
   - `ITCH_GAME` — slug del juego en la URL (p. ej. `wonderboy` para `usuario.itch.io/wonderboy`)

Si `ITCH_USER` o `ITCH_GAME` están vacías, el job de itch.io se omite y solo se publica en GitHub
Releases.

## Desarrollo (desde el código fuente)

Requisitos: **GHC 9.6.7** y **Cabal 3.12** ([GHCup](https://www.haskell.org/ghcup/)).
En Linux también hacen falta las libs de OpenGL/GLUT (ver [docs/dependencies-and-tooling.md](docs/dependencies-and-tooling.md)).

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
