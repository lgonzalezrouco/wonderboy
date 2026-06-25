module Adapters.Gloss.TilingTest where

import Adapters.Gloss.Tiling (tilesToCover)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

-- | Tile natural del juego: puente de 70 px escalado a 'platformVisualHeight' 35.
tileSize :: Float
tileSize = 35

-- | Ancho de las plataformas móviles: 105 = 3 * 35, el múltiplo exacto que dispara el bug.
movingWidth :: Float
movingWidth = 105

-- | Reproduce cómo el render deriva el ancho del AABB: @(posX + w) - posX@, inexacto en 'Float'.
recalcWidth :: Float -> Float
recalcWidth posX = (posX + movingWidth) - posX

unit_exactMultipleCoversWithThreeTiles :: Assertion
unit_exactMultipleCoversWithThreeTiles =
  tilesToCover movingWidth tileSize @?= 3

unit_subPixelExcessDoesNotAddPhantomTile :: Assertion
unit_subPixelExcessDoesNotAddPhantomTile =
  tilesToCover 105.00001 tileSize @?= 3

unit_perceptiblyWiderSpanAddsTile :: Assertion
unit_perceptiblyWiderSpanAddsTile =
  tilesToCover 106 tileSize @?= 4

unit_movingPlatformTileCountStaysStable :: Assertion
unit_movingPlatformTileCountStaysStable =
  assertBool
    "el conteo de tiles debe mantenerse en 3 a lo largo de todo el recorrido"
    (all (== 3) counts)
 where
  counts = map (\px -> tilesToCover (recalcWidth px) tileSize) positions
  positions = take 800 (iterate (+ frameStep) 0)
  frameStep = 0.7667 :: Float
