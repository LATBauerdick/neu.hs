{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Lib
    ( someFunc
    ) where

-- | examples used in haddock docs
import Chart
import Control.Lens
import Data.Generics.Labels()
import NumHask.Prelude
import qualified Diagrams.Prelude as D

makeHistDiffExample :: IO ([Rect Double], [Rect Double])
makeHistDiffExample = do
  g <- create
  xys <- rvs g 1000
  xys1 <- rvs g 1000
  let cuts = grid OuterPos (Range -5.0 5.0) 50
  pure
    ( makeRects IgnoreOvers (fill cuts xys)
    , makeRects IgnoreOvers (fill cuts ((1.5 *) <$> xys1)))

-- histDiffExample :: ([Rect Double], [Rect Double]) -> Chart b
-- histDiffExample (h1, h2) =
--   let deltah =
--         zipWith (\(Rect x y z w) (Rect _ _ _ w') -> Rect x y z (w - w')) h1 h2
--       mainAspect = Rect -0.75 0.75 -0.5 0.5
--       botAspect = Rect -0.75 0.75 -0.2 0.2
--       (Ranges rx ry) = fold $ fold [h1, h2]
--       (Ranges _ deltary) = fold (abs <$> deltah)
--   in D.pad 1.1 $
--      D.beside
--        (D.r2 (0, -1))
--        (rectChart
--           [ #borderColor .~ ucolor 0 0 0 0 $
--             #color .~ ucolor 0.365 0.647 0.855 0.2 $
--             def
--           , #borderColor .~ ucolor 0 0 0 0 $
--             #color .~ ucolor 0.88 0.53 0.23 0.8 $
--             def
--           ]
--           mainAspect
--           (Ranges rx ry)
--           [h1, h2])
--        (rectChart
--           [ #borderColor .~ ucolor 0 0 0 0 $
--             #color .~ ucolor 0.88 0.53 0.23 0.8 $
--             def
--           ]
--           botAspect
--           (Ranges rx deltary)
--           [deltah]) <>
--         hud def botAspect (Ranges rx deltary)


someFunc :: IO ()
someFunc =
  putStrLn ( "someFunc":: Text )

  -- fileSvg "scaleExample.svg" (#size .~ Pair 600 240 $ def) $
  -- withHud
  -- def
  -- widescreen
  -- (Rect 0 20 0 2.0)
  -- (lineChart (repeat def))
  -- (vlineOneD ((0.1*) <$> [0..20]))

  -- putStrLn ("histDiffExample" :: Text)
  -- hs <- makeHistDiffExample
  -- fileSvg "other/histDiffExample.svg" (#size .~ Pair 600 600 $ def) $
  --         histDiffExample hs

