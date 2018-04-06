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
import qualified Chart as C -- ( Chart, Rect, Pair, Pos (..), grid, def, hud, ucolor )
import Control.Lens hiding (beside)
import Data.Generics.Labels()
import NumHask.Prelude as P
import qualified Diagrams.Prelude as D
import System.Random.MWC ( Gen, create )
import System.Random.MWC.Probability ( standard, samples )

import Control.Monad.Primitive (PrimState)
-- import Data.List ((!!), head, zipWith3)
-- import Data.Time
-- import Data.Time.Calendar.WeekDate
-- import qualified Diagrams.TwoD.Text
-- import Formatting
import NumHask.Histogram as NH ( fill, makeRects, DealOvers(..) )
-- import NumHask.Prelude as P

-- * example data generation
-- Standard normal random variates in one dimension.
rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standard gen

makeHistExample :: IO [C.Rect Double]
makeHistExample = do
  g <- create
  xys <- rvs g 1000
  let cuts = C.grid C.OuterPos (C.Range -15.0 15.0) 50
  pure $ NH.makeRects NH.IgnoreOvers (NH.fill cuts ((1.5 *) <$> xys))

histExample :: [C.Rect Double] -> C.Chart b
histExample h1 =
  let deltah =
        zipWith (\(C.Rect x y z w) (C.Rect _ _ _ w') -> C.Rect x y z (w - w')) h1 h1
      mainAspect = C.Rect -0.75 0.75 -0.5 0.5
      botAspect = C.Rect -0.75 0.75 -0.2 0.2
      (C.Ranges rx ry) = fold $ fold [h1, h1]
      (C.Ranges _ deltary) = fold (abs <$> deltah)
   in D.pad 1.1 $
     D.beside
       (D.r2 (0, -1))
       (C.rectChart
          [ #borderColor .~ C.ucolor 0 0 0 0 $
            #color .~ C.ucolor 0.365 0.647 0.855 0.2 $
            C.def
          , #borderColor .~ C.ucolor 0 0 0 0 $
            #color .~ C.ucolor 0.88 0.53 0.23 0.8 $
            C.def
          ]
          mainAspect
          (C.Ranges rx ry)
          [h1, h1])
       (C.rectChart
          [ #borderColor .~ C.ucolor 0 0 0 0 $
            #color .~ C.ucolor 0.88 0.53 0.23 0.8 $
            C.def
          ]
          botAspect
          (C.Ranges rx deltary)
          [deltah]) <>
        C.hud C.def botAspect (C.Ranges rx deltary)


someFunc :: IO ()
someFunc = do
  putStrLn ("someFunc" :: Text)
  putStrLn ("histExample" :: Text)
  hs <- makeHistExample
  C.fileSvg "histExample.svg" (#size .~ C.Pair 600 600 $ C.def) $
    histExample hs


  -- C.fileSvg "scaleExample.svg" (#size .~ C.Pair 600 240 $ C.def) $
  -- withHud
  -- C.def
  -- widescreen
  -- (Rect 0 20 0 2.0)
  -- (lineChart (repeat C.def))
  -- (vlineOneD ((0.1*) <$> [0..20]))

  -- putStrLn ("histDiffExample" :: Text)
  -- hs <- makeHistDiffExample
  -- fileSvg "other/histDiffExample.svg" (#size .~ C.Pair 600 600 $ C.def) $
  --         histDiffExample hs

