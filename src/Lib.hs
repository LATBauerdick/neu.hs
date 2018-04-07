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
import System.Process ( rawSystem )

import Control.Monad.Primitive (PrimState)
import Data.List ( zipWith3 ) --(!!), head, zipWith3)
-- import Data.Time
-- import Data.Time.Calendar.WeekDate
-- import qualified Diagrams.TwoD.Text
-- import Formatting
import NumHask.Histogram as NH ( fill, makeRects, regular, DealOvers(..) )
-- import NumHask.Prelude as P

-- * example data generation
-- Standard normal random variates in one dimension.
rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standard gen

-- This generates n V2 random variates where the x and y parts are correlated.
rvsCorr :: Gen (PrimState IO) -> Int -> Double -> IO [C.Pair Double]
rvsCorr gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zipWith C.Pair s0 s1'

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
          [deltah])
       <> C.hud C.def botAspect (C.Ranges rx deltary)

-- * scatter chart
mkScatterData :: IO [[C.Pair Double]]
mkScatterData = do
  g <- create
  xys <- rvsCorr g 1000 0.7
  xys1 <- rvsCorr g 1000 -0.5
  pure
    [ (\(C.Pair x y) -> C.Pair (x ^^ 2 + 3 * x - 1) (y + 1)) <$> xys
    , (\(C.Pair x y) -> C.Pair (x ^^ 2 + 3 * x + 1) y) <$> xys1
    ]

scatterHistExample :: [[C.Pair Double]] -> C.Chart b
scatterHistExample xys =
  D.beside
    (D.r2 (1, 0))
    (D.beside (D.r2 (0, -1)) (sc1 <> hud1) (D.reflectY histx))
    (D.reflectY $ D.rotateBy (3 / 4) histy)
  where
    sopts =
      zipWith3
        (\x y z -> C.GlyphOptions x y (C.ucolor 0 0 0 0) 0 z)
        [0.01, 0.02, 0.03]
        ((\x -> C.withOpacity (C.d3Colors1 x) 0.3) <$> [6, 8])
        [C.Circle, C.Triangle, C.Square]
    mainAspect = C.Rect -0.5 0.5 -0.5 0.5
    minorAspect = C.Rect -0.5 0.5 -0.1 0.1
    sc1 = C.glyphChart_ sopts mainAspect xys
    histx = C.rectChart_ defHist minorAspect hx
    histy = C.rectChart_ defHist minorAspect hy
    hud1 =
      C.hud
        (#axes .~ [#place .~ C.PlaceTop $ #label . #orientation .~ C.Pair 0 1 $ C.def] $
         C.def)
        mainAspect
        (C.range xys)
    defHist =
      (\x -> #borderSize .~ 0 $ #color .~ C.d3Colors1 x `C.withOpacity` 0.5 $ C.def) <$>
      [6, 8]
    makeHist n = makeRects NH.IgnoreOvers . NH.regular n
    hx = makeHist 50 . fmap (view D._x) <$> xys
    hy = makeHist 50 . fmap (view D._y) <$> xys


someFunc :: IO ()
someFunc = do
  putStrLn ("someFunc" :: Text)

  putStrLn ("histExample" :: Text)
  hs <- makeHistExample
  C.fileSvg "histExample.svg" (#size .~ C.Pair 600 600 $ C.def) $
    histExample hs

  putStrLn ("scatterHistExample" :: Text)
  xys <- mkScatterData
  C.fileSvg "scatterHistExample.svg" C.def (scatterHistExample xys)
  _ <- rawSystem "./ic" [ "scatterHistExample.svg" ]
  pure ()

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

