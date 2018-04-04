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



someFunc :: IO ()
someFunc = -- putStrLn "someFunc"
  fileSvg "scaleExample.svg" (#size .~ Pair 600 240 $ def) $
  withHud
  def
  widescreen
  (Rect 0 20 0 2.0)
  (lineChart (repeat def))
  (vlineOneD ((0.1*) <$> [0..20]))

  putStrLn ("histDiffExample" :: Text)
  hs <- makeHistDiffExample
  fileSvg "other/histDiffExample.svg" (#size .~ Pair 600 600 $ def) $
          histDiffExample hs

