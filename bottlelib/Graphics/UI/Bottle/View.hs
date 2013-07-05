{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Graphics.UI.Bottle.View
  ( View, augmentAnimId, backgroundColor, scaled
  ) where

import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId, Layer)
import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as SBS8
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim

type View = (Anim.Size, Anim.Frame)

augmentAnimId :: Show a => AnimId -> a -> AnimId
augmentAnimId animId = Anim.joinId animId . (:[]) . SBS8.pack . show

backgroundColor :: AnimId -> Layer -> Draw.Color -> View -> View
backgroundColor animId layer color (size, frame) =
  (size, Anim.backgroundColor (animId ++ ["bg"]) layer color size frame)

scaled :: Vector2 Draw.R -> Lens.Iso' View View
scaled factor =
  Lens.iso (scale factor) (scale (1/factor))
  where
    scale ratio (size, frame) =
      (size*ratio, Anim.scale ratio frame)
