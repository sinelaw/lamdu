{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FlyNav(make, State, initState) where

import Control.Applicative (Applicative(..), liftA2, (*>))
import Control.Arrow (second)
import Control.Lens ((^.))
import Control.Monad (void)
import Data.ByteString.Char8 () -- instance IsString ByteString
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Widget (Widget, Size)
import Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Widget as Widget

data Movement = Movement
  { _mName :: String
  , __mModKey :: EventMap.ModKey
  , _mDir :: Vector2 Widget.R
  }
LensTH.makeLenses ''Movement

data ActiveState = ActiveState
  { _asPos :: Vector2 Widget.R
  , _asMovements :: [Movement]
  }

type State = Maybe ActiveState

modifier :: EventMap.ModState
modifier = EventMap.ctrl `mappend` EventMap.shift

modifierKeys :: [EventMap.Key]
modifierKeys =
  [ EventMap.KeyLeftCtrl
  , EventMap.KeyRightCtrl
  , EventMap.KeyLeftShift
  , EventMap.KeyRightShift
  ]

initState :: State
initState = Nothing

withEmptyResult :: Functor f => f () -> f Widget.EventResult
withEmptyResult = (fmap . const) Widget.emptyEventResult

mkTickHandler :: Functor f => f () -> Widget.EventHandlers f
mkTickHandler = EventMap.tickHandler . withEmptyResult

mkKeyMap
  :: Functor f => EventMap.IsPress -> EventMap.ModKey -> EventMap.Doc
  -> f () -> Widget.EventHandlers f
mkKeyMap isPress key doc =
  EventMap.keyEventMap
  (EventMap.KeyEvent isPress key) doc .
  withEmptyResult

speed :: Vector2 Widget.R
speed = 8

accel :: Vector2 Widget.R
accel = 1.05

targetSize :: Size
targetSize = Vector2 25 25

targetColor :: Draw.Color
targetColor = Draw.Color 0.9 0.9 0 0.7

highlightColor :: Draw.Color
highlightColor = Draw.Color 0.4 0.4 1 0.4

target :: AnimId -> Vector2 Widget.R -> Anim.Frame
target animId pos =
  Anim.onDepth (subtract 100) .
  Anim.translate pos .
  Anim.scale targetSize .
  Anim.onImages (Draw.tint targetColor) .
  Anim.simpleFrame animId .
  void $ Draw.circle

cap :: Size -> Vector2 Widget.R -> Vector2 Widget.R
cap size = liftA2 max 0 . liftA2 min size

highlightRect :: AnimId -> Rect -> Anim.Frame
highlightRect animId (Rect pos size) =
  Anim.translate pos . Anim.scale size .
  Anim.onDepth (subtract 50) .
  Anim.onImages (Draw.tint highlightColor) $
  Anim.unitSquare animId

addMovements
  :: Functor f
  => Vector2 Widget.R
  -> [Movement]
  -> (Maybe ActiveState -> f ())
  -> Widget.EventHandlers f
addMovements = mconcat
  [ addMovement "Down"  (keysDown  stdDirKeys) (Vector2   0    1)
  , addMovement "Up"    (keysUp    stdDirKeys) (Vector2   0  (-1))
  , addMovement "Right" (keysRight stdDirKeys) (Vector2   1    0)
  , addMovement "Left"  (keysLeft  stdDirKeys) (Vector2 (-1)   0)
  ]

addMovement
  :: Functor f
  => String
  -> [EventMap.Key]
  -> Vector2 Widget.R
  -> Vector2 Widget.R
  -> [Movement]
  -> (Maybe ActiveState -> f ())
  -> Widget.EventHandlers f
addMovement name keys dir pos movements setState
  | name `elem` map (Lens.view mName) movements = mempty
  | otherwise =
    mconcat
    [ mkKeyMap EventMap.Press modKey ("Start FlyNav " ++ name) .
      setState . Just $ ActiveState pos (Movement name modKey dir : movements)
    | key <- keys
    , let modKey = EventMap.ModKey modifier key
    ]

zipped :: [a] -> [(a, [a])]
zipped [] = []
zipped (x:xs) = (x, xs) : (map . second) (x:) (zipped xs)

focalCenter :: Lens.SimpleLens (Widget f) (Vector2 Widget.R)
focalCenter = Widget.wFocalArea . Rect.center

make
  :: Applicative f => AnimId -> State -> (State -> f ())
  -> Widget f -> Widget f
make _ Nothing setState w =
  (Lens.over Widget.wEventMap . flip mappend)
  (addMovements (w ^. focalCenter) [] setState)
  w
make animId (Just (ActiveState pos movements)) setState w =
  (Lens.over Widget.wFrame . mappend) frame .
  Lens.set Widget.wEventMap eventMap $ w
  where
    delta = sum $ map (Lens.view mDir) movements
    highlight =
      maybe mempty
      (highlightRect (animId ++ ["highlight"]) . Lens.view Widget.enterResultRect)
      mEnteredChild
    frame = target (animId ++ ["target"]) pos `mappend` highlight
    mEnteredChild = fmap ($ targetPos) $ w ^. Widget.wMaybeEnter
    targetPos = Direction.Point pos
    nextState =
      ActiveState (cap (pos + delta*speed) (w ^. Widget.wSize))
      ((map . Lens.over mDir) (* accel) movements)
    eventMap = mconcat $
      (mkTickHandler . setState . Just) nextState :
      addMovements pos movements setState :
      finishMove :
      [ stopMovement name modKey lessMovements
      | (Movement name modKey _, lessMovements) <- zipped movements
      ]
    finishMove = mconcat
    -- TODO: This is buggy, need to be able to be informed that the
    -- key combo was released, regardless of which mod/key was
    -- released first:
      [ finishOn (EventMap.ModKey modifier key)
      | key <- modifierKeys
      ]
    stopMovement name modKey newMovements =
      mkKeyMap EventMap.Release modKey ("Stop FlyNav " ++ name) .
      setState . Just $ ActiveState pos newMovements
    finishOn modKey =
      EventMap.keyEventMap (EventMap.KeyEvent EventMap.Release modKey)
        "Stop FlyNav" $
        setState Nothing *>
        -- TODO: Just cancel FlyNav in any case if the MaybeEnter is
        -- Nothing...
        maybe (pure Widget.emptyEventResult) (Lens.view Widget.enterResultEvent)
          mEnteredChild
