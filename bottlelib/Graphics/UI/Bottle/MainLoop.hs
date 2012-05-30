module Graphics.UI.Bottle.MainLoop (mainLoopAnim, mainLoopImage, mainLoopWidget) where

import Control.Arrow(first, second, (&&&), (***))
import Control.Concurrent(threadDelay)
import Control.Concurrent.MVar
import Control.Monad (liftM)
import Data.IORef
import Data.Monoid (mappend)
import Data.StateVar (($=))
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.DrawingCombinators.Utils (Image, drawText, textSize)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.EventMap (Event)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Widget(Widget)
import Graphics.UI.GLFW (defaultDisplayOptions, getWindowDimensions)
import Graphics.UI.GLFW.Events (GLFWEvent(..), eventLoop)
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

timeBetweenInvocations ::
  IO ((Maybe NominalDiffTime -> IO a) -> IO a)
timeBetweenInvocations = do
  mLastInvocationTimeVar <- newMVar Nothing
  return $ modifyMVar mLastInvocationTimeVar . updateTime
  where
    updateTime f mLastInvocationTime = do
      currentTime <- getCurrentTime
      let
        mTimeSince =
          fmap (currentTime `diffUTCTime`) mLastInvocationTime
      result <- f mTimeSince
      return (Just currentTime, result)

mainLoopImage ::
  Draw.Font -> (Size -> Event -> IO Bool) ->
  (Bool -> Size -> IO (Maybe Image)) -> IO a
mainLoopImage fpsFont eventHandler makeImage = GLFWUtils.withGLFW $ do
  addDelayArg <- timeBetweenInvocations
  GLFWUtils.openWindow defaultDisplayOptions

  let
    windowSize = do
      (x, y) <- getWindowDimensions
      return $ Vector2 (fromIntegral x) (fromIntegral y)

    handleEvent size (GLFWKeyEvent keyEvent) =
      eventHandler size keyEvent
    handleEvent _ GLFWWindowClose =
      error "Quit" -- TODO: Make close event
    handleEvent _ GLFWWindowRefresh = return True

    useFont = drawText fpsFont &&& textSize fpsFont
    scale w h = (Draw.scale w h %%) *** (* Vector2 w h)
    placeAt winSize ratio (image, size) =
      Draw.translate
      (Vector2.uncurry (,) (ratio * (winSize - size))) %% image
    addDelayToImage winSize mkMImage = addDelayArg $ \mTimeSince -> do
      mImage <- mkMImage
      return $
        fmap
          (mappend . placeAt winSize (Vector2 1 0) . scale 20 20 .
           useFont . maybe "N/A" (show . (1/)) $
           mTimeSince)
          mImage

    handleEvents events = do
      winSize@(Vector2 winSizeX winSizeY) <- windowSize
      anyChange <- fmap or $ mapM (handleEvent winSize) events
      GL.viewport $=
        (GL.Position 0 0,
         GL.Size (round winSizeX) (round winSizeY))
      mNewImage <- addDelayToImage winSize $ makeImage anyChange winSize
      case mNewImage of
        Nothing -> threadDelay 10000
        Just image ->
          Draw.clearRender .
          (Draw.translate (-1, 1) %%) .
          (Draw.scale (2/winSizeX) (-2/winSizeY) %%) $
          image
  eventLoop handleEvents

mainLoopAnim ::
  Draw.Font ->
  (Size -> Event -> IO (Maybe (AnimId -> AnimId))) -> (Size -> IO Anim.Frame) ->
  IO Anim.R -> IO a
mainLoopAnim font eventHandler makeFrame getAnimationHalfLife = do
  frameStateVar <- newIORef Nothing
  let
    makeImage isChange size = do
      frameState <- readIORef frameStateVar
      curTime <- getCurrentTime

      newFrameState <-
        case frameState of
          Nothing -> do
            dest <- makeFrame size
            return $ Just (0, (curTime, dest))
          Just (drawCount, (prevTime, prevFrame)) ->
            if drawCount == 0 || isChange
              then do
                dest <- makeFrame size
                animationHalfLife <- getAnimationHalfLife
                let
                  elapsed = realToFrac (curTime `diffUTCTime` prevTime)
                  progress = 1 - 0.5 ** (elapsed/animationHalfLife)
                return . Just $
                  case Anim.nextFrame progress dest prevFrame of
                    Nothing -> (drawCount + 1, (curTime, dest))
                    Just newFrame -> (0 :: Int, (curTime, newFrame))
              else
                return $ Just (drawCount + 1, (curTime, prevFrame))

      writeIORef frameStateVar newFrameState
      return $
        case newFrameState of
          Nothing -> error "No frame to draw at start??"
          Just (drawCount, (_, frame))
            | drawCount < stopAtDrawCount -> Just (Anim.draw frame)
            | otherwise -> Nothing
    -- A note on draw counts:
    -- When a frame is dis-similar to the previous the count resets to 0
    -- When a frame is similar and animation stops the count becomes 1
    -- We then should draw it again (for double buffering issues) at count 2
    -- And stop drawing it at count 3.
    stopAtDrawCount = 3
    imgEventHandler size event = do
      mEventResult <- eventHandler size event
      case mEventResult of
        Nothing -> return False
        Just animIdMapping -> do
          (modifyIORef frameStateVar . fmap)
            ((first . const) 0 .
             (second . second . Anim.mapIdentities) animIdMapping)
          return True
  mainLoopImage font imgEventHandler makeImage

mainLoopWidget :: Draw.Font -> IO (Widget IO) -> IO Anim.R -> IO a
mainLoopWidget font mkWidget getAnimationHalfLife = do
  widgetRef <- newIORef =<< mkWidget
  imageRef <- newIORef Nothing
  let
    eventHandler size event = do
      widget <- readIORef widgetRef
      result <- maybe (return Nothing) (liftM (Just . Widget.eAnimIdMapping)) .
        E.lookup event $ Widget.eventMap widget size
      writeIORef widgetRef =<< mkWidget
      writeIORef imageRef Nothing
      return result
    mkImage size = do
      mPrevImage <- readIORef imageRef
      let
        calcImage = do
          widget <- readIORef widgetRef
          return $ Widget.image widget size
      image <-
        case mPrevImage of
        Nothing -> calcImage
        Just (prevSize, prevImage) ->
          if prevSize == size
          then return prevImage
          else calcImage
      writeIORef imageRef $ Just (size, image)
      return image
  mainLoopAnim font eventHandler mkImage getAnimationHalfLife
