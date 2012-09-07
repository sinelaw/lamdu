{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.PolymorphicEdit(make) where

import Control.Monad (liftM, void)
import Data.Monoid (mappend)
import Data.Vector.Vector2 (Vector2(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

underline :: AnimId -> Draw.Color -> Widget f -> Widget f
underline animId color widget = Widget.atWFrame (mappend lineF) widget
  where
    Vector2 width height = Widget.wSize widget
    lineF =
      Anim.translate (Vector2 0 height) .
      Anim.scale (Vector2 width 1) $
      Anim.simpleFrame (animId ++ ["underline"]) line
    line = void . Draw.tint color $ Draw.line (0, 1) (1, 1)

-- make without the focus delegator
makeInner ::
  MonadF m =>
  ExpressionGui.Maker m -> Sugar.Polymorphic m ->
  Widget.Id -> VarAccess m (ExpressionGui m)
makeInner makeExpressionEdit poly myId =
  assignCursor $ do
    -- TODO: This is just to detect whether cursor is in the full expression.
    -- Even when it's not displayed, which is wasteful.
    fullExprEdit <- makeExpressionEdit $ Sugar.pFullExpression poly
    -- We are inside a non-delegating focus delegator made by makeExpressionEdit,
    -- so if the cursor is on us it means user enterred our widget.
    case (Widget.wIsFocused (ExpressionGui.egWidget fullExprEdit), Sugar.pCompact poly) of
      (False, Just compact) ->
        (liftM . decorate) Config.polymorphicCompactUnderlineColor .
        VarAccess.otransaction .
        ExpressionGui.atEgWidgetM (BWidgets.makeFocusableView myId) =<<
        makeExpressionEdit compact
      _ -> return $ decorate Config.polymorphicFullUnderlineColor fullExprEdit
  where
    decorate = ExpressionGui.atEgWidget . underline (Widget.toAnimId myId)
    assignCursor =
      maybe id (VarAccess.assignCursor myId . WidgetIds.fromGuid . Sugar.rGuid) $
      Sugar.pCompact poly

polymorphicFDConfig :: FocusDelegator.Config
polymorphicFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.expandPolymorphicKey
  , FocusDelegator.startDelegatingDoc = "Expand polymorphic"
  , FocusDelegator.stopDelegatingKey = Config.collapsePolymorphicKey
  , FocusDelegator.stopDelegatingDoc = "Collapse polymorphic"
  }

make ::
  MonadF m =>
  ExpressionGui.Maker m -> Sugar.Polymorphic m ->
  Widget.Id -> VarAccess m (ExpressionGui m)
make makeExpressionEdit poly =
  focusDelegate $ makeInner makeExpressionEdit poly
  where
    focusDelegate =
      case Sugar.pCompact poly of
      Nothing -> id
      Just _ ->
        BWidgets.wrapDelegatedVA polymorphicFDConfig
        FocusDelegator.NotDelegating ExpressionGui.atEgWidget
