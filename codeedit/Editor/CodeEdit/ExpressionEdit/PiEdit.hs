{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.PiEdit(make) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.HasParens
  -> Sugar.Pi m
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
make makeExpressionEdit hasParens (Sugar.Pi param resultType) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  VarAccess.assignCursor myId typeId $ do
    -- TODO: We pollute the resultTypeEdit with our generated name
    -- (which it will skip) even if we end up non-dependent and don't
    -- have a name
    (name, (resultTypeEdit, usedVars)) <-
      VarAccess.withName paramGuid $ \name ->
      liftM ((,) name) . VarAccess.usedVariables $
      FuncEdit.makeResultEdit makeExpressionEdit [paramId] resultType
    let
      paramUsed = paramGuid `elem` usedVars
      redirectCursor cursor
        | paramUsed = cursor
        | otherwise =
          case Widget.subId paramId cursor of
          Nothing -> cursor
          Just _ -> typeId
    VarAccess.atEnv (OT.atEnvCursor redirectCursor) $ do
      let paramType = Sugar.fpType param
      paramTypeGs <-
        case Sugar.rExpression paramType of
        Sugar.ExpressionAtom "Set" -> return []
        _ -> sequence
          [ liftM fvw $ label ":"
          , makeExpressionEdit paramType
          ]
      if paramUsed
        then do
          forallLabel <- label "∀"
          paramNameEdit <- FuncEdit.makeParamNameEdit name paramGuid
          dotLabel <- label "."
          return . ExpressionGui.hbox $
            map fvw
            [ forallLabel
            , paramNameEdit
            ] ++
            paramTypeGs ++
            map fvw
            [ dotLabel
            ] ++
            [ resultTypeEdit
            ]
        else do
          rightArrowLabel <-
            VarAccess.atEnv
            (OT.setTextSizeColor Config.rightArrowTextSize Config.rightArrowColor) $
            label "→"
          paramTypeEdit <- makeExpressionEdit paramType
          return $ ExpressionGui.hboxSpaced
            [paramTypeEdit, fvw rightArrowLabel, resultTypeEdit]
  where
    fvw = ExpressionGui.fromValueWidget
    label text = VarAccess.otransaction . BWidgets.makeLabel text $ Widget.toAnimId paramId
    paramGuid = Sugar.fpGuid param
    paramId = WidgetIds.fromGuid paramGuid
    typeId =
      WidgetIds.fromGuid . Sugar.rGuid . Sugar.fpType $
      param
