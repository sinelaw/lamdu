{-# LANGUAGE TemplateHaskell #-}
module Editor.Data
  ( Definition(..), atDefBody, atDefType
  , DefinitionI, DefinitionIRef
  , FFIName(..)
  , VariableRef(..), variableRefGuid
  , Lambda(..), atLambdaParamType, atLambdaBody
  , LambdaI
  , Apply(..), atApplyFunc, atApplyArg
  , ApplyI
  , Expression(..)
  , ExpressionI, ExpressionIRef(..)
  , GuidExpression(..), atGeGuid, atGeValue
  , PureGuidExpression(..), atPureGuidExpression
  , newExprIRef, readExprIRef, writeExprIRef, exprIRefGuid
  , mapMExpression
  , mapExpression, sequenceExpression
  ) where

import Control.Monad (liftM, liftM2)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Store.Guid (Guid)
import Data.Store.IRef(IRef)
import Data.Store.Transaction (Transaction)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction

newtype ExpressionIRef = ExpressionIRef {
  unExpressionIRef :: IRef (Expression ExpressionIRef)
  } deriving (Eq, Ord, Show)

exprIRefGuid :: ExpressionIRef -> Guid
exprIRefGuid = IRef.guid . unExpressionIRef

newExprIRef
  :: Monad m
  => Expression ExpressionIRef -> Transaction t m ExpressionIRef
newExprIRef = liftM ExpressionIRef . Transaction.newIRef

readExprIRef
  :: Monad m
  => ExpressionIRef -> Transaction t m (Expression ExpressionIRef)
readExprIRef = Transaction.readIRef . unExpressionIRef

writeExprIRef
  :: Monad m
  => ExpressionIRef -> Expression ExpressionIRef -> Transaction t m ()
writeExprIRef = Transaction.writeIRef . unExpressionIRef

data Lambda expr = Lambda {
  lambdaParamType :: expr,
  lambdaBody :: expr
  } deriving (Eq, Ord, Show)
type LambdaI = Lambda ExpressionIRef

data Apply expr = Apply {
  applyFunc :: expr,
  applyArg :: expr
  } deriving (Eq, Ord, Show)
type ApplyI = Apply ExpressionIRef

data VariableRef
  = ParameterRef Guid -- of the lambda/pi
  | DefinitionRef DefinitionIRef
  deriving (Eq, Ord, Show)

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Eq, Ord)

instance Show FFIName where
  show (FFIName path name) = concatMap (++".") path ++ name

data Expression expr
  = ExpressionLambda (Lambda expr)
  | ExpressionPi (Lambda expr)
  | ExpressionApply (Apply expr)
  | ExpressionGetVariable VariableRef
  | ExpressionHole
  | ExpressionLiteralInteger Integer
  | ExpressionBuiltin FFIName
  | ExpressionMagic
  deriving (Eq, Ord, Show)
type ExpressionI = Expression ExpressionIRef

data Definition expr = Definition
  { defType :: expr
  , defBody :: expr
  } deriving (Eq, Ord, Show)
type DefinitionI = Definition ExpressionIRef
type DefinitionIRef = IRef DefinitionI


newtype PureGuidExpression = PureGuidExpression
  { unPureGuidExpression :: GuidExpression PureGuidExpression
  } deriving (Eq)

instance Show PureGuidExpression where
  show = show . unPureGuidExpression

data GuidExpression ref = GuidExpression
  { geGuid :: Guid
  , geValue :: Expression ref
  } deriving (Eq)

instance Show ref => Show (GuidExpression ref) where
  show (GuidExpression guid value) = show value ++ ":" ++ show guid

AtFieldTH.make ''PureGuidExpression
AtFieldTH.make ''GuidExpression


variableRefGuid :: VariableRef -> Guid
variableRefGuid (ParameterRef i) = i
variableRefGuid (DefinitionRef i) = IRef.guid i

derive makeBinary ''ExpressionIRef
derive makeBinary ''FFIName
derive makeBinary ''VariableRef
derive makeBinary ''Lambda
derive makeBinary ''Apply
derive makeBinary ''Expression
derive makeBinary ''Definition
AtFieldTH.make ''Lambda
AtFieldTH.make ''Apply
AtFieldTH.make ''Definition

mapExpression :: (a -> b) -> Expression a -> Expression b
mapExpression f (ExpressionLambda (Lambda x y)) = ExpressionLambda $ Lambda (f x) (f y)
mapExpression f (ExpressionPi (Lambda x y)) = ExpressionPi $ Lambda (f x) (f y)
mapExpression f (ExpressionApply (Apply x y)) = ExpressionApply $ Apply (f x) (f y)
mapExpression _ (ExpressionGetVariable var) = ExpressionGetVariable var
mapExpression _ ExpressionHole = ExpressionHole
mapExpression _ (ExpressionLiteralInteger int) = ExpressionLiteralInteger int
mapExpression _ (ExpressionBuiltin name) = ExpressionBuiltin name
mapExpression _ ExpressionMagic = ExpressionMagic

sequenceExpression :: Monad f => Expression (f a) -> f (Expression a)
sequenceExpression (ExpressionLambda (Lambda x y)) = liftM ExpressionLambda $ liftM2 Lambda x y
sequenceExpression (ExpressionPi (Lambda x y)) = liftM ExpressionPi $ liftM2 Lambda x y
sequenceExpression (ExpressionApply (Apply x y)) = liftM ExpressionApply $ liftM2 Apply x y
sequenceExpression (ExpressionGetVariable var) = return $ ExpressionGetVariable var
sequenceExpression ExpressionHole = return ExpressionHole
sequenceExpression (ExpressionLiteralInteger int) = return $ ExpressionLiteralInteger int
sequenceExpression (ExpressionBuiltin name) = return $ ExpressionBuiltin name
sequenceExpression ExpressionMagic = return ExpressionMagic

mapMExpression
  :: Monad m
  => (from
      -> ( m (Expression from)
         , Expression to -> m to ))
  -> from -> m to
mapMExpression f src =
  afterRecurse =<< sequenceExpression . mapExpression (mapMExpression f) =<< makeExpr
  where
    (makeExpr, afterRecurse) = f src
