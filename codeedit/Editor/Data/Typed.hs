{-# LANGUAGE
    TemplateHaskell,
    GeneralizedNewtypeDeriving
  #-}
module Editor.Data.Typed
  ( StoredExpressionRef(..)
  , atEeInferredType, atEeValue
  , eeReplace, eeGuid, eeIRef
  , InferredTypeLoop(..)
  , StoredDefinition(..)
  , atDeIRef, atDeValue
  , deGuid
  , loadInferDefinition
  , loadInferExpression
  , StoredExpression(..), esGuid -- re-export from Data.Load
  , TypeData, TypedStoredExpression, TypedStoredDefinition
  ) where

--import qualified Data.Store.Transaction as Transaction
import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2, (<=<), when, unless, filterM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.UnionFind (UnionFindT, runUnionFindT)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..), mconcat)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.UnionFind.IntMap (prettyUF)
import Debug.TraceUtils
import Editor.Anchors (ViewTag)
import Editor.Data.Load (StoredExpressionRef(..), esGuid)
import qualified Control.Monad.Trans.List.Funcs as ListFuncs
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.UnionFind as UnionFind
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.List.Class as ListCls
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data
import qualified Editor.Data.Load as DataLoad
import qualified System.Random as Random

type T = Transaction ViewTag

type TypedStoredExpression = StoredExpression [InferredTypeLoop]
type TypedStoredDefinition = StoredDefinition [InferredTypeLoop]

eeGuid :: StoredExpression it m -> Guid
eeGuid = esGuid . eeStored

newtype TypeData = TypeData
  { unTypeData :: [Data.GuidExpression TypeRef]
  }
instance Show TypeData where
  show = show . unTypeData
type TypeRef = UnionFind.Point TypeData

data StoredExpression it m = StoredExpression
  { eeStored :: DataLoad.StoredExpressionRef m
  , eeInferredType :: it
  , eeValue :: Data.Expression (StoredExpression it m)
  } deriving (Eq)

instance Show it => Show (StoredExpression it m) where
  show (StoredExpression _ it value) =
    unwords ["(", show value, ":", show it, ")"]

data InferredTypeLoop
  = InferredTypeNoLoop (Data.GuidExpression InferredTypeLoop)
  | InferredTypeLoop Guid
  deriving (Show, Eq)

data StoredDefinition it m = StoredDefinition
  { deIRef :: Data.DefinitionIRef
  , deValue :: Data.Definition (StoredExpression it m)
  } deriving (Eq)

deGuid :: StoredDefinition it m -> Guid
deGuid = IRef.guid . deIRef

AtFieldTH.make ''StoredExpression
AtFieldTH.make ''StoredDefinition

eeReplace :: StoredExpression it m -> Maybe (Data.ExpressionIRef -> m ())
eeReplace = esReplace . eeStored

eeIRef :: StoredExpression it m -> Data.ExpressionIRef
eeIRef = esIRef . eeStored

--------------- Infer Stack boilerplate:

newtype Infer m a = Infer
  { unInfer :: UnionFindT TypeData (T m) a
  } deriving (Functor, Applicative, Monad)
AtFieldTH.make ''Infer

liftTypeRef
  :: Monad m
  => UnionFindT TypeData (T m) a -> Infer m a
liftTypeRef = Infer

liftTransaction
  :: Monad m => T m a -> Infer m a
liftTransaction = liftTypeRef . lift

----------------- Infer operations:

makeTypeRef :: Monad m => [Data.GuidExpression TypeRef] -> Infer m TypeRef
makeTypeRef = liftTypeRef . UnionFind.new . TypeData

getTypeRef :: Monad m => TypeRef -> Infer m [Data.GuidExpression TypeRef]
getTypeRef = liftM unTypeData . liftTypeRef . UnionFind.descr

setTypeRef :: Monad m => TypeRef -> [Data.GuidExpression TypeRef] -> Infer m ()
setTypeRef typeRef types = do
  tracePutStrLn $ unwords ["Setting", show typeRef, "to", show types]
  liftTypeRef . UnionFind.setDescr typeRef $
    TypeData types

runInfer
  :: Monad m
  => Infer m (TypedStoredExpression f)
  -> T m (TypedStoredExpression f)
runInfer =
  liftM canonizeIdentifiersTypes .
  liftM (\(uf, x) -> flip trace x $ prettyUF uf) .
  runUnionFindT . unInfer

makeSingletonTypeRef :: Monad m => Guid -> Data.Expression TypeRef -> Infer m TypeRef
makeSingletonTypeRef guid = makeTypeRef . (: []) . Data.GuidExpression guid

--------------

-- Entities whose Guid does not matter until canonization can just use
-- a zero guid:
zeroGuid :: Guid
zeroGuid = Guid.fromString "ZeroGuid"

mapStoredExpression
  :: Monad m
  => (StoredExpressionRef f
      -> a
      -> Data.Expression (StoredExpression b g)
      -> m (StoredExpression b g))
  -> StoredExpression a f
  -> m (StoredExpression b g)
mapStoredExpression f =
  Data.mapMExpression g
  where
    g (StoredExpression stored a val) =
      (return val, f stored a)

atInferredTypes
  :: Monad m
  => (StoredExpressionRef f -> a -> m b)
  -> StoredExpression a f
  -> m (StoredExpression b f)
atInferredTypes f =
  mapStoredExpression g
  where
    g stored a v = do
      b <- f stored a
      return $ StoredExpression stored b v

fromStoredExpression
  :: Monad m => (Guid -> Data.Expression ref -> m ref)
  -> StoredExpression it f -> m ref
fromStoredExpression mk =
  Data.mapMExpression f
  where
    f (StoredExpression stored _ val) =
      ( return val
      , mk $ esGuid stored
      )

pureExpressionFromStored
  :: StoredExpression it f -> Data.PureGuidExpression
pureExpressionFromStored =
  fmap runIdentity $ fromStoredExpression f
  where
    f guid = return . Data.PureGuidExpression . Data.GuidExpression guid

ignoreStoredMonad
  :: StoredExpression it (T Identity)
  -> StoredExpression it (T Identity)
ignoreStoredMonad = id

expandStoredToPure :: Monad m => StoredExpression it f -> T m Data.PureGuidExpression
expandStoredToPure =
  (`runReaderT` Map.empty) . recurse . pureExpressionFromStored
  where
    recurse e@(Data.PureGuidExpression (Data.GuidExpression guid val)) =
      case val of
      Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
        -- TODO: expand the result recursively (with some recursive
        -- constraint)
        liftM
        (pureExpressionFromStored .
         ignoreStoredMonad .
         storedFromLoaded () . Data.defBody . DataLoad.defEntityValue) .
        lift $ DataLoad.loadDefinition defI
      Data.ExpressionGetVariable (Data.ParameterRef guidRef) -> do
        mValue <- Reader.asks (Map.lookup guidRef)
        return $ fromMaybe e mValue
      Data.ExpressionApply
        (Data.Apply
         (Data.PureGuidExpression
          (Data.GuidExpression lamGuid
           -- TODO: Don't ignore paramType, we do want to recursively
           -- type-check this:
           (Data.ExpressionLambda (Data.Lambda _paramType body))))
         arg) -> do
          newArg <- recurse arg
          Reader.local (Map.insert lamGuid newArg) $
            recurse body
      Data.ExpressionLambda (Data.Lambda paramType body) ->
        recurseLambda Data.ExpressionLambda paramType body
      Data.ExpressionPi (Data.Lambda paramType body) -> do
        newParamType <- recurse paramType
        recurseLambda Data.ExpressionPi newParamType body
      _ -> return e
      where
        recurseLambda cons paramType body = do
          newBody <- recurse body
          return .
            Data.PureGuidExpression . Data.GuidExpression guid . cons $
            Data.Lambda paramType newBody

typeRefFromStored
  :: Monad m => StoredExpression it f
  -> Infer m TypeRef
typeRefFromStored =
  Data.mapMExpression f <=< liftTransaction . expandStoredToPure
  where
    f (Data.PureGuidExpression (Data.GuidExpression guid val)) =
      ( return val
      , makeSingletonTypeRef guid
      )

storedFromLoaded
  :: it
  -> DataLoad.ExpressionEntity f
  -> StoredExpression it f
storedFromLoaded it =
  runIdentity . Data.mapMExpression f
  where
    f (DataLoad.ExpressionEntity stored val) =
      (return val, makeStored stored)
    makeStored stored newVal =
      return $ StoredExpression stored it newVal

addTypeRefs
  :: Monad m
  => StoredExpression () f
  -> Infer m (StoredExpression TypeRef f)
addTypeRefs =
  mapStoredExpression f
  where
    f stored () val = do
      typeRef <- makeTypeRef []
      return $ StoredExpression stored typeRef val

derefTypeRef
  :: Monad m
  => TypeRef -> Infer m [InferredTypeLoop]
derefTypeRef =
  ListCls.toList . (`runReaderT` []) . go
  where
    liftInfer = lift . lift
    go typeRef = do
      visited <- Reader.ask
      isLoop <- liftInfer . liftTypeRef . liftM or $
        mapM (UnionFind.equivalent typeRef) visited
      if isLoop
        then return $ InferredTypeLoop zeroGuid
        else
        onType typeRef =<<
        lift . ListFuncs.fromList =<< liftInfer (getTypeRef typeRef)
    onType typeRef (Data.GuidExpression guid expr) =
      liftM (InferredTypeNoLoop . Data.GuidExpression guid) .
      Data.sequenceExpression $ Data.mapExpression recurse expr
      where
        recurse =
          Reader.mapReaderT
          (ListFuncs.fromList <=< lift . (liftM holify . ListCls.toList)) .
          Reader.local (typeRef :) .
          go
        holify [] =
          [InferredTypeNoLoop
           (Data.GuidExpression zeroGuid Data.ExpressionHole)]
        holify xs = xs

derefTypeRefs
  :: Monad m
  => StoredExpression TypeRef f
  -> Infer m (StoredExpression [InferredTypeLoop] f)
derefTypeRefs =
  mapStoredExpression f
  where
    f stored typeRef val = do
      types <- derefTypeRef typeRef
      return $ StoredExpression stored types val

unifyOnTree
  :: Monad m
  => StoredExpression TypeRef f
  -> Infer m ()
unifyOnTree = (`runReaderT` []) . go
  where
    go (StoredExpression stored typeRef value) = do
      case value of
        Data.ExpressionLambda (Data.Lambda paramType body) -> do
          paramTypeRef <- lift $ typeRefFromStored paramType
          tracePutStrLn $ unwords ["Stored:Lambda :", show typeRef, "(", show paramTypeRef, "->", show (eeInferredType body), ")"]
          -- We use "flip unify typeRef" so that the new Pi will be
          -- the official Pi guid due to the "left-bias" in
          -- unify/unifyPair. Thus we can later assume that we got the
          -- same guid in the pi and the lambda.
          lift $ do
            flip unify typeRef <=< makePi (esGuid stored) .
              Data.Lambda paramTypeRef $
              eeInferredType body
          Reader.local ((esGuid stored, paramTypeRef):) $ go body
        Data.ExpressionPi (Data.Lambda paramType resultType) -> do
          tracePutStrLn $ unwords ["Stored:Pi :", show typeRef]
          paramTypeRef <- lift $ typeRefFromStored paramType
          lift . setType $ eeInferredType resultType
          Reader.local ((esGuid stored, paramTypeRef):) $ go resultType
        Data.ExpressionApply (Data.Apply func arg) -> do
          tracePutStrLn $ unwords ["Stored:Apply :", show typeRef]
          go func
          go arg
          lift $ do
            let
              funcTypeRef = eeInferredType func
              argTypeRef = eeInferredType arg
            -- We give the new Pi the same Guid as the Apply. This is
            -- fine because Apply's Guid is meaningless and the
            -- canonicalization will fix it later anyway.
            unify funcTypeRef <=< makePi (esGuid stored) $
              Data.Lambda argTypeRef typeRef
            funcTypes <- getTypeRef funcTypeRef
            sequence_
              [ subst piGuid (typeRefFromStored arg) piResultTypeRef
              | Data.GuidExpression piGuid
                (Data.ExpressionPi
                 (Data.Lambda _ piResultTypeRef))
                <- funcTypes
              ]
        Data.ExpressionGetVariable (Data.ParameterRef guid) -> do
          tracePutStrLn $ unwords ["Stored:GetVar", show guid, ":", show typeRef]
          mParamTypeRef <- Reader.asks $ lookup guid
          lift $ case mParamTypeRef of
            -- TODO: Not in scope: Bad code,
            -- add an OutOfScopeReference type error
            Nothing -> return ()
            Just paramTypeRef -> setType paramTypeRef
        Data.ExpressionGetVariable (Data.DefinitionRef defI) -> lift $ do
          tracePutStrLn $ unwords ["Stored:GetVar", show defI, ":", show typeRef]
          defTypeStored <-
            liftM (storedFromLoaded [] . Data.defType . DataLoad.defEntityValue) .
            liftTransaction $
            DataLoad.loadDefinition defI
          defTypeRef <- typeRefFromStored $ ignoreStoredMonad defTypeStored
          setType defTypeRef
        Data.ExpressionLiteralInteger _ -> do
          tracePutStrLn $ unwords ["Stored:LiteralInt:", show typeRef]
          lift $
            setType <=< makeSingletonTypeRef zeroGuid . Data.ExpressionBuiltin $ Data.FFIName ["Prelude"] "Integer"
        _ -> return ()
      where
        makePi guid = makeSingletonTypeRef guid . Data.ExpressionPi
        setType = unify typeRef

unify
  :: Monad m
  => TypeRef
  -> TypeRef
  -> Infer m ()
unify a b = do
  e <- liftTypeRef $ UnionFind.equivalent a b
  unless e $ do
    as <- getTypeRef a
    bs <- getTypeRef b
    tracePutStrLn $
      unwords ["Unifying", show a, show as, "and",
               show b, show bs]
    result <- liftM (as ++) $ filterM (liftM not . matches as) bs
    tracePutStrLn $
      unwords ["Result of", show a, "+", show b, "is", show result]
    liftTypeRef $ do
      a `UnionFind.union` b
      UnionFind.setDescr a $ TypeData result
  where
    matches as y = liftM or $ mapM (`unifyPair` y) as

-- biased towards left child (if unifying Pis,
-- substs right child's guids to left)
unifyPair
  :: Monad m
  => Data.GuidExpression TypeRef
  -> Data.GuidExpression TypeRef
  -> Infer m Bool
unifyPair
  (Data.GuidExpression aGuid aVal)
  (Data.GuidExpression bGuid bVal)
  = case (aVal, bVal) of
    (Data.ExpressionPi l1,
     Data.ExpressionPi l2) ->
      unifyLambdas l1 l2
    (Data.ExpressionLambda l1,
     Data.ExpressionLambda l2) ->
      unifyLambdas l1 l2
    (Data.ExpressionApply (Data.Apply aFuncTypeRef aArgTypeRef),
     Data.ExpressionApply (Data.Apply bFuncTypeRef bArgTypeRef)) -> do
      unify aFuncTypeRef bFuncTypeRef
      unify aArgTypeRef bArgTypeRef
      return True
    (Data.ExpressionBuiltin bi1,
     Data.ExpressionBuiltin bi2) -> return $ bi1 == bi2
    (Data.ExpressionGetVariable v1,
     Data.ExpressionGetVariable v2) -> return $ v1 == v2
    (Data.ExpressionLiteralInteger i1,
     Data.ExpressionLiteralInteger i2) -> return $ i1 == i2
    (Data.ExpressionMagic,
     Data.ExpressionMagic) -> return True
    _ -> return False
  where
    unifyLambdas
      (Data.Lambda aParamTypeRef aResultTypeRef)
      (Data.Lambda bParamTypeRef bResultTypeRef) = do
      unify aParamTypeRef bParamTypeRef
      -- Remap b's guid to a's guid and return a as the unification:
      let
        mkGetAGuidRef =
          makeSingletonTypeRef zeroGuid . Data.ExpressionGetVariable $
          Data.ParameterRef aGuid
      tracePutStrLn $
        unwords ["Subst of", show bGuid, "to", show aGuid, "in", show bResultTypeRef]
      subst bGuid mkGetAGuidRef bResultTypeRef
      unify aResultTypeRef bResultTypeRef
      return True

subst :: Monad m => Guid -> Infer m TypeRef -> TypeRef -> Infer m ()
subst from mkTo rootRef = do
  refs <- allUnder rootRef
  mapM_ replace refs
  where
    removeFrom
      a@(Data.GuidExpression _
       (Data.ExpressionGetVariable (Data.ParameterRef guidRef)))
      | guidRef == from = (Any True, [])
      | otherwise = (Any False, [a])
    removeFrom x = (Any False, [x])
    replace typeRef = do
      (Any removed, new) <- liftM (mconcat . map removeFrom) $ getTypeRef typeRef
      when removed $ do
        setTypeRef typeRef new
        unify typeRef =<< mkTo

allUnder :: Monad m => TypeRef -> Infer m [TypeRef]
allUnder =
  (`execStateT` []) . recurse
  where
    recurse typeRef = do
      visited <- State.get
      alreadySeen <-
        lift . liftTypeRef . liftM or $
        mapM (UnionFind.equivalent typeRef) visited
      unless alreadySeen $ do
        State.modify (typeRef :)
        types <- lift $ getTypeRef typeRef
        mapM_ onType types
    onType =
      Data.sequenceExpression . Data.mapExpression recurse . Data.geValue

canonizeIdentifiersTypes
  :: TypedStoredExpression m
  -> TypedStoredExpression m
canonizeIdentifiersTypes =
  runIdentity . atInferredTypes canonizeTypes
  where
    canonizeTypes stored =
      return . zipWith canonizeIdentifiers (gens (esGuid stored))
    gens guid =
      map Random.mkStdGen . Random.randoms $
      guidToStdGen guid
    guidToStdGen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs
    canonizeIdentifiers gen =
      runIdentity . runRandomT gen . (`runReaderT` Map.empty) . f
      where
        onLambda oldGuid newGuid (Data.Lambda paramType body) =
          liftM2 Data.Lambda (f paramType) .
          Reader.local (Map.insert oldGuid newGuid) $ f body
        f (InferredTypeLoop _oldGuid) =
          lift . liftM InferredTypeLoop $ nextRandom
        f (InferredTypeNoLoop (Data.GuidExpression oldGuid v)) = do
          newGuid <- lift nextRandom
          liftM (InferredTypeNoLoop . Data.GuidExpression newGuid) $
            case v of
            Data.ExpressionLambda lambda ->
              liftM Data.ExpressionLambda $ onLambda oldGuid newGuid lambda
            Data.ExpressionPi lambda ->
              liftM Data.ExpressionPi $ onLambda oldGuid newGuid lambda
            Data.ExpressionApply (Data.Apply func arg) ->
              liftM Data.ExpressionApply $
              liftM2 Data.Apply (f func) (f arg)
            gv@(Data.ExpressionGetVariable (Data.ParameterRef guid)) ->
              Reader.asks $
              maybe gv (Data.ExpressionGetVariable . Data.ParameterRef) .
              Map.lookup guid
            x -> return x

builtinsToGlobals :: Map Data.FFIName Data.VariableRef -> InferredTypeLoop -> InferredTypeLoop
builtinsToGlobals _ x@(InferredTypeLoop _) = x
builtinsToGlobals builtinsMap (InferredTypeNoLoop (Data.GuidExpression guid expr)) =
  InferredTypeNoLoop . Data.GuidExpression guid $
  Data.mapExpression (builtinsToGlobals builtinsMap) $
  case expr of
  builtin@(Data.ExpressionBuiltin name) ->
    (maybe builtin Data.ExpressionGetVariable . Map.lookup name)
    builtinsMap
  other -> other

inferExpression
 :: Monad m
 => StoredExpression TypeRef f
 -> Infer m (TypedStoredExpression f)
inferExpression withTypeRefs = do
  tracePutStrLn $ "Top-level type is " ++ show (eeInferredType withTypeRefs)
  unifyOnTree withTypeRefs
  builtinsMap <- liftTransaction $ Property.get Anchors.builtinsMap
  derefed <- derefTypeRefs withTypeRefs
  return . runIdentity . (atInferredTypes . const) (Identity . map (builtinsToGlobals builtinsMap)) $ derefed

inferDefinition
  :: Monad m
  => DataLoad.DefinitionEntity (T f)
  -> T m (TypedStoredDefinition (T f))
inferDefinition (DataLoad.DefinitionEntity iref value) =
  liftM (StoredDefinition iref) $
  case value of
  Data.Definition typeI bodyI -> do
    let inferredType = canonizeIdentifiersTypes $ storedFromLoaded [] typeI

    inferredBody <- runInfer $ do
      inferredTypeRef <- typeRefFromStored inferredType
      withTypeRefs <- addTypeRefs $ storedFromLoaded () bodyI
      unify inferredTypeRef $ eeInferredType withTypeRefs
      inferExpression withTypeRefs
    return $ Data.Definition inferredType inferredBody

loadInferDefinition
  :: Monad m => Data.DefinitionIRef
  -> T m (TypedStoredDefinition (T m))
loadInferDefinition =
  inferDefinition <=< DataLoad.loadDefinition

loadInferExpression
  :: Monad m => Data.ExpressionIRef
  -> T m (TypedStoredExpression (T m))
loadInferExpression =
  runInfer . (inferExpression <=< addTypeRefs . storedFromLoaded ()) <=<
  flip DataLoad.loadExpression Nothing
