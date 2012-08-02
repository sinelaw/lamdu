{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Data.Store.Transaction
  ( Transaction, run, run_
  , Property
  , Store(..), AccessedKeys(..)
  , listenAccessedKeys
  , lookupBS, lookup
  , insertBS, insert
  , delete, deleteIRef
  , readIRef, readIRefDef, writeIRef
  , readGuid, readGuidDef, writeGuid
  , isEmpty
  , guidExists, irefExists
  , newIRef, newKey, newIRefWithGuid
  , fromIRef, fromIRefDef
  , followBy
  , anchorRef, anchorRefDef
  )
where

import           Prelude                   hiding (lookup)
import           Control.Applicative       (Applicative)
import           Control.Monad             (liftM)
import           Control.Monad.Trans.State (StateT, runStateT, get, gets, modify)
import           Control.Monad.Trans.Reader(ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Writer(WriterT, runWriterT, tell, listen)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Binary               (Binary)
import           Data.Binary.Utils         (encodeS, decodeS)
import           Data.Store.Rev.Change     (Key, Value)
import           Data.Store.IRef           (IRef)
import qualified Data.Store.IRef           as IRef
import           Data.Store.Guid           (Guid)
import qualified Data.Store.Property       as Property
import           Data.Monoid               (Monoid(..))
import           Data.Maybe                (isJust)
import           Data.ByteString           (ByteString)
import qualified Data.Map                  as Map
import           Data.Map                  (Map)
import qualified Data.AtFieldTH            as AtFieldTH

type Property t m = Property.Property (Transaction t m)

type Changes = Map Key (Maybe Value)

-- 't' is a phantom-type tag meant to make sure you run Transactions
-- with the right store
data Store t m = Store {
  storeNewKey :: m Key,
  storeLookup :: Key -> m (Maybe Value),
  storeAtomicWrite :: [(Key, Maybe Value)] -> m ()
  }

data AccessedKeys = AccessedKeys
  { akReads :: [Key]
  , akWrites :: [Key]
  }
instance Monoid AccessedKeys where
  mempty = AccessedKeys mempty mempty
  AccessedKeys ax ay `mappend` AccessedKeys bx by =
    AccessedKeys (ax `mappend` bx) (ay `mappend` by)

-- Define transformer stack:
newtype Transaction t m a = Transaction {
  unTransaction ::
     WriterT AccessedKeys (ReaderT (Store t m) (StateT Changes m)) a
  } deriving (Monad, Applicative, Functor)
AtFieldTH.make ''Transaction

liftAccessedKeys ::
  WriterT AccessedKeys (ReaderT (Store t m) (StateT Changes m)) a -> Transaction t m a
liftAccessedKeys = Transaction
liftStore :: Monad m => ReaderT (Store t m) (StateT Changes m) a -> Transaction t m a
liftStore = liftAccessedKeys . lift
liftChanges :: Monad m => StateT Changes m a -> Transaction t m a
liftChanges = liftStore . lift
liftInner :: Monad m => m a -> Transaction t m a
liftInner = liftChanges . lift

listenAccessedKeys
  :: Monad m => Transaction t m a -> Transaction t m (a, AccessedKeys)
listenAccessedKeys = atTransaction listen

isEmpty :: Monad m => Transaction t m Bool
isEmpty = liftChanges (gets Map.null)

lookupBS :: Monad m => Guid -> Transaction t m (Maybe Value)
lookupBS guid = do
  liftAccessedKeys $ tell mempty { akReads = [guid] }
  changes <- liftChanges get
  case Map.lookup guid changes of
    Nothing -> do
      store <- liftStore ask
      liftInner $ storeLookup store guid
    Just res -> return res

insertBS :: Monad m => Guid -> ByteString -> Transaction t m ()
insertBS key bs = do
  liftAccessedKeys $ tell mempty { akWrites = [key] }
  liftChanges . modify . Map.insert key $ Just bs

delete :: Monad m => Guid -> Transaction t m ()
delete key = do
  liftAccessedKeys $ tell mempty { akWrites = [key] }
  liftChanges . modify . Map.insert key $ Nothing

lookup :: (Monad m, Binary a) => Guid -> Transaction t m (Maybe a)
lookup = (liftM . fmap) decodeS . lookupBS

insert :: (Monad m, Binary a) => Guid -> a -> Transaction t m ()
insert key = insertBS key . encodeS

writeGuid :: (Monad m, Binary a) => Guid -> a -> Transaction t m ()
writeGuid = insert

guidExists :: Monad m => Guid -> Transaction t m Bool
guidExists = liftM isJust . lookupBS

readGuidMb :: (Monad m, Binary a) => Transaction t m a -> Guid -> Transaction t m a
readGuidMb nothingCase guid =
  maybe nothingCase return =<< lookup guid

readGuidDef :: (Monad m, Binary a) => a -> Guid -> Transaction t m a
readGuidDef = readGuidMb . return

readGuid :: (Monad m, Binary a) => Guid -> Transaction t m a
readGuid guid = readGuidMb failure guid
  where
    failure = fail $ show guid ++ " to inexistent object dereferenced"

deleteIRef :: Monad m => IRef a -> Transaction t m ()
deleteIRef = delete . IRef.guid

readIRefDef :: (Monad m, Binary a) => a -> IRef a -> Transaction t m a
readIRefDef def = readGuidDef def . IRef.guid

readIRef :: (Monad m, Binary a) => IRef a -> Transaction t m a
readIRef = readGuid . IRef.guid

irefExists :: (Monad m, Binary a) => IRef a -> Transaction t m Bool
irefExists = guidExists . IRef.guid

writeIRef :: (Monad m, Binary a) => IRef a -> a -> Transaction t m ()
writeIRef = writeGuid . IRef.guid

fromIRef :: (Monad m, Binary a) => IRef a -> Transaction t m (Property t m a)
fromIRef iref = liftM (flip Property.Property (writeIRef iref)) $ readIRef iref

fromIRefDef :: (Monad m, Binary a) => IRef a -> a -> Transaction t m (Property t m a)
fromIRefDef iref def = liftM (flip Property.Property (writeIRef iref)) $ readIRefDef def iref

newKey :: Monad m => Transaction t m Key
newKey = liftInner . storeNewKey =<< liftStore ask

newIRef :: (Monad m, Binary a) => a -> Transaction t m (IRef a)
newIRef val = do
  newGuid <- newKey
  insert newGuid val
  return $ IRef.unsafeFromGuid newGuid

newIRefWithGuid :: (Binary a, Monad m) => (Guid -> Transaction t m a) -> Transaction t m (IRef a)
newIRefWithGuid f = do
  newGuid <- newKey
  let iref = IRef.unsafeFromGuid newGuid
  writeIRef iref =<< f newGuid
  return iref

-- Dereference the *current* value of the IRef (Will not track new
-- values of IRef, by-value and not by-name)
followBy :: (Monad m, Binary a) =>
            (b -> IRef a) ->
            Property t m b ->
            Transaction t m (Property t m a)
followBy conv = fromIRef . conv . Property.value

anchorRef :: (Monad m, Binary a) => String -> Transaction t m (Property t m a)
anchorRef = fromIRef . IRef.anchor

anchorRefDef :: (Monad m, Binary a) => String -> a -> Transaction t m (Property t m a)
anchorRefDef name def = flip fromIRefDef def $ IRef.anchor name

run ::
  Monad m => Store t m -> Transaction t m a -> m (a, AccessedKeys)
run store transaction = do
  (res, changes) <-
    (`runStateT` mempty) .
    (`runReaderT` store) .
    runWriterT .
    unTransaction $
    transaction
  storeAtomicWrite store $ Map.toList changes
  return res

run_ ::
  Monad m => Store t m -> Transaction t m a -> m a
run_ = (fmap . fmap . liftM) fst run
