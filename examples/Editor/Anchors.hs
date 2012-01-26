{-# LANGUAGE EmptyDataDecls #-}

module Editor.Anchors(
    clipboard,
    root, rootIRef,
    Cursor, cursor, cursorIRef,
    focalPointIRefs, branches, view,
    currentBranchIRef, currentBranch,
    initDB,
    dbStore, DBTag,
    viewStore, ViewTag)
where

import Control.Monad (unless)
import Data.Binary (Binary)
import Data.Store.Db (Db)
import Data.Store.IRef (IRef)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction, Store)
import Editor.Data (ITreeD, TreeD)
import Graphics.UI.Bottle.Animation(AnimId)
import qualified Data.Store.Db as Db
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.AnimIds as AnimIds
import qualified Editor.Data as Data

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: Monad m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

clipboard :: Monad m => Transaction.Property ViewTag m [ITreeD]
clipboard = Transaction.anchorRefDef "clipboard" []

rootIRef :: ITreeD
rootIRef = IRef.anchor "root"

root :: Monad m => Transaction.Property ViewTag m TreeD
root = Transaction.fromIRef rootIRef

focalPointIRefs :: Monad m => Transaction.Property ViewTag m [ITreeD]
focalPointIRefs = Transaction.anchorRefDef "focalPoint" []

branchesIRef :: IRef [(IRef String, Branch)]
branchesIRef = IRef.anchor "branches"

branches :: Monad m => Transaction.Property DBTag m [(IRef String, Branch)]
branches = Transaction.fromIRef branchesIRef

currentBranchIRef :: IRef Branch
currentBranchIRef = IRef.anchor "currentBranch"

currentBranch :: Monad m => Transaction.Property DBTag m Branch
currentBranch = Transaction.fromIRef currentBranchIRef

type Cursor = AnimId

cursorIRef :: IRef Cursor
cursorIRef = IRef.anchor "cursor"

-- Cursor is untagged because it is both saved globally and per-revision.
-- Cursor movement without any revisioned changes are not saved per-revision.
cursor :: Monad m => Transaction.Property t m Cursor
cursor = Transaction.fromIRef cursorIRef

-- Initialize an IRef if it does not already exist.
initRef :: (Binary a, Monad m) => IRef a -> Transaction t m a -> Transaction t m a
initRef iref act = do
  exists <- Transaction.irefExists iref
  unless exists (Property.set p =<< act)
  Property.get p
  where
    p = Transaction.fromIRef iref

viewIRef :: IRef View
viewIRef = IRef.anchor "HEAD"

view :: Monad m => Transaction.Property DBTag m View
view = Transaction.fromIRef viewIRef

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef branchesIRef $ do
      masterNameIRef <- Transaction.newIRef "master"
      initialVersionIRef <-
        Version.makeInitialVersion
          [Version.makeInitialValue rootIRef (Data.makeNode "" []),
           Version.makeInitialValue cursorIRef (AnimIds.fromIRef rootIRef)]
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    let branch = snd $ head bs
    _ <- initRef viewIRef $ View.new branch
    _ <- initRef currentBranchIRef (return branch)
    _ <- initRef cursorIRef . return $ []
    return ()