-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2020 Peter Lu
-- License     :  see the file LICENSE
--
-- Maintainer  :  pdlla <chippermonky@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structures for dynamically creating ids and tracking id'd objects
-- TODO remove this module from here, it's too specific
----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Reflex.Data.Directory
  ( DirId
  , DirectoryIdAssigner(..)
  , DirectoryIdAssignerConfig(..)
  , holdDirectoryIdAssigner
  , Directory(..)
  , DirectoryConfig(..)
  , holdDirectory
  )
where

import           Relude

import           Control.Monad.Fix

import qualified Data.IntMap.Strict    as IM
import qualified Data.List.NonEmpty    as NE
import           Data.These

import           Reflex
import           Reflex.Potato.Helpers



-- TODO change this to enum type class
type DirId = IM.Key

data DirectoryIdAssigner t v  = DirectoryIdAssigner {
  -- | tag an event with ided elements that got generate by the event
  -- the input event must be assigned to this directory
  -- i.e. it will be an event that is passed into
  -- '_directoryIdAssignerConfig_assign' in some way
  _directoryIdAssigner_tag :: forall a. Event t a -> Event t (NonEmpty (DirId, v))
}

data DirectoryIdAssignerConfig t v  = DirectoryIdAssignerConfig {
  _directoryIdAssignerConfig_assign :: Event t (NonEmpty v)
}

holdDirectoryIdAssigner
  :: forall t m v
   . (Reflex t, MonadHold t m, MonadFix m)
  => DirectoryIdAssignerConfig t v
  -> m (DirectoryIdAssigner t v)
holdDirectoryIdAssigner DirectoryIdAssignerConfig {..} = do
  uid <- foldDyn (\x -> (+ length x)) 0 _directoryIdAssignerConfig_assign
  --let assigned = attachWith (\firstid -> NE.zip (NE.fromList [firstid..])) (current uid) _directoryIdAssignerConfig_assign
  --dAssigned <- holdDyn ((-1,undefined) :| []) assigned
  let attached :: Event t (NonEmpty (DirId, v))
      attached = attachWith (\firstid -> NE.zip (NE.fromList [firstid ..]))
                            (current uid)
                            _directoryIdAssignerConfig_assign
      alignfn = \case
        These a _ -> Just a
        _         -> Nothing
      maketag :: forall a . Event t a -> Event t (NonEmpty (DirId, v))
      maketag = alignEventWithMaybe alignfn attached
  return DirectoryIdAssigner {
        --_directoryIdAssigner_assigned = dAssigned
                               _directoryIdAssigner_tag = maketag }



-- TODO implement this using DynamicIntMap (once you create it)
-- | this is bassically a DynamicIntMap with a restricted interface
data Directory t v = Directory {
  _directoryMap_contents  :: Behavior t (IM.IntMap v)
  , _directoryMap_added   :: Event t (NonEmpty (DirId, v))
  , _directoryMap_removed :: Event t (NonEmpty (DirId, v))

  -- TODO
  --, _directoryMap_modified :: Event t (NonEmpty (DirId, v, v))
}

data DirectoryConfig t v = DirectoryConfig {
  -- | add a element to the directory
  -- ensure the DirId was assigned from the same instance of DirectoryIdAssigner
  _directoryMapConfig_add          :: Event t (NonEmpty (DirId, v))
  -- | remove an element from the map
  , _directoryMapConfig_remove     :: Event t (NonEmpty DirId)

  -- TODO
  , _directoryMapConfig_modifyWith :: Event t (NonEmpty (DirId, v->v))
}



-- helper type for holdActionStack
data DCmd v = DCAdd (NonEmpty (DirId, v)) | DCRemove (NonEmpty DirId) | DCModify (NonEmpty (DirId, v->v))


holdDirectory
  :: forall t m v
   . (Reflex t, MonadHold t m, MonadFix m)
  => DirectoryConfig t v
  -> m (Directory t v)
holdDirectory DirectoryConfig {..} = mdo

--_directoryMapConfig_modifyWith :: Event t (NonEmpty (DirId, v->v))
  let
    -- lookup each element we are about to remove
    removed = fmap
      (\(m, els) ->
        catMaybes . toList . fmap (\i -> (\x -> (i, x)) <$> IM.lookup i m) $ els
      )
      (attach bDirectory _directoryMapConfig_remove)
    allEvs = leftmostwarn "Directory" [
      fmap DCAdd _directoryMapConfig_add
      , fmap DCRemove _directoryMapConfig_remove
      , fmap DCModify _directoryMapConfig_modifyWith]

    -- setup the directory
    addToMap els m = foldl' (\accm (i, e) -> IM.insert i e accm) m els
    removeFromMap els m = foldl' (\accm i -> IM.delete i accm) m els
    modifyInMap els m = foldl' (\accm (i, f) -> IM.adjust f i accm) m els

    foldfn :: DCmd v -> IM.IntMap v -> IM.IntMap v
    foldfn (DCAdd els) m    = addToMap els m
    foldfn (DCRemove els) m = removeFromMap els m
    foldfn (DCModify els) m = modifyInMap els m

    bDirectory = current directory

  directory :: Dynamic t (IM.IntMap v) <- foldDyn foldfn IM.empty allEvs
  return Directory { _directoryMap_contents = bDirectory
                   , _directoryMap_added    = _directoryMapConfig_add
                   , _directoryMap_removed  = fmapMaybe nonEmpty removed
                   }
