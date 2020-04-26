-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2020 Peter Lu
-- License     :  see the file LICENSE
--
-- Maintainer  :  pdlla <chippermonky@gmail.com>
-- Stability   :  experimental
--
-- A dynamic structure intended for modelling action stacks for undo/redo.
-- Internally, it is represented as two stacks: "done" and "undone".
-- Redo/undo moves the top element from one stack to another.
-- New do events add elements to the "done" stack and clears the "undone" stack.
----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

module Reflex.Data.ActionStack
  ( ActionStack(..)
  , actionStack_makeDoSelector
  , actionStack_makeUndoSelector
  , ActionStackConfig(..)
  , holdActionStack
  )
where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Control.Monad.Fix

import qualified Data.Dependent.Sum    as DS
import qualified Data.GADT.Compare
import           Data.Wedge

data ActionStack t a = ActionStack {
  _actionStack_do     :: Event t a -- ^ fires when element is added to do stack
  , _actionStack_undo :: Event t a -- ^ fires when element is added to undo stack

  -- TODO this is misleading as only the undone stack gets cleared, not the done stack
  -- TODO change it so it's only when undone gets cleared or remove this event all together
  --, _actionStack_clear       :: Event t () -- ^ fires when action stack is cleared either due to a new do action or clear event

  -- probably don't want to expose these?
  --, _actionStack_doneStack   :: Dynamic t [a] -- ^ stack of actions we've done
  --, _actionStack_undoneStack :: Dynamic t [a] -- ^ stack of actions we've undone
}

-- | helper method for ActionStacks that use DSum to represent actions
actionStack_makeDoSelector
  :: (Data.GADT.Compare.GCompare k, Reflex t)
  => ActionStack t (DS.DSum k Identity)
  -> (k a -> Event t a)
actionStack_makeDoSelector as = select (fanDSum $ _actionStack_do as)

-- | helper method for ActionStacks that use DSum to represent actions
actionStack_makeUndoSelector
  :: (Data.GADT.Compare.GCompare k, Reflex t)
  => ActionStack t (DS.DSum k Identity)
  -> (k a -> Event t a)
actionStack_makeUndoSelector as = select (fanDSum $ _actionStack_undo as)

data ActionStackConfig t a = ActionStackConfig {
  _actionStackConfig_do      :: Event t a -- ^ event to add an element to the stack
  , _actionStackConfig_undo  :: Event t () -- ^ event to undo top action of do stack
  , _actionStackConfig_redo  :: Event t () -- ^ event to redo top action of undo stack
  , _actionStackConfig_clear :: Event t () -- ^ clears both do/undo stack without firing any do/undo events
}

-- helper type for holdActionStack
data ASCmd a = ASCDo a | ASCUndo | ASCRedo | ASCClear


holdActionStack
  :: forall t m a
   . (Reflex t, MonadHold t m, MonadFix m)
  => ActionStackConfig t a
  -> m (ActionStack t a)
holdActionStack (ActionStackConfig {..}) = do
  let
    changeEvent :: Event t (ASCmd a)
    changeEvent = leftmostwarn
      "ActionStack"
      [ fmap ASCDo            _actionStackConfig_do
      , fmap (const ASCUndo)  _actionStackConfig_undo
      , fmap (const ASCRedo)  _actionStackConfig_redo
      , fmap (const ASCClear) _actionStackConfig_clear
      ]

    -- Wedge values:
    -- 'Here' is element that was just added to do stack
    -- 'There' is element that was just added to undo stack
    -- 'Nowhere' is everything else
    foldfn
      :: (ASCmd a) -> (Wedge a a, [a], [a]) -> PushM t (Wedge a a, [a], [a])
    foldfn (ASCDo x) (_, xs    , _     ) = return (Here x, x : xs, []) -- clear undo stack on each new do
    foldfn ASCUndo   (_, []    , ys    ) = return (Nowhere, [], ys)
    foldfn ASCUndo   (_, x : xs, ys    ) = return (There x, xs, x : ys)
    foldfn ASCRedo   (_, xs    , []    ) = return (Nowhere, xs, [])
    foldfn ASCRedo   (_, xs    , y : ys) = return (Here y, y : xs, ys)
    foldfn ASCClear  (_, _     , _     ) = return (Nowhere, [], [])

  asdyn :: Dynamic t (Wedge a a, [a], [a]) <- foldDynM foldfn
                                                       (Nowhere, [], [])
                                                       changeEvent

  let changedEv :: Event t (Wedge a a)
      changedEv = fmap (\(x, _, _) -> x) (updated asdyn)

  return $ ActionStack { _actionStack_do   = fmapMaybe getHere changedEv
                       , _actionStack_undo = fmapMaybe getThere changedEv
    -- see comments in ActionStack definition above
    --, _actionStack_clear = leftmost [void _actionStackConfig_do, _actionStackConfig_clear]

    -- just delete these
    --, _actionStack_doneStack = fmap (\(_,x,_)->x) asdyn
    --, _actionStack_undoneStack = fmap (\(_,_,x)->x) asdyn
                       }
