{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts     #-}

------------------------------------------------------------------------------
-- | In this module you can find variations of @withDB@ functions.
--
-- Functions from this module are to be used when you have multiple MongoDB snaplets (databases) in your application.
------------------------------------------------------------------------------
module Snap.Snaplet.MongoDB.Functions.M
( eitherWithDB
, eitherWithDB'
, maybeWithDB
, maybeWithDB'
, unsafeWithDB
, unsafeWithDB'
) where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad
import           Control.Lens (cloneLens, use)

import           Snap (SnapletLens, snapletValue)
import           Snap.Snaplet.MongoDB.Core

import           Database.MongoDB (Action, AccessMode, Failure (ConnectionFailure), access)
import Data.Pool
import Control.Monad.Trans.Control

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > unsafeWithDB accountDB $ insert "test-collection" [ "some_field" = "something" ]
unsafeWithDB :: (MonadIO m, MonadState app m, MonadBaseControl IO m)
             => SnapletLens app MongoDB -- ^ The snaplet (database) on which you want the action to be run.
             -> Action m a             -- ^ 'Action' you want to perform.
             -> m a                     -- ^ The action's result; in case of failure 'error' is called.
unsafeWithDB snaplet action = getMongoAccessMode snaplet >>= flip (unsafeWithDB' snaplet) action

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > unsafeWithDB' accountDB UnconfirmedWrites $ insert "test-collection" [ "some_field" = "something" ]
unsafeWithDB' :: (MonadIO m, MonadState app m, MonadBaseControl IO m)
              => SnapletLens app MongoDB -- ^ The snaplet (database) on which you want the action to be run.
              -> AccessMode              -- ^ Access mode you want to use when performing the action.
              -> Action m a             -- ^ 'Action' you want to perform.
              -> m a                     -- ^ The action's result; in case of failure 'error' is called.
unsafeWithDB' snaplet mode action = do
    res <- eitherWithDB' snaplet mode action
    either (error . show) return res

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > maybeWithDB accountDB $ insert "test-collection" [ "some_field" = "something" ]
maybeWithDB :: (MonadIO m, MonadState app m, MonadBaseControl IO m)
            => SnapletLens app MongoDB -- ^ The snaplet (database) on which you want the action to be run.
            -> Action m a             -- ^ 'Action' you want to perform.
            -> m (Maybe a)             -- ^ 'Nothing' in case of failure or 'Just' the result of the action.
maybeWithDB snaplet action = getMongoAccessMode snaplet >>= flip (maybeWithDB' snaplet) action

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > maybeWithDB' accountDB UnconfirmedWrites $ insert "test-collection" [ "some_field" = "something" ]
maybeWithDB' :: (MonadIO m, MonadState app m, MonadBaseControl IO m)
             => SnapletLens app MongoDB -- ^ The snaplet (database) on which you want the action to be run.
             -> AccessMode              -- ^ Access mode you want to use when performing the action.
             -> Action m a             -- ^ 'Action' you want to perform.
             -> m (Maybe a)             -- ^ 'Nothing' in case of failure or 'Just' the result of the action.
maybeWithDB' snaplet mode action = do
    res <- eitherWithDB' snaplet mode action
    return $ either (const Nothing) Just res

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > eitherWithDB accountDB $ insert "test-collection" [ "some_field" = "something" ]
eitherWithDB :: (MonadIO m, MonadState app m, MonadBaseControl IO m)
             => SnapletLens app MongoDB -- ^ The snaplet (database) on which you want the action to be run.
             -> Action m a             -- ^ 'Action' you want to perform.
             -> m (Either Failure a)    -- ^ 'Either' 'Failure' or the action's result.
eitherWithDB snaplet action = getMongoAccessMode snaplet >>= flip (eitherWithDB' snaplet) action

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > eitherWithDB' accountDB UnconfirmedWrites $ insert "test-collection" [ "some_field" = "something" ]
eitherWithDB' :: (MonadIO m, MonadState app m, MonadBaseControl IO m)
              => SnapletLens app MongoDB -- ^ The snaplet (database) on which you want the action to be run.
              -> AccessMode              -- ^ Access mode you want to use when performing the action.
              -> Action m a             -- ^ 'Action' you want to perform.
              -> m (Either Failure a)    -- ^ 'Either' 'Failure' or the action's result.
eitherWithDB' snaplet mode action = do
    (MongoDB pool database _) <- use (cloneLens snaplet . snapletValue)
    r <- tryWithResource pool (\pip -> access pip mode database action)
    case r of
      Just a -> return $ Right a
      Nothing -> return $ Left $ ConnectionFailure $ userError "can not find pool resource"

getMongoAccessMode :: (MonadIO m, MonadState app m) => SnapletLens app MongoDB -> m AccessMode
getMongoAccessMode snaplet = mongoAccessMode `liftM` use (cloneLens snaplet . snapletValue)
{-# INLINE getMongoAccessMode #-}
