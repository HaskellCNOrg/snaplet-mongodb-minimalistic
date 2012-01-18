{-# LANGUAGE OverloadedStrings     #-}

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

import           Control.Monad.Error

import           Snap
import           Snap.Snaplet.MongoDB.Core

import           Database.MongoDB
import           System.IO.Pool

import qualified Control.Category as C ((.))

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > unsafeWithDB accountDB $ insert "test-collection" ["some_field" = "something" ]
unsafeWithDB :: (MonadIO m, MonadState app m)
             => Lens app (Snaplet MongoDB) -- ^ The snaplet (database) on which you want the action to be run.
             -> Action IO a                -- ^ 'Action' you want to perform.
             -> m a                        -- ^ The action's result; in case of failure 'error' is called.
unsafeWithDB snaplet = unsafeWithDB' snaplet UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > unsafeWithDB' accountDB UnconfirmedWrites $ insert "test-collection" ["some_field" = "something" ]
unsafeWithDB' :: (MonadIO m, MonadState app m)
              => Lens app (Snaplet MongoDB) -- ^ The snaplet (database) on which you want the action to be run.
              -> AccessMode                 -- ^ Access mode you want to use when performing the action.
              -> Action IO a                -- ^ 'Action' you want to perform.
              -> m a                        -- ^ The action's result; in case of failure 'error' is called.
unsafeWithDB' snaplet mode action = do
    res <- (eitherWithDB' snaplet mode action)
    either (error . show) return res

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > maybeWithDB accountDB $ insert "test-collection" ["some_field" = "something" ]
maybeWithDB :: (MonadIO m, MonadState app m)
            => Lens app (Snaplet MongoDB) -- ^ The snaplet (database) on which you want the action to be run.
            -> Action IO a                -- ^ 'Action' you want to perform.
            -> m (Maybe a)                -- ^ 'Nothing' in case of failure or 'Just' the result of the action.
maybeWithDB snaplet = maybeWithDB' snaplet UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > maybeWithDB' accountDB UnconfirmedWrites $ insert "test-collection" ["some_field" = "something" ]
maybeWithDB' :: (MonadIO m, MonadState app m)
             => Lens app (Snaplet MongoDB) -- ^ The snaplet (database) on which you want the action to be run.
             -> AccessMode                 -- ^ Access mode you want to use when performing the action.
             -> Action IO a                -- ^ 'Action' you want to perform.
             -> m (Maybe a)                -- ^ 'Nothing' in case of failure or 'Just' the result of the action.
maybeWithDB' snaplet mode action = do
    res <- (eitherWithDB' snaplet mode action)
    return $ either (const Nothing) Just res

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > eitherWithDB accountDB $ insert "test-collection" ["some_field" = "something" ]
eitherWithDB :: (MonadIO m, MonadState app m)
             => Lens app (Snaplet MongoDB) -- ^ The snaplet (database) on which you want the action to be run.
             -> Action IO a                -- ^ 'Action' you want to perform.
             -> m (Either Failure a)       -- ^ 'Either' 'Failure' or the action's result.
eitherWithDB snaplet = eitherWithDB' snaplet UnconfirmedWrites

------------------------------------------------------------------------------
-- | Database access function.
--
-- Example:
--
-- > eitherWithDB' accountDB UnconfirmedWrites $ insert "test-collection" ["some_field" = "something" ]
eitherWithDB' :: (MonadIO m, MonadState app m)
              => Lens app (Snaplet MongoDB) -- ^ The snaplet (database) on which you want the action to be run.
              -> AccessMode                 -- ^ Access mode you want to use when performing the action.
              -> Action IO a                -- ^ 'Action' you want to perform.
              -> m (Either Failure a)       -- ^ 'Either' 'Failure' or the action's result.
eitherWithDB' snaplet mode action = do
    (MongoDB pool database _) <- gets (getL ((C..) snapletValue snaplet))
    ep <- liftIO $ runErrorT $ aResource pool
    case ep of
         Left  err -> return $ Left $ ConnectionFailure err
         Right pip -> liftIO $ access pip mode database action


