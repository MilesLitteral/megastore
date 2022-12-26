{-# LANGUAGE RecordWildCards #-}
module Log.Function.Classic where

import Data.List
import Log.Types
import System.Exit
import Log.Function.Internal
import Control.Monad.IO.Class

--Util Functions for managing message formatting
logMessage :: MonadIO m => String -> m ()
logMessage text = liftIO $ putStr text

logMessageBar :: MonadIO m => String -> [String] -> m ()
logMessageBar color = logMessage . intercalate (" | " ++ color)

logMessageLines :: MonadIO m =>  [String] -> m ()
logMessageLines = logMessage . unlines

-- A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
orderedMessage :: MonadIO m => LogMessage -> m ()
orderedMessage message@LogMessage{..} = do
  let prefix  = log_prefix (level) (order)
  let msg     = log_color  (level) (prefix ++ " " ++ (body))
  liftIO $ putStrLn msg

-- An unchained message; Be careful this may break formatting if put in a list with an orderedMessage.
eventMessage :: MonadIO m => LogMessage -> m ()
eventMessage message@LogMessage{..} = do
  let event   = "\x1b[45m▨ " ++ body ++ " ▨"
  liftIO $ putStrLn event

confirmMessage :: MonadIO m => LogMessage -> m ()
confirmMessage message@LogMessage{..} = do
  let event   = "\x1b[45m⬤ " ++ body ++ " ⬤"
  liftIO $ putStrLn event
  answer <- liftIO getLine
  if answer == "Yes" then (liftIO $ putStrLn "Understood, Continue") else liftIO exitSuccess
  
-- An unchained Error Message which does not stop execution but just displays a message.
errorMessage :: MonadIO m => LogMessage -> m ()
errorMessage message@LogMessage{..} = do
  let msg     = ("\x1b[31m⚠  " ++ body ++ " ⚠")
  liftIO $ putStrLn msg

-- An unchained System Exception; Be careful this stops execution and displays an error message.
exceptionMessage :: MonadIO m => LogMessage -> m ()
exceptionMessage message@LogMessage{..} = do
  let err     = error "\x1b[31m✖ " ++ body ++ " ✖"
  liftIO $ putStrLn err
