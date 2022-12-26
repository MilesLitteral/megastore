module Log.Function.Simplified where

import Log.Types
import Log.Function.Classic
import Control.Monad.IO.Class

-- |Simple Messages, Equivalent to Print but in ManiLog Style
-- A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
orderedMessageS :: MonadIO m => LogPrefix -> String  -> m ()
orderedMessageS p s = orderedMessage (LogMessage p LOG_MESSAGE s)

-- Simple Messages, Equivalent to Print but in ManiLog Style
-- A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
infoMessageS :: MonadIO m => LogPrefix -> String  -> m ()
infoMessageS p s = orderedMessage (LogMessage p LOG_INFO s)

-- An unchained message; Be careful this may break formatting if put in a list with an orderedMessage.
eventMessageS :: MonadIO m => LogPrefix -> String -> m ()
eventMessageS p s = eventMessage (LogMessage p LOG_ZONE s)

confirmMessageS :: MonadIO m => LogPrefix -> String -> m ()
confirmMessageS p s = confirmMessage (LogMessage p LOG_CONFIRMATION s)
  
-- Simple Messages, Equivalent to Print but in ManiLog Style
-- A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
warnMessageS :: MonadIO m => LogPrefix -> String  -> m ()
warnMessageS p s = orderedMessage (LogMessage p LOG_WARNING s)

-- An unchained Error Message which does not stop execution but just displays a message.
errorMessageS :: MonadIO m => LogPrefix -> String -> m ()
errorMessageS p s = errorMessage (LogMessage p LOG_ERROR s)

-- An unchained System Exception; Be careful this stops execution and displays an error message.
exceptionMessageS :: MonadIO m => LogPrefix -> String -> m ()
exceptionMessageS p s = exceptionMessage (LogMessage p LOG_EXCEPTION s)
