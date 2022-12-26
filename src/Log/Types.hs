module Log.Types where
    
data LogMessage = LogMessage { 
    order :: LogPrefix, 
    level :: LogLevel, 
    body  :: String 
}deriving(Show, Eq) 

data LogLevel = LOG_ZONE
    |LOG_INFO
    |LOG_DEBUG
    |LOG_MESSAGE
    |LOG_WARNING
    |LOG_CONFIRMATION
    |LOG_EXCEPTION
    |LOG_ERROR
    deriving(Show, Eq, Enum)

data LogPrefix = LOG_HEAD
    |LOG_BODY
    |LOG_TAIL 
    deriving(Show, Eq, Enum)
