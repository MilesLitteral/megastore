module Log.Function.Internal where

import Log.Types

--terminate: \033[0m
log_color :: LogLevel -> [Char] -> [Char]
log_color level messageBody = 
    case level of
    LOG_INFO         -> "\x1b[96m" ++ messageBody -- ++ "\033"  --[0m
    LOG_MESSAGE      -> "\x1b[34m" ++ messageBody -- ++ "\033" 
    LOG_ZONE         -> "\x1b[38;5;50m" ++ messageBody -- ++ "\033"
    LOG_EVENT        -> "\x1b[38;5;88m"  ++ messageBody
    LOG_DEBUG        -> "\x1b[33m" ++ messageBody -- ++ "\033"
    LOG_WARNING      -> "\x1b[35m" ++ messageBody -- ++ "\033"
    LOG_ERROR        -> "\x1b[31m" ++ messageBody -- ++ "\033"
    LOG_EXCEPTION    -> "\x1b[31m" ++ messageBody -- ++ "\033"
    LOG_CONFIRMATION -> "\x1b[38;5;124m" ++ messageBody -- ++ "\033"

log_color' :: LogLevel -> [Char]
log_color' level  = 
    case level of
    LOG_INFO         -> "\x1b[96m" ++ log_symbol level ++ "]\x1b[033m " -- ++ "\033"  --[0m
    LOG_MESSAGE      -> "\x1b[34m" ++ log_symbol level ++ "]\x1b[033m " -- ++ "\033" 
    LOG_ZONE         -> "\x1b[32m" ++ log_symbol level ++ "]\x1b[033m " -- ++ "\033"
    LOG_EVENT        -> "\x1b[38;5;88m" ++ log_symbol level ++ "]\x1b[033m "
    LOG_DEBUG        -> "\x1b[33m" ++ log_symbol level ++ "]\x1b[033m " -- ++ "\033"
    LOG_WARNING      -> "\x1b[35m" ++ log_symbol level ++ "]\x1b[033m " -- ++ "\033"
    LOG_ERROR        -> "\x1b[31m" ++ log_symbol level ++ "]\x1b[033m " -- ++ "\033"
    LOG_EXCEPTION    -> "\x1b[31m" ++ log_symbol level ++ "]\x1b[033m " -- ++ "\033"
    LOG_CONFIRMATION -> "\x1b[36m" ++ log_symbol level ++ "]\x1b[033m " -- ++ "\033"

log_symbol :: LogLevel -> String
log_symbol sym = 
    case sym of 
        LOG_INFO         -> "🈳️" --ℹ️ 
        LOG_MESSAGE      -> "💬" 
        LOG_ZONE         -> "🈯"
        LOG_DEBUG        -> "🚧"
        LOG_WARNING      -> "🚸"
        LOG_ERROR        -> "🈲"
        LOG_EXCEPTION    -> "🈵"
        LOG_CONFIRMATION -> "🉐️"
        LOG_EVENT        -> "ℹ️"

log_prefix :: LogLevel -> LogPrefix -> [Char]
log_prefix level ord =
    case ord of
        LOG_HEAD   -> "╔[" ++ log_symbol level ++ "]"
        LOG_BODY   -> "╠[" ++ log_symbol level ++ "]" 
        LOG_TAIL   -> "╚[" ++ log_symbol level ++ "]"
        --_      -> error "\x1b[31m ✖ INVALID LOG PREFIX ✖ \033[0m"

log_prefix' :: Int -> String
log_prefix' line = case line of
                  0 -> "╔["
                  1 -> "╠["
                  2 -> "╚["

-- displayContents :: KeyStore -> String
-- displayContents store = (show $ (map (\x -> (fst x, Lib.null $ snd $ x)) (store ^. contents)))
