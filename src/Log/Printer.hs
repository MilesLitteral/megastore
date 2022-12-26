module Log.Printer (logPrinter) where 
    
import Log.Types
import Log.Function.Classic
import Control.Monad.Writer

logPrinter :: WriterT LogMessage IO a -> IO ()
logPrinter io = do
   (_, res) <- runWriterT io
   if (order res) == LOG_HEAD
      then orderedMessage $ LogMessage LOG_HEAD (level res) (body res)
      else orderedMessage $ LogMessage LOG_BODY (level res) (body res)
   if (order res) == LOG_BODY
      then orderedMessage $ LogMessage LOG_BODY (level res) (body res)
      else orderedMessage $ LogMessage LOG_TAIL (level res) (body res)