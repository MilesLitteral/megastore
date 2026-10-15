{-# LANGUAGE ForeignFunctionInterface #-}

-- | C-callable MegaStore interface for the proposed C# P/Invoke bridge.
-- ABI:
-- int32_t megastore_save(const char* utf8_path, const uint8_t* data, size_t len);
-- int32_t megastore_load(const char* utf8_path, uint8_t** out, size_t* len);
-- void    megastore_free(uint8_t* p);
--
-- Save and load operate on a SINGLE payload at key "payload" inside a
-- MegaStore archive. They do NOT expose the complete multi-entry API yet.
-- 0 success, 1 invalid arguments, 2 I/O/encoding/other failure,
-- 3 no "payload" entry in archive, 4 oversized result.
--
-- Native host MUST initialize the GHC RTS (hs_init) before calling exports.
module MegaStoreFFI where

import Control.Exception (SomeException, try, evaluate)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (poke)
import MegaStore (MegaStore(..), loadStore, saveStore, search)

foreign export ccall "megastore_save"
    megaStoreSave :: CString -> Ptr Word8 -> CSize -> IO CInt
foreign export ccall "megastore_load"
    megaStoreLoad :: CString -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt
foreign export ccall "megastore_free"
    megaStoreFree :: Ptr Word8 -> IO ()

-- | UTF-8 path, unlike peekCString which depends on locale encoding.
readPath :: CString -> IO FilePath
readPath p = do
    raw <- BS.packCString p
    case TE.decodeUtf8' raw of
        Left err -> ioError (userError ("Invalid UTF-8 path: " ++ show err))
        Right txt -> pure (T.unpack txt)

megaStoreSave :: CString -> Ptr Word8 -> CSize -> IO CInt
megaStoreSave pathPtr dataPtr size
    | pathPtr == nullPtr = pure 1
    | size > 0 && dataPtr == nullPtr = pure 1
    | toInteger size > toInteger (maxBound :: Int) = pure 1
    | otherwise = do
        result <- try $ do
            path <- readPath pathPtr
            bytes <- BS.packCStringLen (castPtr dataPtr, fromIntegral size)
            -- Legacy archive format: one entry for this convenience API.
            saveStore path (MegaStore [(T.pack "payload", bytes)])
            :: IO ()
        case (result :: Either SomeException ()) of
            Left _ -> pure 2
            Right _ -> pure 0

megaStoreLoad :: CString -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt
megaStoreLoad pathPtr outPtr lenPtr
    | pathPtr == nullPtr || outPtr == nullPtr || lenPtr == nullPtr = pure 1
    | otherwise = do
        poke outPtr nullPtr
        poke lenPtr 0
        result <- try $ do
            path <- readPath pathPtr
            store@(MegaStore _) <- loadStore path
            case search "payload" (_contents store) of
                Nothing -> pure (Left (3 :: CInt))
                Just bytes -> do
                    n <- evaluate (BS.length bytes)
                    if toInteger n > toInteger (maxBound :: CSize)
                        then pure (Left (4 :: CInt))
                        else if n == 0
                            then pure (Right ())
                            else do
                                -- Memory belongs to caller; megastore_free releases it.
                                mem <- mallocBytes n
                                BS.useAsCString bytes $ \src ->
                                    copyBytes mem (castPtr src) n
                                poke outPtr mem
                                poke lenPtr (fromIntegral n)
                                pure (Right ())
        case (result :: Either SomeException (Either CInt ())) of
            Left _ -> pure 2
            Right (Left code) -> pure code
            Right (Right ()) -> pure 0

megaStoreFree :: Ptr Word8 -> IO ()
megaStoreFree p
    | p == nullPtr = pure ()
    | otherwise = free p
