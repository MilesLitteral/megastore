{-# LANGUAGE RankNTypes, FlexibleInstances, PolyKinds, TemplateHaskell, ScopedTypeVariables #-}

{-|
Module      : KeyStore
Description : a haskell data and file type for efficient image storage 
Copyright   : (c) Miles J. Litteral 2023
License     : BSD-3
Maintainer  : mandaloe2@gmail.com
Stability   : release
Portability : POSIX

A Module for taking a directory of images (for example) and turning them into a key referencable data structure
that will efficiently store all images. Here is a quick crash course:

    @
        a1  <- loadFile "s1.png"
        a2  <- loadFile "s2.png"
        a3  <- loadFile "s3.png"

        let testSet = KeyStore [("s1", a1), ("s2", a2), ("s3", a3)]
        saveStore "./test/testSet" testSet

        loadedContents <- loadStore "./test/testSet.keystore"
        autoUnpack "./results" loadedContents
    @
-}
module KeyStore
    (  -- * Records #Records#
    KeyStore(..)
    -- * I/O Functions     #Functions#
    , saveStore
    , loadStore
    , loadFile
    , loadDirectory
    , createKeystoreWithBulk
    , unpackStore
    , unpackStore'
    , autoUnpack
    -- ** Utility Functions #Functions#
    , append
    , search
    , keyExists
    , remove
    , remove'
    , keystoreToMap
    , mapToKeyStore
    ) where

import Codec.Picture
import Codec.Compression.GZip

import Data.List  (find)
import Data.Maybe ()
import Data.Either
import Data.Binary
import Data.Map (Map(..), toList, fromList)
import Data.Text hiding (append, take, drop, map, find, filter, zip, length)
import System.Directory
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import TextShow
import Control.Lens
import Control.Lens.TH ()

-- | The KeyStore Data Type itself, fundamentally it is a List of Tuples
newtype KeyStore =
    KeyStore {
        _contents :: [(Text, BS.ByteString)] -- ^ the contents of the KeyStore, while made for images it is acknowledged anything that satisfies the constraint/assertion may be a KeyStore
    } deriving(Ord, Eq, Show)
 
makeLenses ''KeyStore

-- | The KeyStore Data Type's instance for serializing the data structure to file type,
instance Binary KeyStore where
      put (KeyStore contents) = put (map (\x -> (fst x, snd x)) contents)
      get = KeyStore <$> get

-- | Writes a KeyStore to physical memory, it does so via Data.ByteString.Lazy.WriteFile
-- Where the data is compressed and encoded to Strict ByteStrings
saveStore :: String -> KeyStore -> IO ()
saveStore name store = BL.writeFile (name ++ ".keystore") (compress $ encode store)

-- | Read a KeyStore from file system path, it reads the file, decodes, and decompresses the data
loadStore :: FilePath -> IO KeyStore
loadStore path = do
    file <- BL.readFile path
    return $ decode (decompress file)

-- | Convert a ByteString back into it's original Image Data Type
unpackStore :: BS.ByteString -> IO (Image PixelRGBA8)
unpackStore byteString = case decodePng byteString of
                                 Left  _   -> error "bad image"
                                 Right img -> return $ convertRGBA8 img

-- | Convert a ByteString back into it's original Image File Type 
-- You have the added option of designating where the file will be savved
unpackStore' :: String -> Image PixelRGBA8 -> IO ()
unpackStore' unpackPath bytesS = writePng unpackPath bytesS

-- | Similar to unpackStore' except that it will turn an entire KeyStore record into it's 
-- Original Image File Type(s) and save the result at a designated file save path 
autoUnpack :: String -> KeyStore -> IO ()
autoUnpack savePath ks = do
    createDirectoryIfMissing False savePath
    let conts = map (\x -> (unpack $ fst x,  convertRGBA8 $ fromRight (error "bad") (decodePng $ snd x))) (_contents ks)
    mapM_ (\x ->  unpackStore' (savePath ++ "/" ++ fst x ++ ".png") $ snd x) conts

-- | Load an Image as a ByteString via Path
loadFile :: FilePath -> IO BS.ByteString
loadFile path = BS.readFile path

-- | Load a directory of Images as ByteStrings via FilePath
loadDirectory :: FilePath -> IO [BS.ByteString]
loadDirectory folderPath = do
  directory <- listDirectory folderPath
  mapM (BS.readFile . (folderPath ++)) directory

-- | Pass a List of ByteStrings (this is intended to work with loadImageDirectory), and a String for a naming scheme (ie: 'S' results in ['S0'..]) 
-- Example (Loading a Directory all at once):
-- @
--     assets <- loadDirectory "./assets"
--     saveStore "./test/testSet" $ createKeystoreWithBulk assets "s"

--     loadedContents <- loadStore "./test/testSet.keystore"
--     autoUnpack "./results" loadedContents
-- @
createKeystoreWithBulk :: [BS.ByteString] -> Text -> KeyStore
createKeystoreWithBulk bytes nameScheme = KeyStore $ zip (map (\x -> showt $ unpack nameScheme ++ show (x :: Integer)) [0..(fromIntegral $ length bytes)]) bytes

-- | Key can be Str here which will be hashed, either way 
-- it will end up as a (String, BS.ByteString)
append :: (Text, BS.ByteString) -> KeyStore -> KeyStore
append info keystore = KeyStore $ info : (keystore ^. contents)

-- | This takes a key and returns a strict bytestring if the key is valid, Nothing is returned otherwise
search :: String -> [(Text, BS.ByteString)] -> Maybe BS.ByteString
search a = fmap snd . find ((== showt a) . fst)

-- | all the side effects of search come with this function
keyExists :: String -> [(Text, BS.ByteString)] -> Bool
keyExists a store = case search a store of
    Nothing -> False
    Just _  -> True

-- | Search the entire store for a key and delete it's associated entry
remove :: String -> KeyStore -> KeyStore
remove str keystore = KeyStore $ filter ((== showt str) . fst) (keystore ^. contents)

-- | Remove an entry by literal index in the KeyStore
remove' :: Int -> KeyStore -> KeyStore
remove' idx ls = KeyStore $ take idx (ls ^. contents) ++ drop (idx + 1) (ls ^. contents)

-- | Convenience Function for easy conversion to a Data.Map
keystoreToMap :: KeyStore -> Map Text BS.ByteString
keystoreToMap k = fromList (_contents k)

-- |  Convenience Function for easy conversion from a Data.Map
mapToKeyStore :: Map Text BS.ByteString -> KeyStore 
mapToKeyStore m = KeyStore $ toList m
