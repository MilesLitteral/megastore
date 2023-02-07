{-# LANGUAGE RankNTypes, PolyKinds, TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances #-}

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
    -- ** Prisms #Prisms#
    , _KeyStore
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
    , search'
    , keyExists
    , keyExists'
    , remove
    , remove'
    , last'
    ) where

import Codec.Picture
import Codec.Compression.GZip

import Data.List 
import Data.Maybe
import Data.Either
import Data.Binary
import Data.Text
import Data.Hashable
import System.Random
import System.Directory
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Control.Lens.TH
import System.IO.Unsafe

-- | The KeyStore Data Type itself, fundamentally it is a List of Tuples
newtype KeyStore = 
    KeyStore {
        _contents :: [(Text, BS.ByteString)] -- ^ the contents of the KeyStore, while made for images it is acknowledged anything that satisfies the constraint/assertion may be a KeyStore
    } deriving(Ord, Eq, Show)

-- | The KeyStore Data Type's instance for serializing the data structure to file type,
instance Binary KeyStore where
      put (KeyStore contents) = do put (map ((\x -> (show $ hash $ fst x, snd x))) contents)
      get = do t <- get 
               return (KeyStore t)

instance At   KeyStore 
instance Ixed KeyStore

makeLenses ''KeyStore
-- | The _KeyStore prism is for manipulating KeyStores with the Lens library, mainly as a form of short hand convenience
-- or more importantly as a means of data manipulation without all the back and fourth of packing and unpacking
-- there are also At and Ixed instances available 
makePrisms ''KeyStore

-- | Writes a KeyStore to physical memory, it does so via Data.ByteString.Lazy.WriteFile
-- Where the data is compressed and encoded to Strict ByteStrings
saveStore :: String -> KeyStore -> IO ()
saveStore name store = BL.writeFile (name ++ ".keystore") (compress $ encode (store))

-- | Read a KeyStore from file system path, it reads the file, decodes, and decompresses the data
-- CAUTION: there is a known bug of sorts where sometimes the files will come out slightly bigger than when they
-- went in (~1kb - 2kbs+), this is being addressed 
loadStore :: FilePath -> IO KeyStore
loadStore path = do 
    file <- BL.readFile path
    return $ decode (decompress file)

-- | Convert a ByteString back into it's original Image Data Type
unpackStore :: BS.ByteString -> IO (Image PixelRGBA8)
unpackStore byteString = do
    case (decodePng byteString) of
            Left  _   -> error "bad image"
            Right img -> return $ convertRGBA8 img

-- | Convert a ByteString back into it's original Image File Type 
-- You have the added option of designating where the file will be savved
unpackStore' :: String -> Image PixelRGBA8 -> IO ()
unpackStore' unpackPath bytesS = do writePng unpackPath bytesS

-- | Similar to unpackStore' except that it will turn an entire KeyStore record into it's 
-- Original Image File Type(s) and save the result at a designated file save path 
autoUnpack :: String -> KeyStore -> IO ()
autoUnpack savePath ks = do
    createDirectoryIfMissing False savePath
    let conts = map (\x -> (fst x,  convertRGBA8 $ fromRight (error "bad") (decodePng $ snd x))) (_contents ks)
    mapM_ (\x ->  unpackStore' (savePath ++ "/" ++ (fst x) ++ ".png") $ snd x) conts

-- | Load an Image as a ByteString via Path
loadFile :: FilePath -> IO (BS.ByteString)
loadFile path = do BS.readFile path

-- | Load a directory of Images as ByteStrings via FilePath
loadDirectory :: FilePath -> IO [BS.ByteString]
loadDirectory folderPath = do
  directory <- listDirectory folderPath
  mapM (BS.readFile) $ map (\path -> folderPath ++ path) directory

-- | Pass a List of ByteStrings (this is intended to work with loadImageDirectory), and a String for a naming scheme (ie: 'S' results in ['S0'..]) 
-- Example (Loading a Directory all at once):
-- @
--     assets <- loadDirectory "./assets"
--     saveStore "./test/testSet" $ createKeystoreWithBulk assets "s"

--     loadedContents <- loadStore "./test/testSet.keystore"
--     autoUnpack "./results" loadedContents
-- @
createKeystoreWithBulk :: [BS.ByteString] -> Text -> KeyStore
createKeystoreWithBulk bytes nameScheme = KeyStore $ zip (map (\x -> nameScheme ++ show x) [0..(fromIntegral $ length bytes)]) bytes

-- | Key can be Str here which will be hashed, either way 
-- it will end up as a (String, BS.ByteString)
append :: (Text, BS.ByteString) -> KeyStore -> KeyStore
append info keystore = KeyStore $ [info] ++ (keystore ^. contents) 

-- | This takes the ORIGINAL string which is tested against a hashed key
-- Do not use hashed strings on it as it will result in Nothing every time
search :: String -> [(Text, BS.ByteString)] -> Maybe (BS.ByteString)
search a = fmap snd . find ((== show (hash a)) . fst)

-- | This expects the literal hash as a key
search' :: String -> [(Text, BS.ByteString)] -> Maybe (BS.ByteString)
search' a = fmap snd . find ((== a) . fst)

-- | all the side effects of search come with this function
keyExists :: String -> [(Text, BS.ByteString)] -> Bool
keyExists a store = case (search a store) of
    Nothing -> False
    Just _  -> True

-- | all the side effects of search' come with this function
keyExists' :: String -> [(Text, BS.ByteString)] -> Bool
keyExists' a store = case (search' a store) of
    Nothing -> False
    Just _  -> True

-- | Search the entire store for a Key and delete it's associated entry
remove :: String -> KeyStore -> KeyStore
remove str keystore = KeyStore $ filter ((== show str) . fst) (keystore ^. contents)

-- | Remove an entry by literal index 
remove' :: Int -> KeyStore -> KeyStore
remove' idx ls = KeyStore $ take idx (ls ^. contents) ++ drop (idx + 1) (ls ^. contents)
