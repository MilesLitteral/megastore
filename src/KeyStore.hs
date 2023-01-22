{-# LANGUAGE RankNTypes, PolyKinds, TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances #-}

{-|
Module      : KeyStore
Description : Short description
Copyright   : (c) Miles J. Litteral 2023
License     : GPL-3
Maintainer  : mandaloe2@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-} 
module KeyStore
    ( KeyStore(..)
    , _KeyStore
    , saveStore
    , loadStore
    , loadImage
    , loadImageDirectory
    , createKeystoreWithBulk
    , fromJust
    , append
    , search
    , search'
    , keyExists
    , keyExists'
    , remove
    , remove'
    , unpackStore
    , unpackStore'
    , autoUnpack
    , last'
    ) where

import Log
import Prisms
import Codec.Picture
import Codec.Compression.GZip
import GHC.Utils.Misc (uncurry3)

import Data.List 
import Data.Maybe
import Data.Either
import Data.Binary
import Data.Hashable
import System.Random
import System.Directory
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import System.Directory (listDirectory)

import GHC.Exts (TYPE)
import Data.Maybe   ()
import Data.Monoid  ( First )
import Control.Monad.Representable.Reader
import Control.Lens
import Control.Lens.TH

-- | The Data Type itself
newtype KeyStore = KeyStore {_contents :: [(String, BS.ByteString)]} deriving(Ord, Eq, Show)

instance Binary KeyStore where
      put (KeyStore contents) = do put (map ((\x -> (show $ hash $ fst x, snd x))) contents)
      get = do t <- get 
               return (KeyStore t)

makeLenses ''KeyStore
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
loadImage :: FilePath -> IO (BS.ByteString)
loadImage path = do BS.readFile path

-- | Load a directory of Images as ByteStrings via FilePath
loadImageDirectory :: FilePath -> IO [BS.ByteString]
loadImageDirectory folderPath = do
  directory <- listDirectory folderPath
  return $ (\path -> BS.inlinePerformIO $ BS.readFile $ folderPath ++ path) <$> directory

-- | Pass a List of ByteStrings (this is intended to work with loadImageDirectory), and a String for a naming scheme (ie: 'S' results in ['S0'..]) 
createKeystoreWithBulk :: [BS.ByteString] -> String -> [(String, BS.ByteString)] --KeyStore
createKeystoreWithBulk bytes nameScheme = KeyStore $ zip (map (\x -> nameScheme ++ show x) [0..(fromIntegral $ length bytes)]) bytes

-- | Key can be Str here which will be hashed, either way 
-- it will end up as a (String, BS.ByteString)
append :: (String, BS.ByteString) -> KeyStore -> KeyStore
append info keystore = KeyStore $ [info] ++ (keystore ^. contents) 

-- | This takes the ORIGINAL string which is tested against a hashed key
-- Do not use hashed strings on it as it will result in Nothing every time
search :: String -> [(String, BS.ByteString)] -> Maybe (BS.ByteString)
search a = fmap snd . find ((== show (hash a)) . fst)

-- | This expects the literal hash as a key
search' :: String -> [(String, BS.ByteString)] -> Maybe (BS.ByteString)
search' a = fmap snd . find ((== a) . fst)

-- | all the side effects of search come with this function
keyExists :: String -> [(String, BS.ByteString)] -> Bool
keyExists a store = case (search a store) of
    Nothing -> False
    Just _  -> True

-- | all the side effects of search' come with this function
keyExists' :: String -> [(String, BS.ByteString)] -> Bool
keyExists' a store = case (search' a store) of
    Nothing -> False
    Just _  -> True

-- | Search the entire store for a Key and delete it's associated entry
remove :: String -> KeyStore -> KeyStore
remove str keystore = KeyStore $ filter ((== show str) . fst) (keystore ^. contents)

-- | Remove an entry by literal index 
remove' :: Int -> KeyStore -> KeyStore
remove' idx ls = KeyStore $ take idx (ls ^. contents) ++ drop (idx + 1) (ls ^. contents)

-- | The opposite of head, will return the final value of a list
last' :: [a] -> a
last' []  = error "no empty lists allowed"
last' [a] = a
last' xs  = xs !! (length xs - 1)