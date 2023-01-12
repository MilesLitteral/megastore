{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances #-}

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
    , module Prisms
    , module Log
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
import Control.Lens
import Control.Lens.TH

import System.Directory (listDirectory)

newtype KeyStore = KeyStore {_contents :: [(String, BS.ByteString)]} deriving(Ord, Eq, Show)

instance Binary KeyStore where
      put (KeyStore contents) = do put (map ((\x -> (show $ hash $ fst x, snd x))) contents)
      get = do t <- get 
               return (KeyStore t)

makeLenses ''KeyStore
makePrisms ''KeyStore

saveStore :: String -> KeyStore -> IO ()
saveStore name store = BL.writeFile (name ++ ".keystore") (compress $ encode (store))

loadStore :: FilePath -> IO KeyStore
loadStore path = do 
    file <- BL.readFile path
    return $ decode (decompress file)

unpackStore :: BS.ByteString -> IO (Image PixelRGBA8)
unpackStore byteString = do
    case (decodePng byteString) of
            Left  _   -> error "bad image"
            Right img -> return $ convertRGBA8 img

unpackStore' :: String -> Image PixelRGBA8 -> IO ()
unpackStore' unpackPath bytesS = do writePng unpackPath bytesS

autoUnpack :: String -> KeyStore -> IO ()
autoUnpack savePath ks = do
    createDirectoryIfMissing False savePath
    let conts = map (\x -> (fst x,  convertRGBA8 $ fromRight (error "bad") (decodePng $ snd x))) (_contents ks)
    mapM_ (\x ->  unpackStore' (savePath ++ "/" ++ (fst x) ++ ".png") $ snd x) conts

loadImage :: FilePath -> IO (BS.ByteString)
loadImage path = do BS.readFile path

loadImageDirectory :: FilePath -> IO [BS.ByteString]
loadImageDirectory folderPath = do
  directory <- listDirectory folderPath
  return $ (\path -> BS.inlinePerformIO $ BS.readFile $ folderPath ++ path) <$> directory

createKeystoreWithBulk :: [BS.ByteString] -> String -> [(String, BS.ByteString)] --KeyStore
createKeystoreWithBulk bytes nameScheme = zip (map (\x -> nameScheme ++ show x) [0..(fromIntegral $ length bytes)]) bytes

-- Key can be Str here or made into a hash, either way 
-- it will end up as a (String, BS.ByteString)
append :: (String, BS.ByteString) -> KeyStore -> KeyStore
append info keystore = KeyStore $ [info] ++ (keystore ^. contents) 

-- CAUTION: This takes the ORIGINAL string which is tested against a hashed key
-- Do not use hashed strings on it as it will result in Nothing every time
search :: String -> [(String, BS.ByteString)] -> Maybe (BS.ByteString)
search a = fmap snd . find ((== show (hash a)) . fst)

-- WARNING: This expects the literal hash as a key
-- this is an internal function it's not for user use
search' :: String -> [(String, BS.ByteString)] -> Maybe (BS.ByteString)
search' a = fmap snd . find ((== a) . fst)

-- all the side effects of search come with this function
keyExists :: String -> [(String, BS.ByteString)] -> Bool
keyExists a store = case (search a store) of
    Nothing -> False
    Just _  -> True

-- all the side effects of search' come with this function
keyExists' :: String -> [(String, BS.ByteString)] -> Bool
keyExists' a store = case (search' a store) of
    Nothing -> False
    Just _  -> True

remove :: String -> KeyStore -> KeyStore
remove str keystore = KeyStore $ filter ((== show str) . fst) (keystore ^. contents)

remove' :: Int -> KeyStore -> KeyStore
remove' idx ls = KeyStore $ take idx (ls ^. contents) ++ drop (idx + 1) (ls ^. contents)

last' :: [a] -> a
last' []  = error "no empty lists allowed"
last' [a] = a
last' xs  = xs !! (length xs - 1)
