module Main where

import KeyStore
import Data.List
import GHC.Utils.IO.Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    a1 <- loadImage "./assets/s1.png"
    a2 <- loadImage "./assets/s2.png"
    a3 <- loadImage "./assets/s3.png"
    a4 <- loadImage "./assets/s4.png"

    let united  = KeyStore [("first", BS.empty), ("second", BS.empty), ("third", BS.empty)]
        testSet = KeyStore [("s1", a1), ("s2", a2), ("s3", a3), ("s4", a4)]
    saveStore "./testGround/testA"   united  -- empty keystore
    saveStore "./testGround/testSet" testSet -- keystore of images
    loadedContents <- loadStore "./testGround/testSet.keystore"
    autoUnpack "./testGround" loadedContents
    KeyStore.log 0 LOG_INFO "Test Log Neo"
    KeyStore.log 1 LOG_WARNING   "Test Log Neo"
    KeyStore.log 1 LOG_EXCEPTION "Test Log Neo"
    KeyStore.log 2 LOG_INFO "Test Log Neo"

    -- eventMessageS LOG_BODY $ show $ loadedContents