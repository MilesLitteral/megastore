module Main where

import KeyStore
import Data.List
import GHC.Utils.IO.Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    a1  <- loadImage "./assets/s1.png"
    a2  <- loadImage "./assets/s2.png"
    a3  <- loadImage "./assets/s3.png"
    a4  <- loadImage "./assets/s4.png"
    a5  <- loadImage "./assets/s5.png"
    a6  <- loadImage "./assets/s6.png"
    a7  <- loadImage "./assets/s7.png"
    a8  <- loadImage "./assets/s8.png"
    a9  <- loadImage "./assets/s9.png"
    a10 <- loadImage "./assets/s10.png"

    let united  = KeyStore [("first", BS.empty), ("second", BS.empty), ("third", BS.empty)]
        testSet = KeyStore [("s1", a1), ("s2", a2), ("s3", a3), ("s4", a4), ("s5", a5), ("s6", a6), ("s7", a7), ("s8", a8), ("s9", a9), ("s10", a10)]
    print "Saving KeyStore: United as testA (empty store)"
    saveStore "./testGround/testA"   united  -- empty keystore
    saveStore "./testGround/testSet" testSet -- keystore of images
    print "Loading KeyStore testSet"
    loadedContents <- loadStore "./testGround/testSet.keystore"
    print "autoUnpack into ./testGround"
    autoUnpack "./testGround" loadedContents
    print "autoUnpack complete check folder"
