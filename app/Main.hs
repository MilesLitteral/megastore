module Main where

import KeyStore
import Data.List
import GHC.Utils.IO.Unsafe
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    a1  <- loadFile "./assets/s1.png"
    a2  <- loadFile "./assets/s2.png"
    a3  <- loadFile "./assets/s3.png"
    a4  <- loadFile "./assets/s4.png"
    a5  <- loadFile "./assets/s5.png"

    let testSet = KeyStore [("s1", a1), ("s2", a2), ("s3", a3), ("s4", a4), ("s5", a5)]
    createDirectoryIfMissing False "./test"
    saveStore "./test/testSet" testSet -- keystore of images
    print "Loading KeyStore testSet"
    loadedContents <- loadStore "./test/testSet.keystore"
    print "autoUnpack into ./test"
    autoUnpack "./test" loadedContents
    print "autoUnpack complete check folder"
