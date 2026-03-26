10module Spec (main) where

import MegaStore
import Data.Text (pack)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    a1  <- loadFile "./assets/s1.png"
    a2  <- loadFile "./assets/s2.png"
    a3  <- loadFile "./assets/s3.png"
    a4  <- loadFile "./assets/s4.png"
    a5  <- loadFile "./assets/s5.png"

    let testSet = MegaStore [(pack "s1", a1), (pack "s2", a2), (pack "s3", a3), (pack "s4", a4), (pack "s5", a5)]
    createDirectoryIfMissing False "./test"
    saveStore "./test/testSet" testSet -- keystore of images
    print "Loading MegaStore testSet"
    loadedContents <- loadStore "./test/testSet.megastore"
    print "autoUnpack into ./test"
    autoUnpack "./test" loadedContents
    print "autoUnpack complete check folder"
    
mainV :: IO ()
mainV = do
    a1 <- loadFile "granite.gguf"

    -- convert to vector
    let vec = byteStringToVector a1

    -- project down to k dimensions (e.g., k = 64)
    projected <- jllProject 64 vec

    -- convert back to ByteString
    let quantized = vectorToByteString projected

    -- store in MegaStore
    let vectorTestSetKV = MegaStore [(pack "granite-4-kv", quantized)]
    createDirectoryIfMissing False "./test"
    saveStore "./test/vectorTestSetKV" vectorTestSetKV
    putStrLn "Stored MegaStore with JLL projection applied"

💡 Key point: JLL isn’t automatic—it only happens if you explicitly convert byt