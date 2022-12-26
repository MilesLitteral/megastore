module Main where

import Lib
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
    orderedMessageS  LOG_HEAD $ "First: "  ++ (show . fromJust $ search'  "first"  $ _contents united)
    warnMessageS     LOG_BODY $ "Key Exists? (first): " ++ (show $ keyExists'  "first"  $ _contents united)
    orderedMessageS  LOG_BODY $ "Second: " ++ (show . fromJust $ search'  "second" $ _contents united)
    orderedMessageS  LOG_BODY $ "Third: "  ++ (show . fromJust $ search'  "third"  $ _contents united)
    -- (warnMessageS LOG_BODY $ show $ _KeyStore ##? united)
    -- (warnMessageS LOG_BODY $ show $ (map (\x -> (fst x, Lib.null $ snd x)) (_contents united)))
    -- eventMessageS LOG_BODY "TEST CASE BELOW, STUDY CAREFULLY"
    -- (warnMessageS LOG_BODY $ show $ ((_contents testSet)))
    -- warnMessageS  LOG_TAIL $ show $ (_KeyStore ##? testSet)
    saveStore "./testGround/testA"   united  -- empty keystore
    saveStore "./testGround/testSet" testSet -- keystore of images
    warnMessageS  LOG_TAIL $ "Unpack Keystore"
    loadedContents <- loadStore "./testGround/testSet.keystore"
    autoUnpack "./testGround" loadedContents
    -- eventMessageS LOG_BODY $ show $ loadedContents