{-# LANGUAGE TemplateHaskell #-}

-- | MegaStore: keyed binary archive storage.
-- The legacy on-disk format remains: gzip (Data.Binary.encode MegaStore).
module MegaStore
    ( MegaStore(..)
    , StoreMode(..)
    , saveStore
    , loadStore
    , saveStoreWith
    , loadFile
    , loadDirectory
    , createMegaStoreWithBulk
    , unpackStore
    , unpackStore'
    , autoUnpack
    , append
    , search
    , keyExists
    , remove
    , remove'
    , megastoreToMap
    , mapToMegaStore
    , jllProject
    ) where

import Codec.Picture (Image, PixelRGBA8, convertRGBA8, decodePng, writePng)
import Codec.Compression.GZip (compress, decompress)
import Control.Lens (makeLenses, (^.))
import Data.Binary (Binary(..), decodeOrFail, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Random (newStdGen, randomRs)

newtype MegaStore = MegaStore
    { _contents :: [(Text, BS.ByteString)]
    } deriving (Ord, Eq, Show)

makeLenses ''MegaStore

instance Binary MegaStore where
    put (MegaStore entries) = put entries
    get = MegaStore <$> get

-- | BinaryMode is lossless. KVMode is a research-only, lossy vector
-- projection; it does not preserve original bytes or support inverse decode.
data StoreMode = BinaryMode | KVMode Int
    deriving (Eq, Show)

type Vector = V.Vector Double

-- | Experimental JL-style sparse projection. Not an archival codec.
-- Uses a fresh random projection for each call; no seed/matrix is persisted.
jllProject :: Int -> Vector -> IO Vector
jllProject k vec
    | k < 1 = ioError (userError "jllProject: k must be positive")
    | otherwise = do
        gen <- newStdGen
        let d = V.length vec
            randomValues = V.fromList (take (k * d) (randomRs (0.0, 1.0) gen :: [Double]))
            project i j =
                let r = randomValues V.! (i * d + j)
                in if r < (1 / 6) then (-1.0)
                   else if r > (5 / 6) then 1.0 else 0.0
            compute i = sum [project i j * (vec V.! j) | j <- [0 .. d - 1]]
        pure $ V.generate k compute

byteStringToVector :: BS.ByteString -> Vector
byteStringToVector = V.fromList . map fromIntegral . BS.unpack

vectorToByteString :: Vector -> BS.ByteString
vectorToByteString vec = BS.pack $ map (fromIntegral . clamp) (V.toList vec)
  where
    clamp x = max (0 :: Int) (min 255 (round x))

-- | Explicit modes. BinaryMode is just lossless archive storage;
-- archive-level gzip is applied once in saveStore. KVMode is lossy.
saveStoreWith :: StoreMode -> String -> MegaStore -> IO ()
saveStoreWith BinaryMode path store = saveStore path store
saveStoreWith (KVMode k) path (MegaStore entries) = do
    projected <- mapM projectEntry entries
    saveStore path (MegaStore projected)
  where
    projectEntry (key, bytes) = do
        vec <- jllProject k (byteStringToVector bytes)
        pure (key, vectorToByteString vec)

-- | Appends .megastore unless already supplied.
archivePath :: FilePath -> FilePath
archivePath path
    | takeExtension path == ".megastore" = path
    | otherwise = path ++ ".megastore"

saveStore :: String -> MegaStore -> IO ()
saveStore path store = BL.writeFile (archivePath path) (compress $ encode store)

-- | Decoding errors are reported rather than using partial Data.Binary.decode.
-- Callers should still place resource limits on untrusted archives.
loadStore :: FilePath -> IO MegaStore
loadStore path = do
    bytes <- BL.readFile path
    case decodeOrFail (decompress bytes) of
        Left (_, _, err) -> ioError (userError ("MegaStore decode error: " ++ err))
        Right (_, _, store) -> pure store

unpackStore :: BS.ByteString -> IO (Image PixelRGBA8)
unpackStore bytes = case decodePng bytes of
    Left err -> ioError (userError ("Bad PNG: " ++ err))
    Right img -> pure (convertRGBA8 img)

unpackStore' :: FilePath -> Image PixelRGBA8 -> IO ()
unpackStore' = writePng

autoUnpack :: FilePath -> MegaStore -> IO ()
autoUnpack dir (MegaStore entries) = do
    createDirectoryIfMissing True dir
    mapM_ writeEntry entries
  where
    -- For untrusted keys, validate/sanitize names before writing paths.
    writeEntry (key, bytes) = do
        img <- unpackStore bytes
        unpackStore' (dir </> T.unpack key ++ ".png") img

loadFile :: FilePath -> IO BS.ByteString
loadFile = BS.readFile

loadDirectory :: FilePath -> IO [BS.ByteString]
loadDirectory dir = do
    files <- listDirectory dir
    mapM (BS.readFile . (dir </>)) files

createMegaStoreWithBulk :: [BS.ByteString] -> Text -> MegaStore
createMegaStoreWithBulk bytes prefix = MegaStore $
    zipWith (\n b -> (prefix `T.append` T.pack (show (n :: Int)), b)) [0..] bytes

append :: (Text, BS.ByteString) -> MegaStore -> MegaStore
append entry (MegaStore entries) = MegaStore (entry : entries)

-- | Exact text key lookup. To use hashed identifiers, hash the key
-- on BOTH insertion and lookup in the caller (not enforced by MegaStore).
search :: String -> [(Text, BS.ByteString)] -> Maybe BS.ByteString
search key = fmap snd . find ((== T.pack key) . fst)

keyExists :: String -> [(Text, BS.ByteString)] -> Bool
keyExists key entries = case search key entries of
    Just _ -> True
    Nothing -> False

remove :: String -> MegaStore -> MegaStore
remove key (MegaStore entries) = MegaStore $
    filter ((/= T.pack key) . fst) entries

remove' :: Int -> MegaStore -> MegaStore
remove' idx (MegaStore entries) = MegaStore $
    take idx entries ++ drop (idx + 1) entries

megastoreToMap :: MegaStore -> Map Text BS.ByteString
megastoreToMap (MegaStore entries) = Map.fromList entries

mapToMegaStore :: Map Text BS.ByteString -> MegaStore
mapToMegaStore = MegaStore . Map.toList
