{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.BootstrapSpec
-- Description : Tests for CBOR bootstrap encode/decode
-- License     : Apache-2.0
module Cardano.MPFS.BootstrapSpec
    ( spec
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import Cardano.MPFS.Bootstrap
    ( BootstrapHeader (..)
    , encodeBootstrapFile
    , foldBootstrapEntries
    )

spec :: Spec
spec = describe "Bootstrap" $ do
    it "roundtrips empty file" $ do
        let hdr = BootstrapHeader 0 Nothing
            pairs = [] :: [(ByteString, ByteString)]
        (hdr', pairs') <- roundtrip hdr pairs
        hdr' `shouldBe` hdr
        pairs' `shouldBe` pairs

    it "roundtrips with block hash" $ do
        let hash = BS.replicate 32 0xAB
            hdr = BootstrapHeader 42 (Just hash)
            pairs =
                [ ("key1", "val1")
                , ("key2", "val2")
                ]
        (hdr', pairs') <- roundtrip hdr pairs
        hdr' `shouldBe` hdr
        pairs' `shouldBe` pairs

    it "roundtrips with many entries" $ do
        let hdr = BootstrapHeader 1000 Nothing
            pairs =
                [ ( "k" <> bshow i
                  , "v" <> bshow i
                  )
                | i <- [1 :: Int .. 100]
                ]
        (hdr', pairs') <- roundtrip hdr pairs
        hdr' `shouldBe` hdr
        pairs' `shouldBe` pairs

-- | Encode then decode via streaming fold.
roundtrip
    :: BootstrapHeader
    -> [(ByteString, ByteString)]
    -> IO
        ( BootstrapHeader
        , [(ByteString, ByteString)]
        )
roundtrip hdr pairs =
    withSystemTempDirectory "bootstrap" $ \dir -> do
        let fp = dir <> "/test.cbor"
        encodeBootstrapFile fp hdr pairs
        hdrRef <- newIORef hdr
        pairsRef <- newIORef ([] :: [(ByteString, ByteString)])
        foldBootstrapEntries
            fp
            (\h -> modifyIORef' hdrRef (const h))
            ( \k v ->
                modifyIORef'
                    pairsRef
                    (<> [(k, v)])
            )
        (,)
            <$> readIORef hdrRef
            <*> readIORef pairsRef

-- | Show an 'Int' as a 'ByteString'.
bshow :: Int -> ByteString
bshow =
    BS.pack
        . map (fromIntegral . fromEnum)
        . show
