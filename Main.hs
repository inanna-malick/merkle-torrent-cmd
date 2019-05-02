-- | Haskell language pragma
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

-- | Haskell module declaration
module Main where


import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Miso framework import
import Miso
import Miso.String

-- | MAGIC BULLSHIT
import Merkle.Higher.Types
import Merkle.Higher.BitTorrent

import JavaScript.Web.XMLHttpRequest

import Control.Concurrent.MVar

import GHCJS.Types
import GHCJS.Foreign.Callback

import Language.Javascript.JSaddle.Value
import Language.Javascript.JSaddle.Marshal.String

import Data.ByteString.Lazy (toStrict)
import Data.Singletons
import Util.HRecursionSchemes

type TorrentHash = Hash 'TorrentTag

-- | Type synonym for an application model
data Model
  = NoFocus -- starting state
  | TorrentFocus TorrentHash -- after entering a hash
  deriving (Eq, Show)

data Action
  = SelectTorrent TorrentHash
  | DownloadFile FilePath
  | NoOp
  | TestPut
  | TestGet
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model         = NoFocus
    update        = updateModel
    view          = viewModel
    events        = defaultEvents        -- default delegated events
    subs          = []                   -- empty subscription list
    mountPoint    = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (SelectTorrent h) m = noEff m
updateModel (DownloadFile fp) m = do
  -- deref chunk
  pure m
updateModel NoOp m = noEff m
updateModel TestPut m = m <# do
  let t = exampleTorrent1
  r <- cataM ipfsPut t >>= toJSVal . unHash
  consoleLog r

  pure NoOp
updateModel TestGet m = m <# do
  -- downloadFile "fname" "dGVzdA==" -- 'test' in base64
  let h = Hash "QmVUM6tbpnoyto1CAktVAzsNM7JjU9G6dinMMGW1BM45Ke" :: Hash 'TorrentTag -- from put test
  r <- ipfsGet h >>= toJSVal . toJSON
  consoleLog r

  downloadFile "fname" "dGVzdA==" -- 'test' in base64

  pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ []
 [ button_ [ onClick TestPut ] [ text "test-put" ]
 , button_ [ onClick TestGet ] [ text "test-get" ]
 ]

-- this works for test file/test b64!
foreign import javascript unsafe
  "saveByteArray($1, base64ToArrayBuffer($2))"
  downloadFile :: JSString -> JSString -> IO ()

data IPFSPutResp (i :: k) = IPFSPutResp (Hash i) Int

instance FromJSON (IPFSPutResp i) where
    parseJSON = withObject "IPFS Put Resp" $ \v -> IPFSPutResp
        <$> v .: "Key"
        <*> v .: "Size"

ipfsPut :: forall i. SingI i => BitTorrent Hash i -> IO (Hash i)
ipfsPut x = do
    Just resp <- contents <$> xhrByteString req -- yolo, pattern match
    case eitherDecodeStrict resp :: Either String (IPFSPutResp i) of
      Left s -> error s
      Right (IPFSPutResp h _) -> pure h

  where
    payload = toJSString . T.decodeUtf8 $ toStrict $ encode x
    req = Request { reqMethod = POST
                  , reqURI =  "http://localhost:5001/api/v0/block/put"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = FormData [("data", StringVal payload)]
                  }


ipfsGet :: forall i. SingI i => Hash i -> IO (BitTorrent Hash i)
ipfsGet (Hash h) = do
    Just resp <- contents <$> xhrByteString req -- yolo, pattern match
    case eitherDecodeStrict resp :: Either String (BitTorrent Hash i) of
      Left s -> error s
      Right j -> pure j

  where
    req = Request { reqMethod = GET
                  , reqURI =  "http://localhost:5001/api/v0/block/get?arg=" <> toJSString (T.unpack h)
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }
