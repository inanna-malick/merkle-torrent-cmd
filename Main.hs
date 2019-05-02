-- | Haskell language pragma
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

-- | Haskell module declaration
module Main where


import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString.Base64 as Base64


-- | Miso framework import
import Miso
import Miso.String

-- | MAGIC BULLSHIT
import Merkle.Higher.Types
import Merkle.Higher.BitTorrent

import JavaScript.Web.XMLHttpRequest

import JavaScript.TypedArray.ArrayBuffer

import Control.Concurrent.MVar


import GHCJS.Buffer -- weird magic

import GHCJS.Types
import GHCJS.Foreign.Callback

import Language.Javascript.JSaddle.Value
import Language.Javascript.JSaddle.Marshal.String


import Data.ByteString as BS (ByteString, pack, useAsCStringLen)

import Data.ByteString.Lazy (toStrict)
import Data.Singletons
import Util.HRecursionSchemes

import Foreign.Ptr (Ptr)

import GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)

import GHCJS.DOM.Types (noBlobPropertyBag)

import GHCJS.DOM.Blob (newBlob, Blob(..))



type TorrentHash = Hash 'TorrentTag
type TorrentLayer = BitTorrent Hash 'TorrentTag



-- | Type synonym for an application model
data Model
  = NoFocus -- starting state
  | TorrentFocus TorrentLayer -- after entering a hash
  deriving (Eq)

data Action
  = SelectTorrent TorrentHash
  | FocusTorrent TorrentLayer -- once torrent dl done
  | DownloadFile FilePath
  | NoOp
  deriving (Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model         = NoFocus
    update        = updateModel
    view          = viewModel
    events        = defaultEvents -- default delegated events
    subs          = []            -- empty subscription list
    mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (SelectTorrent h) m = m <# do

  -- let t = exampleTorrent1
  -- r <- cataM ipfsPut t >>= toJSVal . unHash
  -- consoleLog r

  t <- ipfsGet h
  pure $ FocusTorrent t -- todo: should handle failure/not found case gracefully, eventually
updateModel (FocusTorrent t) m = noEff $ TorrentFocus t
updateModel (DownloadFile fp) NoFocus = noEff NoFocus -- just no-op? should never happen..
updateModel (DownloadFile fp) m@(TorrentFocus (Torrent _ files)) = m <# do
  Chunk bytes <- case (lookup fp files) of
    Nothing -> error "file not found, fail - should never happen.."
    Just h -> ipfsGet h

  -- hax, convert back to b64 as string to get bytrestring across js ffi gap
  let bytes' = T.unpack $ T.decodeLatin1 (Base64.encode bytes)

  downloadFile (toJSString fp) (toJSString bytes')
  pure NoOp
updateModel NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel NoFocus = div_ []
-- click to run on hardcoded torrent
 [ button_ [ onClick $ SelectTorrent $ Hash "QmZ9BwD86pWrg8MNqWjRc85zyTWzN7vaxQpLDV8X8EoJWk"]
           [ text "pick precoded torrent hash" ]
 ]
viewModel (TorrentFocus (Torrent m fs)) = div_ [] $ [text $ toJSString m] ++ fs'
  where
    fs' = fmap mkF fs
    mkF (fp, c) = button_ [ onClick $ DownloadFile fp]
                          [ text $ toJSString fp]

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
