-- | Haskell language pragma
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

-- | Haskell module declaration
module Main where



import Options.Applicative
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)

import Control.Monad (join)
import Data.Aeson

-- | MY MAGIC BULLSHIT
import Merkle.Higher.Types
import Merkle.Higher.BitTorrent
import Merkle.Higher.Store
import Merkle.Higher.Store.IPFS
import Util.HRecursionSchemes

import qualified Data.ByteString as B

-- | dir stuff
import qualified System.Directory as Dir


main :: IO ()
main = parse >>= \(MkTorrentFromDir meta fp) -> do
  files <- readFiles fp
  putStrLn "got files"
  let store = ipfsStore eitherDecode encode localHost :: Store IO BitTorrent
  torrent <- mkTorrentLazy store meta files
  hash <- sPut store torrent
  print hash

readFiles
  :: FilePath
  -> IO [IO (FilePath, ByteString)]
readFiles path = do
    putStrLn $ "readFile for: " ++ path
    dirContents <- Dir.getDirectoryContents path
    let dirContents'
          = fmap (\x -> path ++ "/" ++ x)
          -- ignore all starting with '.' (hidden files, .config, etc)
          . filter (\fn -> take 1 fn /= ".")
          $ dirContents
    fmap join $ traverse categorize dirContents'

  where
    -- weird name, also does file reading..
    categorize p = do
      isFile <- Dir.doesFileExist p
      if isFile
        then do
         pure $ pure $ do
           -- note: this hangs if it tries to read own running process exe (not sure why?)
           putStrLn $ "delayed read for: " ++ p
           bs <- B.readFile p
           pure (p, bs)
        else do
          isDir <- Dir.doesDirectoryExist p
          if isDir
            then readFiles p
            else fail ("file read error: unexpected type at " ++ p)




-- | CMD PARSER STUFF

parse :: IO Cmd
parse = execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "torrent gen tool"
     <> header "hgit - an implementation of core git/mercurial features using recursion schemes" )

parser :: Parser Cmd
parser = MkTorrentFromDir
      <$> strArgument
          ( metavar "METADATA"
         <> help "some string info type situation"
          )
      <*> strArgument
          ( metavar "ROOT PATH"
         <> help "path to ingest"
          )

data Cmd = MkTorrentFromDir String FilePath -- todo ipfs daemon info too
