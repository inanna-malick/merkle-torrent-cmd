-- | Haskell language pragma
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.List as L
import qualified Data.Text as T

-- | MY MAGIC BULLSHIT
import Merkle.Higher.Types
import Merkle.Higher.BitTorrent
import Merkle.Higher.Store
import Merkle.Higher.Store.IPFS
import Util.HRecursionSchemes

import qualified Data.ByteString as B

-- | dir stuff
import qualified System.Directory as Dir


infura :: IPFSNode
infura = IPFSNode "https://ipfs.infura.io" 5001

main :: IO ()
main = parse >>= \case
  MkReleaseFromDir fp -> do
    let store = ipfsStore localHost :: Store IO BitTorrent
    hash <- mkRelease store fp
    putStr $ "Release Hash: " ++ show hash

  MkTorrentFromDir meta fp -> do
    files <- readFiles fp
    putStrLn "got files"
    let store = ipfsStore localHost :: Store IO BitTorrent
    torrent <- mkTorrentLazy store meta files
    hash <- sPut store torrent
    putStr $ "Torrent Hash: " ++ show hash

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


-- | Makes release from dir, janky, should figure out better format
mkRelease
  :: Store IO BitTorrent
  -> FilePath
  -- returns content from dir (and subdirs and torrent files in same)
  -> IO (Hash 'ReleaseTag)
mkRelease store path = do
    putStrLn $ "making release, readFile for: " ++ path
    dirContents <- Dir.getDirectoryContents path
    let dirContents'
          = fmap (\x -> path ++ "/" ++ x)
          -- ignore all starting with '.' (hidden files, .config, etc)
          . filter (\fn -> take 1 fn /= ".")
          $ dirContents
    elems <- fmap join $ traverse categorize dirContents'

    hasMetaFile <- Dir.doesFileExist $ path ++ "/" ++ magicReleaseMetaString
    meta <- if (hasMetaFile) then (T.pack <$> readFile magicReleaseMetaString)
                             else pure ""

    sPut store $ Release meta elems

  where
    magicReleaseMetaString = "RELEASE-META"

    handleFile p
        | L.isSuffixOf ".torrent" p = do
              t <- readFile p
              pure [(T.pack $ justTheName p, Left $ Hash $ T.pack t)]
        | justTheName p == magicReleaseMetaString =
              pure []
        | otherwise =
              fail ("should only have files containing hashes with .torrent suffix!")

    -- weird name, also does file reading..
    categorize p = do
      isFile <- Dir.doesFileExist p
      if isFile
        then handleFile p
        else do
          isDir <- Dir.doesDirectoryExist p
          if isDir
            then do
              rh <- mkRelease store p
              pure [(T.pack $ justTheName p, Right rh)]
            else fail ("file read error while making release: unexpected type at " ++ p)

    justTheName :: FilePath -> String -- hacky hax but it works - take just the name given a file path
    justTheName = reverse . takeWhile (/= '/') . reverse






-- | CMD PARSER STUFF

parse :: IO Cmd
parse = execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "torrent gen tool"
     <> header "hgit - an implementation of core git/mercurial features using recursion schemes" )

parser :: Parser Cmd
parser = subparser
       ( command "torrent" (info mkT ( progDesc "make a torrent from a directory"))
      <> command "release" (info mkR ( progDesc "cut a release from a dir (finicky format)"))
       )

  where
    mkT = MkTorrentFromDir
      <$> fmap T.pack (strArgument
          ( metavar "METADATA"
         <> help "torrent metadata"
          ))
      <*> strArgument
          ( metavar "ROOT PATH"
         <> help "path to ingest to build torrent"
          )

    mkR = MkReleaseFromDir
      <$> strArgument
          ( metavar "ROOT PATH"
         <> help "path to ingest to build release"
          )


data Cmd = MkTorrentFromDir T.Text FilePath -- todo ipfs daemon info too
         | MkReleaseFromDir FilePath
