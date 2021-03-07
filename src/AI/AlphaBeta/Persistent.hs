{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.AlphaBeta.Persistent
  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Catch (catch, SomeException)
import Control.Concurrent.STM
import qualified Data.HashPSQ as PQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Data.Text.Format.Heavy
import Data.Store
import Data.Bits.Coded
import Data.Bits.Coding
import Data.Bytes.Put
import System.Clock
import Text.Printf
import GHC.Generics
import System.Posix.Types
import qualified System.IO.RandomAccessFile as File
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import qualified Core.Monitoring as Monitoring
import AI.AlphaBeta.Types

maxPieces :: Integer
maxPieces = 30

encodeBoard :: Board -> B.ByteString
encodeBoard board = runPutS $ runEncode $ encodeB board
  where

    encodePiece Nothing =
      putBit False
    encodePiece (Just (Piece Man First)) = do
      putBit True
      putBit False
      putBit False
    encodePiece (Just (Piece Man Second)) = do
      putBit True
      putBit False
      putBit True
    encodePiece (Just (Piece King First)) = do
      putBit True
      putBit True
      putBit False
    encodePiece (Just (Piece King Second)) = do
      putBit True
      putBit True
      putBit True

    encodeB b = do
      forM_ (allPieces b) encodePiece
      

sizeOf :: Data.Store.Store a => a -> ByteCount
sizeOf a =
  case Data.Store.size of
    Data.Store.VarSize fn -> fromIntegral $ fn a
    Data.Store.ConstSize n -> fromIntegral n

unexistingBlock :: IndexBlockNumber
unexistingBlock = maxBound

