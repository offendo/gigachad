{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Defines handlers for each of the different commands
module Handlers where

import Commands (Command (..), Roll (..))
import Control.Monad
import Control.Monad.State
import Data.List (intercalate, sort, sortOn)
import Data.Ord
import Data.Text
  ( Text,
    pack,
    toLower,
    unpack,
  )
import System.Random
import System.Random.Stateful (StatefulGen)
import Text.Printf

handle :: RandomGen g => Command -> State g Text
handle = \case
  CmdHelp -> return "I am `GigaChad`. Summon me with\n```chad <cmd> <args>```\n"
  x@(CmdRoll n r) -> do
    gets (pack . show . roll r)
  x@(CmdTime z) -> return . pack $ printf "Getting time for %s" (show z)

data RollResult = MkRollResult {cmd :: Roll, nums :: [Int], discarded :: [Int]}
  deriving (Eq)

instance Show RollResult where
  show (MkRollResult r nums disc) = printf "__Rolling %s:__ \n`%d` from %s" (show r) total allDice
    where
      sNums = if null nums then "" else "**" ++ intercalate ", " (map show nums) ++ "**"
      sDisc = if null disc then "" else "~~" ++ intercalate ", " (map show disc) ++ "~~"
      allDice = intercalate ", " [sNums, sDisc]
      total = sum nums

-- | Executes a Roll command given a random seed `g`
roll :: RandomGen g => Roll -> g -> RollResult
roll = \case
  r@(Standard n d) -> uncurry (MkRollResult r) . (,[]) . take n . randomRs (1, d)
  r@(KeepHighest n d k) -> uncurry (MkRollResult r) . splitAt k . sortOn Down . take n . randomRs (1, d)
  r@(KeepLowest n d k) -> uncurry (MkRollResult r) . splitAt k . sort . take n . randomRs (1, d)
