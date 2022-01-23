{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command parsing.
module Commands
  ( Roll (..),
    Command (..),
    command,
  )
where

import Control.Applicative hiding (many, some)
import Data.Char (chr)
import Data.Functor
import Data.Text
import Data.Time
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf

type Parser = Parsec Void Text

prefix :: Parser Text
prefix = string "chad"

data Roll
  = Standard Int Int
  | KeepHighest Int Int Int
  | KeepLowest Int Int Int
  deriving (Eq)

instance Show Roll where
  show (Standard n d) = printf "%dd%d" n d
  show (KeepHighest n d k) = printf "%dd%d (keep highest %d)" n d k
  show (KeepLowest n d k) = printf "%dd%d (keep lowest %d)" n d k

-- | Data type to encapsulate all possible `chad` commands
data Command
  = CmdRoll Int Roll
  | CmdTime TimeZone
  | CmdHelp
  deriving (Show, Eq)

hidehspace1 :: Parser ()
hidehspace1 = hidden hspace1 -- <?> "more stuff!"

-- | Papa parser for all valid commands. Simply tries one at a time until one works.
command :: Parser Command
command = prefix *> hidehspace1 *> (time <|> roll <|> help) <* hidden space <* eof

-- | Parses a "time" command
time :: Parser Command
time = string "time" *> hidehspace1 *> (CmdTime <$> timezone)
  where
    zones =
      choice $
        Prelude.map
          string'
          ["UTC", "UT", "GMT", "EST", "EDT", "CST", "CDT", "MST", "MDT", "PST", "PDT"]
    timezone :: Parser TimeZone
    timezone = read . unpack <$> zones <?> "valid timezone"

-- | Parses a "roll" command
roll :: Parser Command
roll =
  string "roll"
    *> hidehspace1
    *> (CmdRoll <$> option 1 (try $ integer <* hidehspace1) <*> allRolls)
  where
    allRolls = do
      n <- integer
      k <- char' 'd' *> integer
      option (Standard n k) $ KeepHighest n k <$> highest <|> KeepLowest n k <$> lowest
      where
        highest = string "kh" *> integer <?> "roll in the form [N]d[D]kh[K]"
        lowest = string "kl" *> integer <?> "roll in the form [N]d[D]kl[K]"

-- | Parses a "help" command
help :: Parser Command
help = string "help" >> return CmdHelp

-- Utility parsers
integer :: Parser Int
integer = read <$> some digitChar <?> "an integer"
