{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot
  ( runBot,
  )
where

import Commands
import Control.Monad (unless, void)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Handlers (handle)
import System.Random
import Text.Megaparsec (errorBundlePretty, parse)
import Control.Monad.State
import System.IO.Unsafe (unsafePerformIO)

----------------------------------- Utilities -----------------------------------

-- | Discord bot auth token
authtoken :: Text
authtoken = pack . unsafePerformIO . readFile $ "../auth-token.txt"

-- | Integer representation of Discord permissions
permissions :: Int
permissions = 380104723520

-- | Returns True if input message is from a bot
fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

----------------------------------- Main code -----------------------------------

-- | Starts the bot
runBot :: IO ()
runBot = do
  seed <- getStdGen
  userFacingError <-
    runDiscord $
      def
        { discordToken = authtoken,
          discordOnEvent = eventHandler seed,
          discordOnEnd = putStrLn "See ya, virgins!",
          discordForkThreadForEvents = False
        }
  TIO.putStrLn userFacingError

-- | Processes a Discord event (e.g., incoming message)
eventHandler :: StdGen -> Event -> DiscordHandler ()
eventHandler seed = \case
  MessageCreate m -> unless (fromBot m) $ do
    void $ restCall (R.CreateMessage (messageChannel m) t)
    where
      t = case parse command "" (messageText m) of
        Left err -> pack $ "```You moron." ++ dropWhile (/= '\n') (errorBundlePretty err) ++ "```"
        Right cmd -> evalState (handle cmd) seed
  _ -> return ()
