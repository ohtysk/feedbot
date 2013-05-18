--ircbot.hs

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes, RecordWildCards #-}
module Main where

import Data.Time.Format           (formatTime)
import Data.Time.LocalTime        (utcToLocalTime, getTimeZone)
import Data.Time.Clock            (getCurrentTime)
import Control.Concurrent         (killThread, forkIO, threadDelay)
import Control.Concurrent.Chan    (Chan)
import Control.Concurrent.STM     (atomically)
import Control.Concurrent.STM.TVar(TVar, newTVar, readTVar, writeTVar)
import Control.Monad              (liftM, forever)
import Control.Monad.IO.Class     (liftIO)
import Data.Set                   (Set, insert, toList)
import Network                    (PortID(..))
import Network.IRC                (Message)
import Network.IRC.Commands       (privmsg)
import Network.IRC.Bot.BotMonad   (BotMonad(..))
import Network.IRC.Bot.Log        (LogLevel(..), nullLogger, stdoutLogger)
import Network.IRC.Bot.Part.Dice  (dicePart)
import Network.IRC.Bot.Part.Ping  (pingPart)
import Network.IRC.Bot.Part.NickUser (nickUserPart)
import Network.IRC.Bot.Part.Channels (initChannelsPart)
import System.Console.GetOpt      (OptDescr(..), ArgDescr(..), getOpt, ArgOrder(..), usageInfo)
import System.Environment         (getArgs, getProgName)
import System.Exit                (exitFailure)
import System.Cmd                 (system)
import System.Locale              (defaultTimeLocale)
import Data.Maybe                 (fromJust)
import TimerBot                   (BotConf(..), User(..), nullBotConf, simpleBot, timerBot)
import HttpGet                    (httpGet)
import Text.Feed.Query            (getItemTitle, getItemAuthor)
import FeedItems                  (getUpdatedItems, saveUpdated, lastUpdated, getItemDateString, getUrlFromItem)

data Flag
    = BotConfOpt { unBotConfOpt :: (BotConf -> BotConf) }
      
botOpts :: [OptDescr Flag]
botOpts =
  [ Option [] ["irc-server"] (ReqArg setIrcServer "hostname or IP") "irc server to connect to"
  , Option [] ["port"]       (ReqArg setPort      "port")           "port to connect to on server"
  , Option [] ["nick"]       (ReqArg setNick      "name")           "irc nick"
  , Option [] ["username"]   (ReqArg setUsername  "username")       "ident username"
  , Option [] ["hostname"]   (ReqArg setHostname  "hostname")       "hostname of machine bot is connecting from"
  , Option [] ["realname"]   (ReqArg setRealname  "name")           "bot's real name"
  , Option [] ["cmd-prefix"] (ReqArg setCmdPrefix "prefix")         "prefix to bot commands (e.g., ?, @, bot: )"
  , Option [] ["channel"]    (ReqArg addChannel   "channel name")   "channel to join after connecting. (can be specified more than once to join multiple channels)"
  , Option [] ["log-level"]  (ReqArg setLogLevel  "debug, normal, important, quiet") "set the logging level"
  , Option [] ["limit"]      (ReqArg setLimit     "int,int")        "enable rate limiter. burst length, delay in microseconds"
  , Option [] ["feed-url"]   (ReqArg setNone      "hoge")           "huga"
  , Option [] ["feed-span"]  (ReqArg setNone      "iiii")           "jjjj"
  ]
  where
    setIrcServer n = BotConfOpt $ \c -> c { host = n, user = (user c) { servername = n } }
    setPort str    = BotConfOpt $ \c -> c { port = PortNumber (fromIntegral $ ((read str) :: Integer)) }
    setNick n      = BotConfOpt $ \c -> c { nick = n }
    setUsername n  = BotConfOpt $ \c -> c { user = (user c) { username = n } }
    setHostname n  = BotConfOpt $ \c -> c { user = (user c) { hostname = n } }
    setRealname n  = BotConfOpt $ \c -> c { user = (user c) { realname = n } }
    setCmdPrefix p = BotConfOpt $ \c -> c { commandPrefix = p }
    addChannel ch  = BotConfOpt $ \c -> c { channels = insert ch (channels c) }
    setLogLevel l  = BotConfOpt $ \c ->
      case l of
        "debug"     -> c { logger = stdoutLogger Debug }
        "normal"    -> c { logger = stdoutLogger Normal }
        "important" -> c { logger = stdoutLogger Important }
        "quiet"     -> c { logger = nullLogger }
        _           -> error $ "unknown log-level: " ++ l
    setLimit s    = BotConfOpt $ \c ->
      case break (== ',') s of
        (burstStr, delayStr) ->
          case reads burstStr of
            [(burstLen,[])] ->
              case reads (drop 1 $ delayStr) of
                [(delay,[])] ->
                  c { limits = Just (burstLen, delay) }
                _ -> error $ "unabled to parse delay: " ++ delayStr
            _ -> error $ "unabled to parse burst length: " ++ burstStr
    setNone _ = BotConfOpt $ \c -> c

getBotConf :: Maybe (Chan Message -> IO ()) -> IO BotConf
getBotConf mLogger =
  do args <- getArgs
     case getOpt Permute botOpts args of
       (f, _, [])   ->
         do let conf = (foldr ($) nullBotConf (map unBotConfOpt f)) { channelLogger = mLogger }
            checkConf conf
            putStrLn (showBotConf conf)
            return conf
       (_, _, _) ->
         do progName <- getProgName
            putStr (helpMessage progName)
            exitFailure

getFeedConf :: IO (String, String, Int)
getFeedConf = do
  args <- getArgs
  let url = fromJust $ getParam "--feed-url" args
      channel = fromJust $ getParam "--channel" args
      span = read . fromJust $ getParam "--feed-span" args
  return (url, channel, span)

getParam :: String -> [String] -> Maybe String
getParam _ [] = Nothing
getParam _ [x] = Nothing
getParam key xs = Just . head $ drop 1  $ dropWhile (/= key) xs


exitHelp msg =
  do progName <- getProgName
     putStrLn msg
     putStr (helpMessage progName)
     exitFailure

checkConf :: BotConf -> IO ()
checkConf BotConf{..}
  | null host            = exitHelp "must specify --irc-server"
  | null nick            = exitHelp "must specify --nick"
  | null (username user) = exitHelp "must specify --username"
  | null (hostname user) = exitHelp "must specify --hostname"
  | null (realname user) = exitHelp "must specify --realname"
  | otherwise            = return ()

helpMessage progName = usageInfo header botOpts
  where
    header = "Usage: "++progName++" [OPTION...]\n" ++ "e.g.\n" ++
             progName ++ " --irc-server irc.freenode.net --nick stepbot --username stepbot --hostname happstack.com --realname \"happstack bot\" --channel \"#stepbot\""

main :: IO ()
main =
  do botConf <- getBotConf Nothing
     (url, channel, span) <- getFeedConf
     ircParts <- initParts (channels botConf)
     let server = host botConf
         nic = nick botConf
         file = concat [server, channel, nic, slashEscape url, show span]
         feedParts = [(feedPart file url channel, span * 1000 * 1000)]
     (tids, _) <- timerBot botConf ircParts feedParts
     (logger botConf) Important  "Press enter to quit."
     getLine
     mapM_ killThread tids


initParts :: (BotMonad m) =>
             Set String  -- ^ set of channels to join
             -> IO [m ()]
initParts chans =
  do (_, channelsPart) <- initChannelsPart chans
     return [ pingPart
            , nickUserPart
            , channelsPart
            , dicePart
            ]

showBotConf :: BotConf -> String
showBotConf conf = "user=" ++ show (user conf)
                   ++ " nick=" ++ show (nick conf)
                   ++ " prefix=" ++ show (commandPrefix conf)
                   ++ " channnels=" ++ show (channels conf)
                   ++ " limits=" ++ show (limits conf)
                   ++ " port=" ++ showPort conf
                   ++ " host=" ++ show (host conf)
                   
showPort :: BotConf -> String
showPort conf =
  case port conf of
    (PortNumber num) -> show num
    (Service serv) -> serv
 
    
logPart :: BotMonad m => String -> m ()
logPart channel =
  do msg <- liftM show askMessage
     time <- liftIO $ getLocalCurrentTimeString "%F %X"
     let cmd = "echo '" ++ time ++ " "  ++ msg ++ "' >> log"
     logM Debug cmd
     liftIO $ system cmd
     sendMessage $ privmsg channel cmd
     return ()

logPart' :: BotMonad m => m ()
logPart' =
  do msg <- liftM show askMessage
     time <- liftIO $ getLocalCurrentTimeString "%F %X"
     let cmd = "echo log2 '" ++ time ++ " "  ++ msg ++ "' >> log"
     logM Debug cmd
     return ()

getLocalCurrentTimeString :: String -> IO String
getLocalCurrentTimeString format =
  do utc <- getCurrentTime
     zone <- getTimeZone utc
     let local = utcToLocalTime zone utc
     let formatted = formatTime defaultTimeLocale format local
     return formatted

feedPart :: BotMonad m => String -> String -> String -> m ()
feedPart file url channel =
  do (_, _, body) <- liftIO $ httpGet url
     last <- liftIO $ lastUpdated file
     let news = getUpdatedItems last body
     mapM (\item ->
            do let link = getUrlFromItem item
                   title = case getItemTitle item of
                     Just t -> t
                     _ -> ""                       
                   author = case getItemAuthor item of
                     Just a -> a
                     _ -> ""
                   msg = title ++ "(" ++ author ++ ") " ++ link
               sendMessage $ privmsg channel msg
               return ()
              ) $ reverse $ take 10 news
     liftIO $ saveUpdated file news
     return ()

slashEscape :: String -> String
slashEscape str = filter (/= '/') str
