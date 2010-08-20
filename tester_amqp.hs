{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Main(children, main, forkChild,val,killer) where
-- import Forker
import Network.AMQP
-- import Control.Concurrent(threadDelay)
import Control.Monad(forM_)
import qualified Data.ByteString.Lazy.Char8 as L
--import Control.Concurrent
--import Control.Exception
-- import System.IO.Unsafe

import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

waitForChildren :: IO ()
waitForChildren = do
      cs <- takeMVar children
      case cs of
        []   -> return ()
        m:ms -> do
           putMVar children ms
           takeMVar m
           waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
        mvar <- newEmptyMVar
        childs <- takeMVar children
        putMVar children (mvar:childs)
        forkIO (io `finally` putMVar mvar ())
          
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

threads = 1000
iterations::Int
iterations = 5

killer :: IO()
killer = return ()

main :: IO()
main = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
        
    -- declare a queue, exchange and binding
    declareQueue chan newQueue {queueName = "jobs"}
    declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
    bindQueue chan "jobs" "myExchange" "myKey"
    mapM_ (\x -> forkChild $ putter chan x) [1..threads]
    waitForChildren  

    putStrLn "Waiting:::"
    threadDelay 1000000   
    putStrLn "Done Waiting:::"

    closeConnection conn

putter chan id = forM_ [1..iterations] (\x ->mq_put chan (val id x))
--main = do q <- declareQueue "queue"
--       mapM_ (forkChild . tester q) [1..threads] >> forkChild killer >> waitForChildren
mq_put chan n = publishMsg chan "myExchange" "myKey" newMsg {msgBody = n, msgDeliveryMode = Just Persistent }          


val i myID = L.pack (show i ++ ":" ++ show myID)
    -- subscribe to the queue.
                   -- consumeMsgs chan "jobs" Ack myCallback

    -- publish a message to our new exchange
    -- publishMsg chan "myExchange" "myKey" 
    --     newMsg {msgBody = "hello world", 
    --             msgDeliveryMode = Just Persistent}

    -- getLine -- wait for keypress
    -- closeConnection conn
    -- putStrLn "connection closed"


-- mq_get chan _n = consumeMsgpublishMsg chan "myExchange" "myKey" newMsg {msgBody = show n, msgDeliveryMode = Just Persistent }

--    server <- openConnection "127.0.0.1" 22122
--    mapM (\i -> do result <- get server (val i myID)
--                   if result /= (Nothing :: Maybe String)
--                     then putStrLn $ show myID ++ " failed at " ++ show i 
--                     else return ()
--          ) [1..iterations]     
--    mapM_ (\i -> set server (val i myID) (val i myID)) [1..iterations]
--    mapM_ (\i -> check server (val i myID) (Just (val i myID))) [1..iterations]
--    putStrLn (show myID ++ " is done")

-- check :: Server -> String -> Maybe String -> IO ()
-- check server key desired = do result <- get server key
--                               if result /= desired
--                                 then putStrLn $ "Failed! " ++ show result ++ ", " ++ show desired
--                                 else return ()
