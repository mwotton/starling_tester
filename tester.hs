import Network.Memcache.Protocol
import Network.Memcache
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

threads = 500
iterations::Int
iterations = 3
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

killer = return ()

main = mapM_ (forkChild . tester) [1..threads] >> forkChild killer >> waitForChildren
          
val i myID = (show myID ++ ":" ++ show i)
tester myID = do
   server <- connect "127.0.0.1" 22122
   mapM (\i -> do result <- get server (val i myID)
                  if result /= (Nothing :: Maybe String)
                    then putStrLn $ show myID ++ " failed at " ++ show i 
                    else return ()
         ) [1..iterations]     
   mapM_ (\i -> set server (val i myID) (val i myID)) [1..iterations]
   mapM_ (\i -> check server (val i myID) (Just (val i myID))) [1..iterations]
   putStrLn (show myID ++ " is done")

check :: Server -> String -> Maybe String -> IO ()
check server key desired = do result <- get server key
                              if result /= desired
                                then putStrLn $ "Failed! " ++ show result ++ ", " ++ show desired
                                else return ()
