module Forker where
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
