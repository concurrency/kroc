import Control.Monad
import Control.Concurrent
import System.Environment
import System.IO

elements = 256

nextToken 0 = 0
nextToken t = t + 1

element this next = loop
  where loop = do
          token <- takeMVar this
	  putMVar next $! (nextToken token)
          when (token > 0) loop

rootElement 0 token _ _ = do
  return token
rootElement cycles token this next = do
  putMVar next $! (token + 1)
  token' <- takeMVar this
  rootElement (cycles - 1) token' this next

root cycles this next = do
  putMVar next $! 1
  token1 <- takeMVar this
  
  hPutStrLn stdout "start"
  hFlush stdout
  token2 <- rootElement cycles token1 this next
  hPutStrLn stdout "end"
  hFlush stdout
  print token2

  putMVar next $! 0
  token3 <- takeMVar this
  
  return (token1 + token2 + token3)

newElement this _ = do
  next <- newEmptyMVar
  forkIO (element this next)
  return next

main = do
  args <- getArgs
  next <- newEmptyMVar
  this <- foldM newElement next [2..elements]
  root (read (head args)) this next

