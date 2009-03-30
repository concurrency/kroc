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

passTokens 0 _ _ = do
  return Nothing
passTokens tokens this next = do
  token <- takeMVar this
  putMVar next $! (token + 1)
  passTokens (tokens - 1) this next

recvTokens 0 _ sum = do 
  return sum
recvTokens tokens this sum = do
  token <- takeMVar this
  recvTokens (tokens - 1) this (sum + token)

sendTokens 0 _ = do 
  return Nothing
sendTokens tokens next = do
  putMVar next $! tokens
  sendTokens (tokens - 1) next

rootElement 0 tokens this _ = recvTokens tokens this 0
rootElement cycles tokens this next = do
  passTokens tokens this next
  rootElement (cycles - 1) tokens this next

root :: Int -> Int -> MVar Int -> MVar Int -> IO Int
root cycles tokens this next = do
  putMVar next $! 1
  token1 <- takeMVar this
  
  hPutStrLn stdout "start"
  hFlush stdout
  sendTokens tokens next
  sum <- rootElement cycles tokens this next
  hPutStrLn stdout "end"
  hFlush stdout
  print sum

  putMVar next $! 0
  token3 <- takeMVar this
  
  return (token1 + sum + token3)

newElement this _ = do
  next <- newEmptyMVar
  forkIO (element this next)
  return next

main = do
  args <- getArgs
  next <- newEmptyMVar
  this <- foldM newElement next [2..elements]
  root (read (head args)) (read (head (tail args))) this next

