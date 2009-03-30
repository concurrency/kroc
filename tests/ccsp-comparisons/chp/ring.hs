import Control.Monad
import Control.Concurrent.CHP
import qualified Control.Concurrent.CHP.Common as CHP
import Control.Concurrent.CHP.Channels
import Control.Concurrent.CHP.Console
import Control.Concurrent.CHP.Monad
import Control.Concurrent.CHP.Parallel
import qualified Control.Concurrent.CHP.Utils
import System.Environment
import System.IO

elements = 256

nextToken 0 = 0
nextToken t = t + 1

element :: Chanin Int -> Chanout Int -> CHP ()
element this next = loop
  where loop = do
          token <- readChannel this
	  writeChannel next $! (nextToken token)
          when (token > 0) loop

rootElement 0 token _ _ = do
  return token
rootElement cycles token this next = do
  writeChannel next (token + 1)
  token' <- readChannel this
  rootElement (cycles - 1) token' this next

printString c s = do
  mapM_ (writeChannel c) s 

root :: ConsoleChans -> Int -> Chanin Int -> Chanout Int -> CHP ()
root chans cycles this next = do
  writeChannel next 1
  token1 <- readChannel this
  
  printString (cStderr chans) "start\n"
  token2 <- rootElement cycles token1 this next
  printString (cStderr chans) "end\n"
  printString (cStderr chans) (show token2)
  printString (cStderr chans) "\n"

  writeChannel next 0
  token3 <- readChannel this

  --printString (cStderr chans) (show (token1 + token2 + token3))
  --printString (cStderr chans) "\n"
  return ()

consoleMain :: Int -> ConsoleChans -> CHP ()
consoleMain count chans = do
  Control.Concurrent.CHP.Utils.cycle (c:cs)
  return ()
    where c  = root chans count
          cs = take (elements - 1) (repeat element)

main = do
  args <- getArgs
  runCHP_ (consoleProcess (consoleMain (read (head args))))

