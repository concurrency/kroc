import Control.Monad
import Control.Concurrent
import Data.Bits
import System.Environment
import System.IO

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- Constants

nCycles = 1024 :: Int

worldSize = 5 :: Int

locAgents = 12
locSize = 4096 :: Int
locMax = ((locSize `div` 2) - 1) :: Int
locMin = (- (locSize `div` 2)) :: Int

-- Vectors

data Vector = Vector Int Int
  deriving (Eq, Ord, Show)

instance Num Vector where
  (Vector x1 y1) + (Vector x2 y2) = 
    (Vector (x1 + x2) (y1 + y2))
  (Vector x1 y1) - (Vector x2 y2) =
    (Vector (x1 - x2) (y1 - y2))
  (Vector x1 y1) * (Vector x2 y2) =
    (Vector (x1 * x2) (y1 * y2))
  negate (Vector x y) =
    (Vector (negate x) (negate y))
  abs (Vector x y) =
    (Vector (abs x) (abs y))
  signum (Vector x y) = 
    (Vector (signum x) (signum y))
  fromInteger i =
    (Vector (fromInteger i) (fromInteger i))

-- Barriers

data BarrierReq = Enroll Int | Resign Int | Sync | ShutdownBarrier
data Barrier = Barrier (MVar BarrierReq) (MVar Int)

newBarrierCount enrolled count
  | count <= 0 = enrolled
  | otherwise  = count

signalBar _ 0 = return ()
signalBar resp count = do
  putMVar resp count
  signalBar resp (count - 1)

barrierControl req resp enrolled count = do
  message <- takeMVar req
  handleMessage message
    where
      handleMessage (Enroll n) =
        barrierControl req resp (enrolled + n) (count + n)
      handleMessage (Resign n) = do
        when ((count - n) <= 0) (signalBar resp enrolled')
	barrierControl req resp enrolled' (newBarrierCount enrolled' (count - n))
	  where enrolled' = enrolled - n
      handleMessage (Sync) = do
        when ((count - 1) <= 0) $ do
	  signalBar resp enrolled
	barrierControl req resp enrolled (newBarrierCount enrolled (count - 1))
      handleMessage (ShutdownBarrier) = 
        return ()

newBarrier :: Int -> IO (Barrier)
newBarrier enrolled = do
  req <- newEmptyMVar
  resp <- newEmptyMVar
  forkIO (barrierControl req resp enrolled enrolled)
  return (Barrier req resp)

enrollOnBarrier :: Barrier -> Int -> IO ()
enrollOnBarrier (Barrier req _) count = do
  putMVar req (Enroll count)

resignFromBarrier :: Barrier -> Int -> IO ()
resignFromBarrier (Barrier req _) count = do
  putMVar req (Resign count)

syncBarrier :: Barrier -> IO ()
syncBarrier (Barrier req resp) = do
  putMVar req (Sync)
  x <- takeMVar resp
  return ()

shutdownBarrier :: Barrier -> IO ()
shutdownBarrier (Barrier req _) = do
  putMVar req (ShutdownBarrier)

-- Simulation

data AgentInfo = 
  AgentInfo Int		-- id
            Int		-- loc
	    Vector      -- position
  deriving (Eq, Show)

data ViewReq = GetView Int (MVar ViewResp)
             | ShutdownView
data ViewResp = View [AgentInfo]

data LocationReq = Enter AgentInfo (MVar LocationResp)
                 | Move Int Vector (MVar LocationResp)
                 | GetInfo (MVar LocationResp)
                 | ShutdownLocation
data LocationResp = StayHere AgentInfo (MVar ViewReq)
		  | GoThere (MVar LocationReq) AgentInfo
		  | Info [AgentInfo]

data Neighbourhood = Neighbours [MVar LocationReq] 

searchOffsets = [(Vector (-1) (-1)),
          (Vector 0 (-1)),
	  (Vector 1 (-1)),
	  (Vector (-1) 0),
	  (Vector 1 0),
	  (Vector (-1) 1),
	  (Vector 0 1),
	  (Vector 1 1)]
directionOffsets = searchOffsets ++ [Vector 0 0]
neighbourOffsets =
  map (\x -> (x * (Vector locSize locSize))) directionOffsets

getNeighbour :: Int -> Neighbourhood -> (MVar LocationReq)
getNeighbour n (Neighbours ls) = ls !! n 

calculateDirection (Vector x y) = 
  let dv = Vector (dirValue x) (dirValue y)
  in matchIndex 0 dv directionOffsets
  where 
    matchIndex i v (x:xs)
      | v == x     = i
      | otherwise  = matchIndex (i + 1) v xs
    dirValue x
      | x < locMin = -1
      | x > locMax = 1
      | otherwise  = 0

adjustPosition :: Int -> Vector -> Vector
adjustPosition d v = v - (neighbourOffsets !! d)

inBounds (Vector x y)  
  | (x < locMin) || (x > locMax) = False
  | (y < locMin) || (y > locMax) = False
  | otherwise                    = True

mergeAgents _ [] vs = vs
mergeAgents f (a:as) vs = mergeAgents f as (ua:vs)
  where ua = f a

buildView respChan search = do
  getView respChan neighbourOffsets search []
  where getView _ [] [] view = return view
        getView r (p:ps) (l:ls) view = do
          putMVar l (GetInfo r)
          (Info agents) <- takeMVar r
          getView r ps ls (mergeAgents (updateAgent p) agents view)
            where updateAgent v (AgentInfo id loc p') = AgentInfo id loc (p' + v)
	          

view :: MVar ViewReq -> MVar LocationResp -> [MVar LocationReq] -> (Int, [AgentInfo]) -> IO ()
view req locResp search (cycle, agents) = do
  message <- takeMVar req
  (running, cache) <- handleMessage message
  when running
    (view req locResp search cache)
    where
      handleMessage (GetView reqCycle resp) = 
        if reqCycle == cycle
	then do
          putMVar resp (View agents)
	  return (True, (cycle, agents))
	else do
	  agents' <- buildView locResp search
          putMVar resp (View agents')
	  return (True, (reqCycle, agents'))
      handleMessage (ShutdownView) =
	return (False, (cycle, agents))

location :: Int -> (MVar LocationReq) -> Neighbourhood -> IO ()
location loc req neighbours = do
  viewReq <- newEmptyMVar
  locResp <- newEmptyMVar
  forkIO (view viewReq locResp (search ++ [req]) (0, []))
  innerLoop viewReq IntMap.empty
    where 
      (Neighbours search) = neighbours
      innerLoop viewReq agents = do
        message <- takeMVar req
        (running, agents') <- handleMessage message
        when running
          (innerLoop viewReq agents')
	where
	  handleMessage (Enter (AgentInfo id _ p) resp) = 
	    if inBounds p
	      then
	        let info    = AgentInfo id loc p
		    agents' = IntMap.insert id info agents
		in do
	          putMVar resp (StayHere info viewReq)
	          return (True, agents')
	      else
	        let direction = calculateDirection p
		    neighbour = getNeighbour direction neighbours
	            p'        = adjustPosition direction p
		    info      = AgentInfo id loc p'
		in do
	          putMVar resp (GoThere neighbour info)
	          return (True, agents)
	  handleMessage (Move id v resp) = 
	    if inBounds p'
	      then
	        let info    = AgentInfo id loc p'
		    agents' = IntMap.adjust (\x -> info) id agents
		in do
	          putMVar resp (StayHere info viewReq)
		  return (True, agents')
	      else
	        let direction = calculateDirection p'
		    neighbour = getNeighbour direction neighbours
	            p''       = adjustPosition direction p'
		    info      = AgentInfo id loc p''
		    agents'   = IntMap.delete id agents
	        in do
		  putMVar resp (GoThere neighbour info)
		  return (True, agents')
	    where (AgentInfo _ _ p) = IntMap.findWithDefault def id agents
		  def               = AgentInfo 0 0 (Vector 0 0)
	          p'                = p + v
	  handleMessage (GetInfo resp) = do
	    putMVar resp (Info agentList)
	    return (True, agents)
	      where (_, agentList) = unzip (IntMap.toList agents)
	  handleMessage (ShutdownLocation) = do
            putMVar viewReq (ShutdownView)
	    return (False, agents)

aSqrt :: Int -> Int -> Int
aSqrt x r = innerSqrt 0 x r
  where
    innerSqrt step x r
      | step >= 16 || r' == r = r
      | otherwise             = innerSqrt (step + 1) x r'
      where r' = shiftR (r + (x `quot` r)) 1

calculateR dx2 dy2 r2 dx dy
    | dx2 > dy2     = aSqrt r2 (abs dx)
    | dy2 > 0       = aSqrt r2 (abs dy)
    | otherwise     = 1
forceVector cycle _ _ 0 _ _ m = Vector fx fy
  where xJitter = (cycle .&. 1) * (-1)
        yJitter = ((shiftR cycle 1) .&. 1) * (-1)
        fx      = m * xJitter
        fy      = m * yJitter
forceVector _ dx2 dy2 r2 dx dy m = Vector fx fy
  where r  = calculateR dx2 dy2 r2 dx dy
        fx = (m * dx) `quot` r
        fy = (m * dy) `quot` r

calculateForce _ _ _ _ _ [] f = f
calculateForce cycle id px py position ((AgentInfo agentID _ agentPosition):as) f 
  | id == agentID = calculateForce cycle id px py position as f
  | otherwise     = calculateForce cycle id px py position as (f + f')
    where
      (Vector dx dy) = position - agentPosition
      dx'            = (dx * (px + 1)) `quot` 128
      dy'            = (dy * (py + 1)) `quot` 128
      dx2            = dx' * dx'
      dy2            = dy' * dy'
      r2             = dx2 + dy2
      magnitude      = min locSize ((3 * (locSize * locSize)) `quot` (r2 + 1))
      f'             = forceVector cycle dx2 dy2 r2 dx' dy' magnitude         

calculateMove cycle info persona vReq vResp = do
  putMVar vReq (GetView cycle vResp)
  message <- takeMVar vResp
  handleMessage message
    where 
      handleMessage (View agents) =
	return (persona', calculateForce cycle id px py p agents (Vector 0 0))
          where (AgentInfo id _ p) = info
                persona'           = persona + (length agents)
                px                 = persona' .&. 255
                py                 = (shiftR persona' 8) .&. 255

updatePersona (AgentInfo _ loc (Vector x y)) persona = 
  (((((persona .&. 65535) * x) .&. 65535) * y) .&. 65535) * loc

enterAgent info locReq locResp = do
  putMVar locReq (Enter info locResp)
  message <- takeMVar (locResp)
  handleMessage message
    where
      handleMessage (StayHere info' vReq) = 
        return (info', locReq, vReq)
      handleMessage (GoThere locReq' info') = 
        enterAgent info' locReq' locResp

moveAgent (AgentInfo id _ _) move locReq locResp = do
  putMVar locReq (Move id move locResp)
  message <- takeMVar (locResp)
  handleMessage message
    where
      handleMessage (StayHere info' vReq) = 
        return (info', locReq, vReq)
      handleMessage (GoThere locReq' info') =
        enterAgent info' locReq' locResp

agentStart cycle (AgentInfo id _ _) = do
  putStr ((show cycle) ++ " " ++ (show id) ++ " start\n")
agentAt cycle (AgentInfo id loc (Vector x y)) = do
  putStr ((show cycle) ++ " " ++ (show id) ++ " at " ++ (show loc) ++ ":" ++ (show x) ++ "," ++ (show y) ++ "\n")
agentEnd cycle (AgentInfo id _ _) persona = do
  putStr ((show cycle) ++ " " ++ (show id) ++ " end " ++ (show persona) ++ "\n")

agentLoop b cycle info persona locReq locResp vReq vResp = do
  when (cycle == nCycles) (do 
    agentAt cycle info
    agentEnd cycle info persona
    syncBarrier b)
  when (cycle < nCycles) (do
    --agentAt cycle info
    (persona', move) <- calculateMove (cycle + 1) info (updatePersona info persona) vReq vResp
    syncBarrier b
    (info', locReq', vReq') <- moveAgent info move locReq locResp
    syncBarrier b
    agentLoop b (cycle + 1) info' persona' locReq' locResp vReq' vResp)

agent b cycle info locReq = do
  locResp <- newEmptyMVar
  vResp <- newEmptyMVar
  agentStart cycle info
  syncBarrier b
  (info', locReq', vReq) <- enterAgent info locReq locResp
  syncBarrier b
  agentLoop b cycle info' persona locReq' locResp vReq vResp
    where (AgentInfo id _ _) = info
          persona            = id * 37

newWorld 0 ls = return ls
newWorld c ls = do
  l <- newEmptyMVar
  newWorld (c - 1) (l:ls)

wrap :: Int -> Int -> Int
wrap n l
  | n < 0     = wrap (n + l) l
  | n < l     = n
  | otherwise = n `rem` l

buildNeighbours :: Int -> [MVar LocationReq] -> Int -> [Vector] -> [MVar LocationReq] -> Neighbourhood
buildNeighbours _ _ _ [] ns = Neighbours (reverse ns)
buildNeighbours size world loc (o:os) ns = 
  buildNeighbours size world loc os (n:ns)
    where
      n              = world !! ((ny * size) + nx)
      nx             = wrap (x + ox) size
      ny             = wrap (y + oy) size
      x              = loc `rem` size
      y              = loc `div` size
      (Vector ox oy) = o

addAgents la _ _ id 0 = return id
addAgents la bar locReq id n = do
  forkIO (agent bar 0 info locReq)
  addAgents la bar locReq (id + 1) (n - 1)
    where 
      info = AgentInfo id (-1) (Vector o o)
      o    = locMin + ((locSize `div` (la + 4)) * (p + 2))
      p    = la - n

setupWorld _ _ _ _ _ _ [] = return ()
setupWorld ws la world bar loc baseID (l:ls) = do
  forkIO (location loc l neighbourhood)
  baseID' <- addAgents la bar l baseID la
  setupWorld ws la world bar (loc + 1) baseID' ls
    where neighbourhood = buildNeighbours ws world loc searchOffsets []

shutdownWorld [] = return ()
shutdownWorld (l:ls) = do
  putMVar l (ShutdownLocation)
  shutdownWorld ls

ticker bar 0 = do
  syncBarrier bar
  return ()
ticker bar cycle = do
  --putStr ((show cycle) ++ " A\n")
  syncBarrier bar
  --putStr ((show cycle) ++ " B\n")
  syncBarrier bar
  ticker bar (cycle - 1)

agents ws la = do
  bar <- newBarrier ((la * wa) + 1)
  world <- newWorld wa []
  setupWorld ws la world bar 0 1 world
  ticker bar (nCycles + 1)
  shutdownBarrier bar
  shutdownWorld world
    where wa = ws * ws

runAgents 0 _ = 
  agents worldSize locAgents
runAgents 1 args = 
  agents (read (args !! 0)) locAgents
runAgents _ args = 
  agents (read (args !! 0)) (read (args !! 1))

main = do
  args <- getArgs
  runAgents (length args) args
