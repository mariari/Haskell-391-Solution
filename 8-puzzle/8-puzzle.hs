{-# LANGUAGE KindSignatures #-}
import           Control.Lens as L hiding ((|>), (<|), index) -- used for easier getting and setting for vectors
import           Data.List           as L
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence       as S hiding (null)-- queue for BFS
import           Data.Traversable
import           Data.Vector         as V hiding (null,foldr,foldl)
import           Prelude             as P hiding (Left,Right)
import           System.Random       as R
import Data.PriorityQueue.FingerTree as Q hiding (null) -- Priority queue for A*
import qualified Data.Char           as C
import qualified Data.Foldable       as F
import qualified Data.Map.Strict     as M
import qualified Data.Set            as H  -- O(log(n)) replace with impure hash-table if slow
import qualified Data.String         as ST

-- Data types----------------------------------------------------------------------------------------------------

data Moves = Up | Down | Left | Right
  deriving (Show,Eq)

type Puzz a = Vector (Vector a)

data Point a = P a a
  deriving (Show,Eq, Ord)

data Node a = Node {puzzle  :: Puzz a
                   ,moves   :: [Moves]
                   ,zeroLoc :: Point Int
                   ,steps   :: Int
                   ,value   :: Int
                   } deriving (Show,Eq)

defaultNode :: (Eq a, Num a) => Puzz a -> Node a
defaultNode puzzle = Node puzzle [] (zeroPuzz puzzle) 0 0

class Stack (s :: * -> *) where
  pop  :: s a -> s a
  peek :: s a -> a
-- Testing Sets--------------------------------------------------------------------------------------------------

-- let 0 be the empty square, we do this instead of defining a data
ans :: Puzz Int
ans = V.fromList <$> V.fromList [[0,1,2]
                                ,[3,4,5]
                                ,[6,7,8]]

off :: Puzz Int
off = V.fromList <$> V.fromList [[2,6,4]
                                ,[8,5,3]
                                ,[1,0,7]]

anotherTest = randomMove 39123 100 off

randomTest = (\x -> randomMove x 100 off) <$> randomIO
-- GENERAL FUNCTIOINS -------------------------------------------------------------------------------------------
instance Stack Seq where
  pop  = S.drop 1
  peek = (`index` 0)

-- alternate definition
--peek = element . S.viewl
--  where element (ele S.:< _) = ele

instance Ord k => Stack (PQueue k) where
  pop  = ppop
  peek = ppeek

-- for our use case, we can ignore the maybe type, as it does not matter as we bounds check in searchGen!
ppeek :: Ord k => PQueue k t -> t
ppeek = value . Q.minView
  where
    value (Just (a,_)) = a
    value Nothing      = error "didn't bounds check"

ppop :: Ord k => PQueue k t -> PQueue k t
ppop = value . Q.minView
  where
    value (Just (_,a)) = a
    value Nothing      = error "didn't bounds check"

computeNothing :: Num a => t -> a
computeNothing _ = -1

-- =<< = (f (g x) x)
queuefold :: Ord k => (Puzz a -> k) -> PQueue k (Node a) -> Node a -> PQueue k (Node a)
queuefold heuristic = flip (Q.add =<< heuristic . puzzle)


queuefoldPath :: (Puzz a -> Int) -> PQueue Int (Node a) -> Node a -> PQueue Int (Node a)
queuefoldPath heuristic = flip (Q.add =<< ((+) . steps) <*> heuristic . puzzle)

sortTupMax :: Node Int -> Node Int -> Ordering
sortTupMax node1 node2 = compare (value node2) (value node1)

-- runs in O(nlog(n))
beamPrune :: Int -> [Node Int] -> [Node Int] -> [Node Int]
beamPrune max seq xs = L.drop (P.length newXs - max) $ L.sortBy sortTupMax newXs
  where
    newXs = seq <> xs

-- beamPrune for the modiifed version of beamSort
beamPruneMod :: Seq (Node Int) -> [Node Int] -> Seq (Node Int)
beamPruneMod seq list  = S.fromList (listSeq `intersect` listSeqSorted)
  where
    listSeq       = F.toList (foldl (|>) seq list)
    listSeqSorted = L.drop (P.length listSeq - listMax) $ L.sortBy sortTupMax listSeq
    listMax       = 10 -- 10 is a decent size for the list to get to before we prune

-- SEARCHES -----------------------------------------------------------------------------------------------------

searchGen
  :: (Foldable s, Stack s, Num a, Ord a) =>
     (Puzz a -> Int)                           -- The value of each node, useless unless for beamSearch
     -> s (Node a)                             -- the empty set for whatever the data type we are sending is!
     -> (s (Node a) -> [Node a] -> s (Node a)) -- the function that will combine our two new structures!
     -> Puzz a                                 -- The starting location
     -> Puzz a                                 -- The goal
     -> Int                                    -- The max number of moves, -1 is unbounded
     -> Maybe [Moves]                          -- the Answer if it even exists
searchGen valVec emptyQueue fold start goal lim = P.reverse <$> recurse (defaultNode start) emptyQueue H.empty
  where
    recurse node@(Node puzz path _ _ _) queue seen
      | puzz == goal      = Just path
      | null updatedQueue = Nothing
      | otherwise         = recurse (peek updatedQueue) (pop updatedQueue) newSeen
      where
        validMoves   = legalMoves node seen valVec lim
        updatedQueue = fold queue validMoves  -- if this is breadthFirst then we append to the back of the queue
        newSeen      = H.insert puzz seen     -- if this is depthFirst   then we append to the start of the queue

depthOrBredth = searchGen computeNothing S.empty

breadthFirst :: Puzz Int -> Puzz Int -> Int -> Maybe [Moves]
breadthFirst = depthOrBredth (foldl (|>))


depthFirst :: Puzz Int -> Puzz Int -> Int -> Maybe [Moves]
depthFirst = depthOrBredth (foldr (<|))


aStarGen f start = searchGen computeNothing  Q.empty f start ans

-- the specific case A* that only goes to answer
aStarSearch :: Puzz Int -> Int -> Maybe [Moves]
aStarSearch = aStarGen heuristic2

-- version of A* with the path calculated... IE proper A*, but you pay the price of it being 30x slower
aStarSearchPath :: Puzz Int -> Int -> Maybe [Moves]
aStarSearchPath = aStarGen heuristic2p

-- this is a modified version of Beamsearch that expands 1 node at a
-- time instead of all the children nodes at once
beamSearchMod :: Puzz Int -> Int -> Maybe [Moves]
beamSearchMod start = searchGen heuristic2Ans S.empty beamPruneMod start ans

-- This is the traditional version of beamSearch
-- it expands all nodes on the queue
beamSearch :: Puzz Int -> Int -> Int -> Maybe [Moves]
beamSearch start numBeams lim
  | start == ans = Just []
  | otherwise    = P.reverse <$> loop startingMove H.empty
  where
    startingMove = legalMoves (defaultNode start) H.empty heuristic2Ans lim

    loop queue seen
      | null updatedQueue  = Nothing
      | not (null beamAns) = Just . moves . L.head $ beamAns
      | otherwise          = loop updatedQueue newSeen
      where
        updatedQueue = foldr addInNodes [] queue                      -- adds all inner nodes into a new set for beamSort
        newSeen      = foldr    (H.insert . puzzle) seen updatedQueue -- adds all the new nodes into the seen list
        beamAns      = P.filter ((== ans) . puzzle) updatedQueue      -- try to see if the answer is in the new queue

        addInNodes node newQueue = beamPrune numBeams newQueue (legalMoves node seen heuristic2Ans lim)

-- MOVE FUNCTIONS------------------------------------------------------------------------------------------------

locMov :: (Num a, Eq a) => Puzz a -> Point Int -> Maybe a
locMov vec (P x y) = vec ^? (ix x . ix y)

upDir :: Num a => Point a -> Point a
upDir    (P x y) = P (x - 1)    y
downDir  (P x y) = P (x + 1)    y
leftDir  (P x y) = P    x    (y - 1)
rightDir (P x y) = P    x    (y + 1)

up :: (Num a, Eq a) => Puzz a -> Point Int -> Maybe a
up    vec = locMov vec . upDir
down  vec = locMov vec . downDir
left  vec = locMov vec . leftDir
right vec = locMov vec . rightDir

-- Updates the vector
-- new version using lenses
updateVec :: Puzz a -> Point Int -> a -> Puzz a
updateVec vec (P x y) val = vec & (ix x . ix y) .~ val

-- swaps the 0 node and a node around it
moveLoc :: Num a =>
           Maybe a        -- Maybe the value to replace with 0 if it's even valid!
        -> Puzz a         -- the current puzzle to change
        -> Point Int      -- the location of the new 0xy location
        -> Point Int      -- the location of the current 0xy location
        -> Maybe (Puzz a) -- updated puzzle
moveLoc Nothing    _       _     _     = Nothing
moveLoc (Just val) current xynew xyold = Just updateOld
  where
    updateNew = updateVec current   xynew 0   --update new 0 pos
    updateOld = updateVec updateNew xyold val --update old 0 pos


moveGen :: Num a => (Puzz a -> Point Int -> Maybe a) -- the function that would move the 0 piece
                -> Moves                            -- the new move
                -> (Point Int -> Point Int)         -- applies a change on the coordinates
                -> Node a                           -- the current node
                -> Maybe (Node a)
moveGen dir move newDir (Node vec path xyold numSteps _) = createNode <$> moveLoc loc vec xynew xyold
  where loc            = dir vec xyold -- The valid value if the location in question even exists
        xynew          = newDir xyold  -- the new 0 coordinates
        createNode vec = Node vec (move : path) xynew numSteps (-1) -- note the -1 is bad, replace with a Nothing later

moveDown  :: (Num a, Eq a) => Node a -> Maybe (Node a)
moveUp    :: (Num a, Eq a) => Node a -> Maybe (Node a)
moveLeft  :: (Num a, Eq a) => Node a -> Maybe (Node a)
moveRight :: (Num a, Eq a) => Node a -> Maybe (Node a)
moveUp    = moveGen up    Up    upDir
moveDown  = moveGen down  Down  downDir
moveLeft  = moveGen left  Left  leftDir
moveRight = moveGen right Right rightDir


legalMoves :: (Num a, Eq a, Ord a) => Node a -> H.Set (Puzz a) -> (Puzz a -> Int) -> Int -> [Node a]
legalMoves currNode seen f lim = (\n -> n {value = f (puzzle n)}) <$> P.filter isValid (catMaybes maybeMove)
  where
    apply f   = f (currNode {steps = 1 + steps currNode})
    maybeMove = apply <$> [moveUp,moveDown,moveLeft,moveRight]
    isValid n = (lim < 0 || steps n < lim) && not (H.member (puzzle n) seen)

-- Heuristic Functions-------------------------------------------------------------------------------------------

heuristicGenerator :: (Enum a, Num c, Num a) => ((Point a, b) -> c) -> Vector (Vector b) -> c
heuristicGenerator difference = snd . V.foldl' innersum (0,0)
  where
    -- we are essentially mapping the Point location of the vector to the vector, then grabbing the diff from
    -- the proper place with diff, then adding everything together
    innersum (loc,acc) ys = (1 + loc, P.sum (difference <$> tzip pointLoc ys) + acc)
      where
        pointLoc = P loc <$> [0..2]

heuristic1Ans :: Puzz Int -> Int
heuristic1Ans = heuristicGenerator h1

-- the heuristic for the normal 012 345 678 answer
-- this version does not return a new matrix to return the difference in size
heuristic2Ans :: Puzz Int -> Int
heuristic2Ans = heuristicGenerator h2

pointsAre :: H.Set (Point Int, Int)
pointsAre =  H.fromList [(P 0 0, 0), (P 0 1, 1), (P 0 2, 2),
                         (P 1 0, 3), (P 1 1, 4), (P 1 2, 5),
                         (P 2 0, 6), (P 2 1, 7), (P 2 2, 8)]

h1 :: (Point Int, Int) -> Int
h1 = f . flip H.member pointsAre
  where f False = 1
        f True = 0

h2 :: (Point Int, Int) -> Int
h2 (xy, index) = calcDiff point xy
  where (point, _) = H.elemAt index pointsAre

calcDiff :: Num a => Point a -> Point a -> a
calcDiff (P x y) (P xoff yoff) = abs (x - xoff) + abs (y - yoff)


-- the heuristic function that takes a goal and a starting point and stores the appropriate values in a new matrix
heuristicGen :: Puzz Int -> Puzz Int -> (Int, Puzz (Int,Int))
heuristicGen = undefined

-- prebuilt up heuristic functions to be passed to aStarGen
heuristic1 :: PQueue Int (Node Int) -> [Node Int] -> PQueue Int (Node Int)
heuristic1 = foldl (queuefold heuristic1Ans)

heuristic1p :: PQueue Int (Node Int) -> [Node Int] -> PQueue Int (Node Int)
heuristic1p = foldl (queuefoldPath heuristic1Ans)

heuristic2 :: PQueue Int (Node Int) -> [Node Int] -> PQueue Int (Node Int)
heuristic2 = foldl (queuefold heuristic2Ans)

heuristic2p :: PQueue Int (Node Int) -> [Node Int] -> PQueue Int (Node Int)
heuristic2p = foldl (queuefoldPath heuristic2Ans)

-- Setup Functions-----------------------------------------------------------------------------------------------

-- zips the vecotr with the indices
vecWithIndex :: Vector b -> [(Int, b)]
vecWithIndex = tzip [0..(V.length ans - 1)]

--  just grabs the value out of Maybe, which should always happen!
zerVal :: Num a => (a, Maybe a) -> Point a
zerVal (x,Just y)  = P x y
zerVal (_,Nothing) = error "Didn't bounds check"

-- gets the location of the zero
zeroPuzz :: (Eq a, Num a) => Puzz a -> Point Int
zeroPuzz = zerVal . P.head . P.filter (isJust . snd) . (fmap . fmap) (V.findIndex (== 0)) . vecWithIndex

-- Helper Functions----------------------------------------------------------------------------------------------

-- tzipWith is not written by me!
tzipWith :: Traversable t => (a -> b -> c) -> [a] -> t b -> Maybe (t c)
tzipWith f xs = sequenceA . snd . mapAccumL pair xs
    where pair [] y = ([], Nothing)
          pair (x:xs) y = (xs, Just (f x y))

tzip :: [a] -> Vector b -> [(a, b)]
tzip lis seq = ans zipped
  where
    zipped = tzipWith (,) lis seq
    ans Nothing  = []
    ans (Just a) = toList a

parseNum :: Char -> Int
parseNum = (`mod` 11) . C.digitToInt


-- grab a single random move
getRandMove :: (Num a, Ord a, RandomGen g) => H.Set (Puzz a) -> Puzz a -> g -> (Int, g)
getRandMove seen = randomR . (,) 0 . subtract 1 . P.length . (`getMoves` seen)

-- get all legal puzzles from a vector
getMoves :: (Num a, Ord a) => Puzz a -> H.Set (Puzz a) -> [Puzz a]
getMoves vec seen = puzzle <$> legalMoves (defaultNode vec) seen computeNothing (-1)

-- USER INTERFACE FUNCTIONS-------------------------------------------------------------------------------------

-- Pareses an input string and gives back a Puzzle
parseString :: String -> Puzz Int
parseString = fmap V.fromList . V.fromList . intList  -- turns [[Int]] into a proper Puzzle form
  where
    intList = (fmap . fmap) parseNum . ST.words        -- turns the string into a [[Int]] list

justMovement :: (Eq a, Num a) => Puzz a -> (Node a -> Maybe (Node a1)) -> Puzz a1
justMovement vec = move . maybMov
  where maybMov f       = puzzle <$> f (defaultNode vec)
        move Nothing    = error "invalid movement"
        move (Just mov) = mov

move :: Moves -> Puzz Int -> Puzz Int
move Up    vec  = justMovement vec moveUp
move Down  vec  = justMovement vec moveDown
move Left  vec  = justMovement vec moveLeft
move Right vec  = justMovement vec moveRight


printPuzz :: (Show a, Foldable t) => t a -> IO ()
printPuzz = F.mapM_ print

moveList :: Foldable t => Puzz Int -> t Moves -> Puzz Int
moveList vec movList = foldl (flip move) vec movList

moveListP :: Foldable t => Puzz Int -> t Moves -> IO ()
moveListP vec = printPuzz . moveList vec


randomMove :: Int -> Int -> Puzz Int -> Puzz Int
randomMove seed numMoves vec = fst $ iterate into (vec, startSeed) !! numMoves
  where
    startSeed       = mkStdGen seed
    into (vec, gen) = (getMoves vec H.empty !! mov, newSeed)
      where
        (mov,newSeed) = getRandMove H.empty vec gen

-- move X moves away without backtracking... note that if numMoves gets big enough... we may backtrack
-- in Other ways!!
randomMoveX :: Int -> Int -> Puzz Int -> Maybe (Puzz Int)
randomMoveX seed numMoves vec = (^._1) <$> (iterate into (Just (vec, startSeed, H.empty)) !! numMoves)
  where
    startSeed                    = mkStdGen seed
    into Nothing                 = Nothing
    into (Just (vec, gen, seen))
      | null (getMoves vec seen) = Nothing
      | otherwise                = Just (getMoves vec seen !! mov, newSeed, H.insert vec seen)
      where
        (mov,newSeed) = getRandMove seen vec gen
-- TIMING FUNCTIONS----------------------------------------------------------------------------------------------

-- genStats generates a bunch of puzzles and sees how steps each answer gives us, and returns a map of them all!
genStats :: Int -- the limit of how many steps we want for acceptable answers
         -> Int -- how many squares we want to permute the answer
         -> Int -- the starting seed for our test
         -> Int -- How many tests we want
         -> M.Map (Maybe Int) ([Maybe Int], [Maybe Int], [Maybe Int])
genStats lim away seed = fst . (iterate foldCase (M.empty, (seed,startSeed)) !!)
  where
    startSeed           = mkStdGen seed
    foldCase (map,seed) = (testCase map vec, newSeed)
      where
        newSeed = R.next . snd $ seed
        vec     = randomMove (fst newSeed) away ans

    testCase map vec = M.insert key (emptyTest lookup) map
      where
        key    = P.length <$> breadthFirst vec ans (-1)
        lookup = M.lookup key map
        h1     = P.length <$> aStarGen heuristic1p vec lim
        h2     = P.length <$> aStarGen heuristic2p vec lim
        beam   = P.length <$> beamSearch vec 40 lim
        emptyTest Nothing             = ([h1],   [h2],   [beam])
        emptyTest (Just (h1s,h2s,bs)) = (h1:h1s, h2:h2s, beam:bs)


avgNothing :: (Fractional a, Integral a1) => [Maybe a1] -> a
avgNothing = ((/) . fromIntegral . P.sum <*> fromIntegral . P.length) . catMaybes

goodStats :: M.Map k ([Maybe Int], [Maybe Int], [Maybe Int]) -> M.Map  k ((Double, Int, Int), (Double, Int, Int), (Double, Int, Int))
goodStats = fmap grabInfo
  where
    numJusts    mxs      = P.length (catMaybes mxs)
    numNothings mxs      = P.length mxs - numJusts mxs
    createInfo  mxs      = (avgNothing mxs, numNothings mxs, numJusts mxs)
    grabInfo (h1, h2, b) = (createInfo h1, createInfo h2, createInfo b)


test = goodStats (genStats (-1) 50 12313 10000)
-- RUNNING FUNCTIONS---------------------------------------------------------------------------------------------

main :: IO ()
--main = print =<< (\x -> P.length <$> breadthFirst x ans (-1)) <$> randomTest

--main  = print test
main = print $ P.length <$> beamSearch anotherTest 2 (-1)
--main = print $ P.length <$> depthFirst anotherTest ans (-1)

-- DEPRECATED CODE USED TO REMEMBER PREVIOUS DESIGN DECISION-----------------------------------------------------

-- Updates the vector
updateVec' :: Vector (Vector a) -> Point Int -> a -> Vector (Vector a)
updateVec' vec (P x y) val = vec // [(x, vec ! x // [(y, val)])]

locMov' :: Puzz Int -> Point Int -> Maybe Int
locMov' vec (P x y) = vec V.!? x >>= (V.!? y)

avgNothing' :: (Fractional a, Integral a1) => [Maybe a1] -> a
avgNothing' xs = fromIntegral (P.sum numList) / fromIntegral (P.length numList)
  where
    numList = catMaybes xs
