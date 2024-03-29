-- This file should do analysis and manipulation of the PCode as
-- represented by the PCode Module.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- TODO make this a nice list with shadowing (but I also want all the POpt constructors)
module Analysis where

import Data.List (foldl')
import Control.Lens
import qualified Data.Map as Map

import PCode
import VarNodeUtils

-- -------------------------------------------------------------------
--
-- Sequences of instructions
--
-- Contigious sequences of instructions and things we might want to do
-- with them.

data PBlock = PBlock {
  _blockId :: !Integer,
  _stream :: [PInst]
  }
{- ^ These are a contiguous stream of instructions that we think are
 probably atomic with respect to clontrol flow. they have ids to talk
 about them indirectly. An id is a reference to the block starting at
 the associated instruction, though that mapping may be arbitrary. This
 matters if you decide laters to split a block. The new block before
 the split should keep the same id, and the block after should get a
 new one. -}
makeLenses ''PBlock

terminating :: PBlock -> PInst
initial :: PBlock -> PInst
-- ^ just for readability and avoiding unwrapping
terminating (PBlock _id strm) = last strm
initial (PBlock _id strm) = head strm

splitBlock :: Integer -> PAddr -> PBlock -> Maybe (PBlock, PBlock)
-- ^ Split the Block at the given address, and return the truncated
-- original and the new successor, with the given id. Or don't, if
-- that PAddr isn't in this block.
splitBlock nid target (PBlock oid bstream) =
  let (before, after) = break ((== target) . (view location)) bstream
  in
    if (null after) || (null before)
    then
      Nothing
    else
      Just (PBlock oid before,
            PBlock nid after)

_splitBlockInList :: Ordering -> Integer -> PAddr -> [PBlock] -> Maybe [PBlock]
-- ^ Same thing, but just traverse the list for it and return the
-- replcaement list if possible. Nothing if the address doesn't match
-- anything. Assumes the list of blocks is in reversed program order.
_splitBlockInList whileOrd nid target blockList =
  let (beforeAndCurrent, after) = break ((/= whileOrd) . (compare target) . (view location) . initial) blockList
      toSplit = last beforeAndCurrent
      prior = init beforeAndCurrent
  in
    if null beforeAndCurrent
    then
      Nothing
    else
      (splitBlock nid target toSplit) >>= (\(a,b) -> Just [a,b]) >>= (\newEntryList -> Just $ concat [prior, newEntryList, after])

splitBlockInForwardList :: Integer -> PAddr -> [PBlock] -> Maybe [PBlock]
splitBlockInForwardList nid target blockList =  _splitBlockInList LT nid target blockList
splitBlockInRevList :: Integer -> PAddr -> [PBlock] -> Maybe [PBlock]
splitBlockInRevList nid target blockList =  _splitBlockInList GT nid target blockList

initialSplitPass :: [PInst] -> ([PBlock], Integer)
initialSplitPass iList =
  let (rBlocks, nextId, []) = _initialSplitPass ([], 1, []) iList
  in
    ((reverse rBlocks), nextId)

_initialSplitPass :: ([PBlock], Integer, [PInst]) -> [PInst] -> ([PBlock], Integer, [PInst])
-- ^ Splits based on where control flow instructions are, that's it
_initialSplitPass acc [] = acc
_initialSplitPass (revPriorBlocks, nextId, revInstAcc) (nextI:streamRest) =
  if (terminalP nextI)
  then
    _initialSplitPass ((PBlock nextId (reverse (nextI:revInstAcc))):revPriorBlocks,
                       nextId + 1,
                       []
                      )
                      streamRest
  else
    _initialSplitPass (revPriorBlocks, nextId, (nextI:revInstAcc)) streamRest

targetSplitPass :: Integer -> [PBlock] -> [PBlock] -> [PBlock]
targetSplitPass nextId revPriorBlocks [] = reverse revPriorBlocks
targetSplitPass nextId revPriorBlocks (nextBlock:remainingBlocks) =
    case (branchTarget (terminating nextBlock)) of
      Just tAddr -> case (compare tAddr ((view location) (initial nextBlock))) of
        LT ->
          case (splitBlockInRevList nextId tAddr revPriorBlocks) of
            Just newPrior ->
              targetSplitPass (nextId+1) (nextBlock:newPrior) remainingBlocks
            Nothing -> --already exists, (out of func jump caught in cfg pass), same as base case
              targetSplitPass nextId (nextBlock:revPriorBlocks) remainingBlocks
        GT ->
          case (splitBlockInForwardList nextId tAddr (nextBlock:remainingBlocks)) of
            Just (newNextBlock:newRemainingBlocks) ->
              targetSplitPass (nextId+1) (newNextBlock:revPriorBlocks) newRemainingBlocks
            Nothing -> --already exists, (out of func jump caught in cfg pass), same as base case
              targetSplitPass nextId (nextBlock:revPriorBlocks) remainingBlocks
        EQ -> --split already exists here, we are good, same as base case
          targetSplitPass nextId (nextBlock:revPriorBlocks) remainingBlocks
      Nothing -> targetSplitPass nextId (nextBlock:revPriorBlocks) remainingBlocks

mkPFunction :: String -> [PInst] -> PFunction
mkPFunction name stream =
  let (bList, nid) = initialSplitPass stream
      bList' = targetSplitPass nid [] bList
  in
    PFunction bList' name

-- -------------------------------------------------------------------
--
-- Sequences of blocks.
--
-- A collection of blocks that might reasonably go together. The
-- blocks of a function. These sequences must be kept in program
-- order.

data PFunction = PFunction {
  _blocks :: [PBlock], -- in program order, head is entry
  _name :: !String
  }
-- ^ A set of sequential blocks that form a function. These will
-- become the functions in the crucible graph.
makeLenses ''PFunction

-- -------------------------------------------------------------------
--
-- Nodes of a CFG
--
-- A Block endowed with some data about its successors. The result of
-- branch analysis. Only useful in combination with others.

data TargetExcuse =
  Indirect
  | UncondNeedSplit PAddr
  | CondNeedSplit PAddr Integer -- with fallthrough id for CBRANCH
  -- ^ These are reasons we might not already have the full set of
  -- sucessors to this node.
  --
  -- These are a mix of shorterm and long term excuses, but we need
  -- them to be together, since they can be discovered at the same
  -- time.

data Successor =
  Fallthrough Integer
  -- IndirectSet [Integer]      -- TODO will we ever use this?
  | UncondTarget Integer
  | CondTarget VarNode Integer Integer -- target, then fallthrough
  -- | Extern PAddr                -- A call to another function
  -- | ExternInd                   -- A call with an indirect target
  --                               -- (likely most of them)
  -- TODO these aren't termining in crucible
  | ExternReturn                -- a return to a calling func
  -- ^ These tell you about your terminating instruction and which
  -- nodes are immediately dominated. This type singals completed
  -- analysis for the sucessors of the associated node, otherwise use
  -- TargetExcuse. Note that Extern and ExternInd don't follow this
  -- logic. That is so we can use them as the links in the higher
  -- level callgraph. The other arms are edges in the function-local
  -- CFG.

-- TODO should this just return empty if it's not appropriate?
intoList :: Successor -> Maybe [Integer]
intoList (Fallthrough fallthrough) = Just [fallthrough]
intoList (UncondTarget target) = Just [target]
intoList (CondTarget _ target fallthrough) = Just [target, fallthrough]
intoList _ = Nothing


data CFGBlock = CFGBlock {
  _block :: !PBlock,
  _successor :: Either TargetExcuse Successor -- ^ IDS of blocks in
                                              -- the same graph that
                                              -- immediately follow
                                              -- this one (downlinks)
  }
  {- ^ A single node in a CFG -}
makeLenses ''CFGBlock

-- splitCFGB :: Integer -> PAddr -> CFGBlock -> Maybe (CFGBlock, CFGBlock)
-- -- ^ Lift a block split to work on nodes. The later block inherits the
-- -- dominated blocks of the prior, and the prior gets just the later as
-- -- a fallthrough.
-- splitCFGB nid target (CFGBlock oblock osucc) =
--   (splitBlock nid target oblock) >>= (\(a,b) ->
--                                         Just (
--                                          CFGBlock a (Right (Fallthrough nid)),
--                                          CFGBlock b osucc
--                                          ))

-- splitCFGBInList :: Integer -> PAddr -> [CFGBlock] -> Maybe [CFGBlock]
-- -- ^ Same thing, but just traverse the list for it and return the
-- -- replcaement list if possible. Nothing if the address doesn't match
-- -- anything
-- splitCFGBInList nid target nodeList =
--   let (beforeAndCurrent, after) = break ((/= LT) . (compare target) . (view location) . initial . (view block)) nodeList
--       toSplit = last beforeAndCurrent
--       prior = init beforeAndCurrent
--   in
--     if null beforeAndCurrent
--     then
--       Nothing
--     else
--       (splitCFGB nid target toSplit) >>= (\(a,b) -> Just [a,b]) >>= (\newEntryList -> Just $ concat [prior, newEntryList, after])

data CFG = CFG {
  _nodes :: [CFGBlock],
  _cfgNextId :: !Integer, -- the first unused id, for use when splitting nodes
  _cfgIsStable :: !Bool,  -- True if it is safe to assume that all
                          -- control flow paths have been
                          -- discovered. We can do non-reversable and
                          -- other nastier transformations and
                          -- analysis. This means that all successors
                          -- are non-excuses.
  _cfgName :: !String
  }
-- ^ A (likely) connected set of blocks seperated by control flow
-- instructions or control flow targets.
--
-- Note that our analysis at the moment does not extend to indirect
-- branches, so when using or translating this structure, either
-- handle that yourself or warn / panic at the sign of indirection.
makeLenses ''CFG

locToBidMapper :: PFunction -> Map.Map PAddr Integer
locToBidMapper (PFunction blockList _name) =
  foldl' (\ !priorMap block ->
             Map.insert (((view location) . initial) block) (block^.blockId) priorMap)
         Map.empty
         blockList

  {- | Takes a list of blocks and produces a list of pairs (block, maybe
   nextId) where the id is from the next suquesntial block, or nothing
   if there isn't one. | -}
doubleList :: [PBlock] -> [(PBlock, Maybe Integer)]
doubleList (h:n:r) = ((h, Just (n^.blockId)):(doubleList (n:r)))
doubleList (h:[]) = (h, Nothing):[]
doubleList [] = []

initialGraph :: PFunction -> Either String CFG
initialGraph func = do
  sList <- sequence $ map single (doubleList (func^.blocks))
  zipped <- return $ zip (func^.blocks) sList
  cfgbList <- return $ map (uncurry CFGBlock) zipped
  return $ CFG cfgbList (error "Todo, plumb nextId") False (func^.name)

  where mToE = (\ msg v -> case v of
                   Just a -> Right a
                   Nothing -> Left msg)
        lbMap = locToBidMapper func
        single = (\(block, mNextId) -> do
                     let finalInst = terminating block
                         mTarget = branchTarget finalInst
                       in
                       case (finalInst^.opt) of
                         RETURN _vn -> Right $ Right ExternReturn
                         BRANCH _target -> do
                           tid <- (mToE "Bad branch target" (mTarget >>= (flip Map.lookup lbMap)))
                           return $ Right $ UncondTarget tid
                         CBRANCH cond _target -> do
                           tid <- (mToE "Bad cbranch target" (mTarget >>= (flip Map.lookup lbMap)))
                           nid <- mToE "Fallthrough on final block" mNextId
                           return $ Right $ CondTarget cond tid nid
                         BRANCHIND _ -> Right $ Left Indirect
                         _ -> Left "unexpected final instruction during CFG pass")

-- TODO should probably be a maybe return type or something
cfgCommitNonIndirect :: CFG -> CFG
-- ^ Sets the stable flag in the event that this CFG has no non-extern
-- indirect jumps. Does nothing if flag is already set
cfgCommitNonIndirect input =
  if input^.cfgIsStable
  then
    input
  else
    set cfgIsStable newFlag input
  where
    isInd (PInst _ (BRANCHIND _)) = True
    isInd (PInst _ (CALLIND _)) = True
    isInd (PInst _ _) = False
    accumulateP :: Bool -> PBlock -> Bool
    accumulateP = (\(!flag) (!nextBlock) ->
                      flag && (isInd . terminating) nextBlock)
    newFlag = foldl' accumulateP True (map (view block) (view nodes input))
    -- ^ pull the PBlock out, check the terminating inst, and collect

-- -------------------------------------------------------------------
-- Augmented (concrete) CFGs
--
-- If we are confident we have a complete list of control flow
-- locations (see cfgIsStable), then we can start with analysis,
-- including touched/observed listings at the block level, backlinks
-- for control flow, and immediate dominators. These are useful, but
-- primarily needed for determining block argument detection, since we
-- can't just translate the phi expressions. We need to re-discover
-- which things need to be arugments in the cruicble arg-passing cfg
-- style.

-- TODO decide if this is still current
-- TODO we need to be careful, since varnodes don't have the disjoint
-- nature of registers. I need to check what the llvm IR's take on
-- this problem is, but we should:
--
-- A: eliminate subset dependencies. If I depend on a range and a
-- subset of said range, then I either need to drop the subset, or
-- have symbolic semantics that the subset matches t he parent range
-- like it should, since they are the same bits. If I just drop the
-- subset, I need to be careful about still propagating the symbolic
-- reasoning, and still assigning down to whatever the formal arg of
-- the subset range was. Keeping both and adding a symbolic constraint
-- might be better.
--
-- B: Do collision detection during touch/observe accumulation with
-- ranges instead of direct comparison.

data ACFGBlock = ACFGBlock {
  _aBlock :: !PBlock,
  _aPreds :: [Integer],
  _aSuccs :: !Successor, -- distinguish what kind of downlinks these are, and to where
  _aObserves :: [VarNode],
  _aTouches :: [VarNode],
  -- _aImmDomBy :: Maybe Integer -- closest stricting dominating node.
  _aDoms :: [Integer] -- list of nodes that this domminates
  }
makeLenses ''ACFGBlock

-- TODO careful during touch/observe conflict accumulation to include
-- self as a block if there is a loop that contains the current block.

-- TODO when I feed into crucible, does that have to be in program
-- order? How can I teach crucible about the weirdness of pcode
-- addresses? Do I need to?
data AugCFG = AugCFG {
  _aSize :: !Integer,
  _aBlockList :: [ACFGBlock] --of length matching size, still in program order
  -- the only possible entry point is still the head of the list
  }
makeLenses ''AugCFG

{- | Does what it says on the tin. This is used for building a AugCFG,
   particularly being able to do so in one go rather than having it be
   a several pass process. | -}
collectBackLinks :: CFG -> Map.Map Integer [Integer]
collectBackLinks cfg
  | not (view cfgIsStable cfg) = error "Collecting backlinks on unstable cfg"
  | otherwise =
    cbl' (view nodes cfg) Map.empty
  where cbl' :: [CFGBlock] -> Map.Map Integer [Integer] -> Map.Map Integer [Integer]
        cbl' [] curMap = curMap
        cbl' (n:_) !curMap =
          case n^.successor of
            Left _ -> error "Found excuse during collection pass in supposedly stable cfg"
            Right s -> case intoList s of
              Nothing -> error "unexpected term instruction during collection pass"
              Just forwardLinks ->
                let curId = n^.block^.blockId
                in
                  foldr (\dest m -> Map.insertWith (++) dest [curId] m) curMap forwardLinks

{- | Returns a program order pair (observed_locations_list,
   altered_locations_list), subject to some sane rules (see
   VarNodeUtils) like not counting reads after writes, and only
   counting the superset of a super/subset pair. | -}
aggregateEffects :: CFGBlock -> ([VarNode], [VarNode])
aggregateEffects cfgb =
  foldl' collectionStep ([], []) instStream
  where instStream = cfgb^.block^.stream
        collectionStep = \(see, touch) inst->
          let operation = (inst^.opt)
          in
            foldl' addTouched (foldl' addObserved (see, touch) (observes operation)) (touches operation)
          -- TODO This is expensive, since these all append at the
          -- back, which is expensive for lists. Again this is a
          -- startup cost, but we could refactor the add[...] stuff
          -- from VarNodeUtils to work in reverse, since prepend is
          -- constant time.


-- TODO next:
--
-- block arg reconstruction (interval analysis? or just overestimate? See Prior TODO)
-- data type for blocks with formal args?
-- pipeline functions:
-- CFG -> ACFG -> block_arg'd_cfg

{- | Useful if you don't care about program order. | -}
cfgToMap :: CFG -> Map.Map Integer CFGBlock
cfgToMap cfg =
  foldl' step Map.empty (cfg^.nodes)
  where step = \(!curMap) inBlock ->
          Map.insert (inBlock^.block^.blockId) inBlock curMap


{- | Internal data type for the dommination frontier alg | -}
data MarkedCFGB = MarkedCFGB !CFGBlock !(Maybe Integer)

{- | Recursively mark the decedents of the current block. | -}
markInMap :: Map.Map Integer MarkedCFGB -> Integer -> Map.Map Integer MarkedCFGB
markInMap !priorMap curId =
  mim' curId priorMap curId
  where current = case (priorMap Map.! curId) of
          MarkedCFGB b _ -> b
        nextList = case (current^.successor) of
          Right s -> case (intoList s) of
            Just l -> l
            Nothing -> []
          Left _ -> []
          -- TODO I think there should be some slick monad way to do this
        markSingle = \(!curMap) targetId ->
          Map.adjust (\(MarkedCFGB blk _) -> MarkedCFGB blk (Just curId)) targetId curMap
        mim' mid (!pmap) cid
          | MarkedCFGB _ (Just cmid) <- (pmap Map.! cid), cmid == mid = pmap
            -- base case, already marked
          | otherwise =
            let markedSucc = foldl' markSingle pmap nextList
            in
              foldl' (mim' mid) markedSucc nextList


{- | Collect the nodes with a matching mark. | -}
collectMarked :: Map.Map Integer MarkedCFGB -> Integer -> [Integer]
collectMarked m mid =
  (Map.elems . (Map.mapMaybe matches)) m
  where matches blk
          | MarkedCFGB cfgb (Just i) <- blk, i == mid = Just (cfgb^.block^.blockId)
          | otherwise = Nothing

{- | Which blocks are only reachable by going through the current block?
   Only for stable CFGs, since excuses break things. | -}
dominatedBy :: CFG -> Integer -> [Integer]
dominatedBy cfg curId =
  if not (cfg^.cfgIsStable)
  then
    error "domination frontier called for unstable cfg"
    -- TODO actually the internal markMap covers this with a default
    -- empty value, but it doesn't really make sense on unstable cfgs,
    -- so I'm still enforcing it.
  else
    collectMarked (markInMap markMap curId) curId
  where markMap = fmap (\blk -> MarkedCFGB blk Nothing) (cfgToMap cfg)

{- | Collect all that data and lift us to the AugCFG type. | -}
augmentCfg :: CFG -> Maybe AugCFG
augmentCfg cfg =
  if not (cfg^.cfgIsStable)
  then
    Nothing
  else
    let aBlocks = map blockTranslate (cfg^.nodes)
    in
      Just $ AugCFG (cfg^.cfgNextId) aBlocks
  where backLinks = collectBackLinks cfg
        blockTranslate = \cblock ->
          let (see, touch) = aggregateEffects cblock
          in
            ACFGBlock
              (cblock^.block)
              (backLinks Map.! (cblock^.block^.blockId))
              (case (cblock^.successor) of
                 Left _ -> error "Supposed stable cfg has excuse during augment pass"
                 Right s -> s)
              see
              touch
              (dominatedBy cfg (cblock^.block^.blockId))





-- -------------------------------------------------------------------
-- Historical alg430 implementation
--
-- Not currently in use, can be erased once the new block arg
-- detection setup is completed.

--internal data type for ImmDom discovery alg
-- data MarkedACFGB = MarkedACFGB ACFGBlock Maybe Integer

-- getMarkedBlockIndex (MarkedACFGB b _) = acfgBlockId b
-- mark (MarkedACFGB b _) id = MarkedACFGB b $ Just id
-- unmark (MarkedACFGB b _) = MarkedACFGB b Nothing
-- marked (MarkedACFGB _ m) = isJust m
-- markedWith (MarkedACFGB _ Just id) tid = id == tid
-- markedWith (MarkedACFGB _ Nothing) = False

-- -- TODO make these dfses strict? thunk buildup vs stack buildup
-- markWithFrom :: Integer -> Integer -> [MarkedACFGB] -> [MarkedACFGB]
-- markWithFrom markId srcId bl =
--     -- recursive dfs from src, marking with markId returns new
--     -- blocklist. Arg order to facilitate a slick foldr during
--     -- the recursive step. Relies on sorted order
--     let current = bl !! srcId
--         succList = intoList succs
--         listWithMark = (take srcId bl) ++ $ (mark current markId) ++ drop (srcId + 1) bl
--     in
--     if not . markedWith markId $ current
--     then
--         -- needs marking
--         foldr (markWithFrom markId) listWithMark succList
--         -- run again with one step down from src node, composing the effects of running on each successor
--     else
--         -- is already marked, this is the base case
--         bl
-- unmarkExcludeFrom :: Integer -> Integer -> [MarkedACFGB] -> [MarkedACFGB]
-- unmarkExcludeFrom excId srcId bl =
--     -- recursive dfs, unmarking found nodes back to prior
--     -- imdom. Returns new blocklist. prunes paths that go
--     -- through excId's node
--     let current = bl !! srcId
--         succList = intoList succs
--         listWithUnmark = (take srcId bl) ++ $ (unmark current) ++ drop (srcId + 1) bl
--     in
--     if not . marked $ current || getMarkedBlockIndex current == excId
--     then
--         -- prune this branch, or already reached
--         bl
--     else
--         -- reachable with w/o exclude path, unmark
--         foldr (unmarkExcludeFrom excId) listWithUnmark succList

-- alg430 :: [MarkedACFGB]
--        -> Set Integer
--        -> [Integer]
--        -> [MarkedACFGB]
-- -- ^ Nice recursive bfs. O(n^2) I think. Based on
-- -- https://dl.acm.org/doi/epdf/10.1145/361532.361566
-- alg430 graph seen queue =
--   if null queue
--   then graph -- done!
--   else
--     let current = head queue
--     in
--       if member current seen
--       then alg430 graph seen $ tail queue
--       else
--         -- new node, do actual work
--         let transRule = (markWithFrom current current) . (unmarkExcludeFrom current current)
--             -- [MarkedACFGB] -> [MarkedACFGB]
--         in
--           alg430 (transRule graph) (insert current seen) $ tail queue

-- populateImmDomBy :: AugCFG -> AugCFG
-- -- ^ Runs a version of alg 430 that marks the immediate dominator of
-- -- each node. Note that Nothing is still a valid value, since some
-- -- nodes (like entry nodes) don't have one.
-- populateImmDomBy (AugCFG size blockList entryIdList) =
--   let wrappedWithMarks = map (\b -> MarkedACFGB b Nothing) blockList
--   in
--     AugCFG size (map commitImDom (alg430 wrappedWithMarks empty entryIdList)) entryIdList
--   where commitImDom = \(MarkedACFGB (ACFGBlock b p s o t _i) im) ->
--           ACFGBlock b p s o t im


