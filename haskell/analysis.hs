-- This file should do analysis and manipulation of the PCode as
-- represented by the PCode Module.

-- TODO make this a nice list with shadowing (but I also want all the POpt constructors)
module Analysis where

import Data.List (find)

import PCode

-- -------------------------------------------------------------------
--
-- Sequences of instructions
--
-- Contigious sequences of instructions and things we might want to do
-- with them.

type PIStream = [PInst]
-- ^ Slightly more readable function types

data PBlock = PBlock {
  pBlockId :: Integer,
  stream :: [PInst],
  ghidraIsFunctionHead :: Bool,
  ghidraName :: Maybe String
  }
{- ^ These are a contiguous stream of instructions that we think are
 probably atomic with respect to clontrol flow. they have ids to talk
 about them indirectly. An id is a reference to the block starting at
 the associated instruction, though that mapping may be arbitary. This
 matters if you decide laters to split a block. The new block before
 the split should keep the same id, and the block after should get a
 new one. They also have some info carried in from ghidra, namely the
 name of the block, if it is associated with a label, and whether
 ghidra thinks this block is the start of a function.-}

terminating :: PBlock -> PInst
initial :: PBlock -> PInst
-- ^ just for readability and avoiding unwrapping
terminating (PBlock _id stream _ _) = last stream
initial (PBlock _id stream _ _) = head stream

splitBlock :: Integer -> PAddr -> PBlock -> Maybe (PBlock, PBlock)
-- ^ Split the Block at the given address, and return the truncated
-- original and the new successor, with the given id. Or don't, if
-- that PAddr isn't in this block.
splitBlock nid target (PBlock oid bstream func name) =
  let (before, after) = break ((== target) . location) bstream
  in
    if null after
    then
      Nothing
    else
      Just (PBlock oid before func name,
            PBlock nid after False Nothing)

splitBlockInList :: Integer -> PAddr -> [PBlock] -> Maybe [PBlock]
-- ^ Same thing, but just traverse the list for it and return the
-- replcaement list if possible. Nothing if the address doesn't match
-- anything. Assumes the list of blocks is in program order.
splitBlockInList nid target blockList =
  let (beforeAndCurrent, after) = break ((/= LT) . (comparePAddr target) . location . initial) blockList
      toSplit = last beforeAndCurrent
      prior = init beforeAndCurrent
  in
    if null beforeAndCurrent
    then
      Nothing
    else
      (splitBlock nid target toSplit) >>= (\(a,b) -> Just [a,b]) >>= (\newEntryList -> Just $ concat [prior, newEntryList, after])

-- -------------------------------------------------------------------
--
-- Sequences of blocks.
--
-- A collection of blocks that might reasonably go together. The
-- blocks of a function or several functions. These sequences must be
-- kept in program order.

data PBlockSeq = PBlockSeq {
  blocks :: [PBlock],
  pBSeqNextId :: Integer -- first unused id. Used for splits
  }
-- ^ Sequences of this type are expected to keep blocks in program order.

findByHead :: PBlockSeq -> PAddr -> Maybe Integer
-- ^ Just id of block with matching initial instruction in this seq,
-- or Nothing if there isn't one in this seq
findByHead (PBlockSeq blocks _) testAddr =
          (find (((flip at) testAddr) . initial)
            blocks) >>= (\(PBlock id _ _ _) -> Just id)

-- -------------------------------------------------------------------
--
-- Nodes of a CFG
--
-- A Block endowed with some data about its successors. The result of
-- branch analysis. Only useful in combination with others.

data TargetExcuse =
  -- Unanalyzed -- doesn't seem to come up
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
  | UncondTarget Integer
  | CondTarget Integer Integer -- target, then fallthrough
  | IndirectSet [Integer]      -- TODO will we ever use this?
  -- ^ These tell you about your terminating instruction and which
  -- nodes are immediately dominated. This type singals completed
  -- analysis for the sucessors of the associated node, otherwise use
  -- TargetExcuse

data CFGBlock = CFGBlock {
  block :: PBlock,
  succ :: Either TargetExcuse Successor -- ^ IDS of blocks in the same graph that this dominates
  }
  {- ^ A single node in a CFG -}

splitCFGB :: Integer -> PAddr -> CFGBlock -> Maybe (CFGBlock, CFGBlock)
-- ^ Lift a block split to work on nodes. The later block inherits the
-- dominated blocks of the prior, and the prior gets just the later as
-- a fallthrough.
splitCFGB nid target (CFGBlock oblock osucc) =
  (splitBlock nid target oblock) >>= (\(a,b) ->
                                        Just (
                                         CFGBlock a (Right (Fallthrough nid)),
                                         CFGBlock b osucc
                                         ))

splitCFGBInList :: Integer -> PAddr -> [CFGBlock] -> Maybe [CFGBlock]
-- ^ Same thing, but just traverse the list for it and return the
-- replcaement list if possible. Nothing if the address doesn't match
-- anything
splitCFGBInList nid target nodeList =
  let (beforeAndCurrent, after) = break ((/= LT) . (comparePAddr target) . location . initial . block) nodeList
      toSplit = last beforeAndCurrent
      prior = init beforeAndCurrent
  in
    if null beforeAndCurrent
    then
      Nothing
    else
      (splitCFGB nid target toSplit) >>= (\(a,b) -> Just [a,b]) >>= (\newEntryList -> Just $ concat [prior, newEntryList, after])

data CFG = CFG {
  nodes :: [CFGBlock],
  cfgNextId :: Integer, -- the first unused id, for use when splitting nodes
  cfgIsStable :: Bool   -- True if it is safe to assume that all
                        -- control flow paths have been discovered. We
                        -- can do non-reversable and other nastier
                        -- transformations and analysis. This means
                        -- that all successors are non-excuses.
  }
-- ^ A (likely) connected set of blocks seperated by control flow
-- instructions or control flow targets.
--
-- Note that our analysis at the moment does not extend to indirect
-- branches, so when using or translating this structure, either
-- handle that yourself or warn / panic at the sign of indirection.

-- -------------------------------------------------------------------
--
-- Build the augmented types by doing analysis along the way

-- TODO do I still need this? I think so, but I don't recall why I
-- commented it.

-- terminatingInstSplit :: PIStream -> PBlockSeq
-- -- ^ Split into blocks that end with a control flow
-- -- instruction. Returns the blocks and the first unused id number
-- terminatingInstSplit stream =
--   uncurry PBlockSeq $ tis' stream 0
--   where tis' :: PIStream -> Integer-> ([PBlock], Integer)
--         tis' strm free =
--           if null strm
--           then
--             ([], free) -- ^ base case
--           else
--             let (first, cf:rest) = break controlFlowP strm
--                 (tailL, unusedId) = tis' rest (free + 1)
--             in
--               ((PBlock free first False Nothing):tailL, unusedId)

-- TODO we need a way to read in a PBlockSeq based on the dumping script

-- TODO v This function is really really messy. That's because it
-- basically doesn't have any easily broken off subroutines, and while
-- there is a lot of very similar code, there is basically no code
-- reuse between case arms. And what little there is can't even be
-- patched up with contiuations or generic types or anything slick
-- like that to have code reuse sandwiched by specialization. At least
-- that's what it looks like to me at the moment.
linkCFG :: PBlockSeq -> [CFGBlock] -> CFG
{- ^ Gnarly. Does the messy job of coverting the block list one at a
 time into CFG nodes that have id references to the immediately
 dominated successor blocks for each input block. Recurses and
 consumes from the front of the block sequence and appends to the back
 of the node sequence. Passes both down. This split list recusion
 allows us to mutate (notably split) both the consumed and unconsumed
 parts of the sequence when we find a target that splits a block. -}
linkCFG seq@(PBlockSeq rawBlockStream unusedId) prefix =
  if null rawBlockStream
  then
    -- base case. No more blocks to analyze
    CFG prefix unusedId False
  else
    -- TODO could this let block be partially a where block? I need
    -- that null check for safety, but laziness might make it work
    let curBlock = head rawBlockStream
        blockStream = tail rawBlockStream
        curInst = terminating curBlock
        target = branchTarget curInst
        tOpt = opt curInst
        single :: Either TargetExcuse Sucessor -- only safe on BRANCH/CALL/CBRANCH
        single = case (findByHead seq target) of
                   Just id -> Right $ UncondTarget id
                   Nothing -> Left $ UncondNeedSplit target
        successor = case tOpt of
                      BRANCH _ -> single
                      CALL _ -> single
                      CBRANCH _ _ ->
                        case blockStream of
                          (PBlock nextId _ _ _):_rest ->
                            -- there is some next block
                            case findByHead seq target of
                              Just targetId -> Right (CondTarget targetId nextId)
                              Nothing -> Left (CondNeedSplit target nextId)
                          otherwise -> error $ "CBRANCH fallthrough fell off the sequence: " ++ (show curInst)
                      BRANCHIND _ -> Left Indirect
                      CALLIND _ -> Left Indirect
                      RETURN _ -> Left Indirect
                      otherwise -> error $ "Unexpected instruction type during CFG branch target discovery. This shouldn't happen: " ++ (show curInst)
    in
      -- We have a sucessor, of the sort that CFGBlock expects, but we might still have to split a block somewhere
      case successor of
        Right succ -> -- We are done, no splitting necessary.
          linkCFG (PBlockSeq blockStream unusedId) $ prefix ++ [CFGBlock curBlock (Right succ)]
        Left (UncondNeedSplit splitAddr) ->
          case splitCFGBInList unusedId splitAddr prefix of
            Just newPrefix ->
              -- It worked, we found it. Now just recurse as normal, linking our newly inserted block
              linkCFG (PBlockSeq blockStream (unusedId + 1)) $ newPrefix ++ [CFGBlock curBlock (Right $ UncondTarget unusedId)]
            Nothing ->
              -- It didn't work, we need to look for a block that we haven't yet converted
              case splitBlockInList unusedId splitAddr (curBlock:blockStream) of
                Just (newBSHead:newBSTail) ->
                  -- Found it, recurse. We need to use the replacement
                  -- version of blockStream for both head and tail
                  -- since we might have split the block we are
                  -- currently analyzing
                  linkCFG (PBlockSeq newBSTail (unusedId + 1)) $ prefix ++ [CFGBlock newBSHead (Right $ UncondTarget unusedId)]
                Nothing -> error $ "Couldn't locate the address to split on during CFG first pass: " ++ (show curInst)
        Left (CondNeedSplit splitAddr fallthroughId) ->
          case splitCFGBInList unusedId splitAddr prefix of
            Just newPrefix ->
              -- It worked, we found it. Now just recurse as normal, linking our newly inserted block
              linkCFG (PBlockSeq blockStream (unusedId + 1)) $ newPrefix ++ [CFGBlock curBlock (Right $ CondTarget unusedId fallthroughId)]
            Nothing ->
              -- It didn't work, we need to look for a block that we haven't yet converted
              case splitBlockInList unusedId splitAddr (curBlock:blockStream) of
                Just (newBSHead:newBSTail) ->
                  -- Found it, recurse. We need to use the replacement
                  -- version of blockStream for both head and tail
                  -- since we might have split the block we are
                  -- currently analyzing
                  linkCFG (PBlockSeq newBSTail (unusedId + 1)) $ prefix ++ [CFGBlock newBSHead (Right $ CondTarget unusedId fallthroughId)]
                Nothing -> error $ "Couldn't locate the address to split on during CFG first pass: " ++ (show curInst)

initialGraph :: PBlockSeq -> CFG
-- ^ Do the simple connecting of blocks using fallthroughs and
-- addresses.
initialGraph seq@(PBlockSeq blocks unusedId) = linkCFG seq []

cfgCommitNonIndirect :: CFG -> CFG
-- ^ Sets the stable flag in the event that this CFG has no non-return
-- indirect jumps. Does nothing if flag is already set
cfgCommitNonIndirect input@(CFG blocks id stable) =
  if stable
  then
    input
  else
    CFG blocks id $ foldr (\rightBlock leftValue ->
                             leftValue || (isInd . terminating) rightBlock) False $ map block blocks -- just pull the PBlock out of each CFGBlock
  where
    isInd (PInst _ _ (BRANCHIND _)) = True
    isInd (PInst _ _ (CALLIND _)) = True
    isInd (PInst _ _ _) = False

-- TODO not sure where to put this, but currently I am working under
-- the assumption that the initial block seq that is passed in for the
-- CFG first pass is of ALL the functions, since a call will basically
-- always take you out of the current function.
--
-- We are plumbing the function info through now, but the idea of
-- doing this parsing at the function scale is very desirable. We need
-- to edit the above to allow branches out of the current CFG, and we
-- should pull that info to the top of the CFG I think. So each
-- function CFG is itself a node of a function level CFG. Doing the
-- whole program in one giant CFG is tempting, but will exascerbate
-- the problems we will have with indirect control flow I think,
-- particularly returns.
--
-- The way to do this in my opinion is turn ids from an Integer to
-- somethink like `Internal Integer | External PAddr`. That way it's
-- still fully analyzed (ie no room for indirection nonsense), but
-- gives enough info to sew it back together if you have the acfg that
-- the target it in.


-- -------------------------------------------------------------------
-- augmented (concrete) CFGs
--
-- If we are confident we have a complete list of control flow
-- locations (see cfgIsStable), then we can start with analysis,
-- including touched/observed listings at the block level, backlinks
-- for control flow, and immediate dominators. These are useful, but
-- primarily needed for determining block argument detection, since we
-- can't just translate the phi expressions. We need to re-discover
-- which things need to be arugments in the cruicble arg-passing cfg
-- style.

-- TODO we need to be careful, since varnodes don't have the disjoint
-- nature of registers. I need to check what the llvm IR's take on
-- this problem is, but we should
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

-- TODO is this worth rewriting with lenses, or explicit setters?
data ACFGBlock = ACFGBlock {
  acfgBlock :: PBlock,
  acfgPreds :: [Integer],
  acfgSuccs :: Sucessor, -- distinguish what kind of downlinks these are, and to where
  acfgObserves :: [VarNode],
  acfgTouches :: [VarNode],
  acfgImmDomBy :: Maybe Integer -- closest stricting dominating node.
  }

-- TODO careful during touch/observe conflict accumulation to include
-- self as a block if there is a loop that contains the current block.

-- Since the cfg is concrete, and downlinks are annotated, we can
-- discard program order, and store blocks in id order, so we can do
-- constant time lookup.
--
-- TODO when I feed into crucible, does that have to be in program
-- order? How can I teach crucible about the weirdness of pcode
-- addresses? Do I need to?
data AugCFG = AugCFG {
  acfgSize :: Integer,
  acfgBlockList :: [ACFGBlock], --of length matching size
  acfgEntries :: [Integer]      --places execution can begin
  }
-- No way we are constructing all of this in one pass, so we will do it in steps

acfgBlockId :: ACFGBlock -> Integer
-- ^ save unwrapping
acfgBlockId (ACFGBlock block _ _ _ _ _) = pBlockId block

basicACFGBlockTranslate :: CFGBlock -> ACFGBlock
-- ^ Just get us into the right type. Leave new fields empty.
basicACFGBlockTranslate (CFGBlock block succ) =
  ACFGBlock block [] succ [] [] Nothing

-- TODO ok so this is correct, but leads to vast overestimates of
-- connectedness, since a block that writes and then reads the same
-- location will still have that location marked as a
-- observation. This is hugely undesirable.
--
-- We can solve that with intersection analysis and eliminate
-- observations that are subsets of prior touches. We probably want to
-- build some kind of interval tree, but we also need to care about
-- address spaces, (one per should be fine?) and temporal ordering
-- (mark at node, again not undoable)
acfgBlockAccumulateEffects :: ACFGBlock -> ACFGBlock
-- ^ Populate the observes and touches fields
acfgBlockAccumulateEffects (ACFGBlock pblock@(PBlock _id instList _ _) pred succ _ _ im) =
  let (oList, tList) = foldr (combine . opt) ([], []) instList
  in
    -- note that these lists are still backwards. If you want them to
    -- be minimal, like the todo above suggests, or do other
    -- processing, that should happen here rather than later.
    ACFGBlock pblock pred succ olist tList im
  where combine popt (obsList, touchList) =
          -- with foldr, produces a pair of lists for o/t'd varnodes
          -- in reverse order from the current block
          ((observes popt):obsList, (touches popt):touchList)

putInIdOrder :: [CFGBlock] -> [CFGBlock]
-- ^ Just sorts based on id. You should have a list from id zero to
-- length-1, but I won't be explicitly checking that.
putInIdOrder cfgBList =
  sortBy (compare . pBlockId . block) cfgBList

populateBackLinks [ACFGBlock] -> [ACFGBlock]
-- ^ Do an update pass over the blocks, populating the reverse cfg
-- edges
populateBackLinks aList =
  pbl [] aList
  where pbl before after =
          if null after
          then before
          else
            -- take head of after, and update each target
            let cur = head after
                curId = acfgBlockId cur
                ids = sort $ case acfgSuccs cur of
                               Fallthrough id -> [id]
                               UncondTarget id -> [id]
                               CondTarget id1 id2 -> [id1, id2]
                               IndirectSet idSet -> idSet
            in
              -- TODO we can skip before if ((head ids) > curId)
              -- should be doable with monads, but either is backwards
              -- here
              case updateInList before ids curId of
                Right newBefore -> newBefore ++ after
                Left remainingKeys ->
                  case updateInList after ids curId of
                    Right newAfter -> before ++ newAfter
                    Left ->
                      error $ "id successor out of acfg in populateBackLinks: " ++ (show curId)
        updateInList alist keyList curId =
          -- both sorted
          if null alist
          then Left keyList
          else
            if null keyList
            then
              Right alist
            else
                let h = head alist
                    k = head keyList
                in
                  case compare (acfgBlockId h) k of
                    LT -> (updateInList (tail alist) keyList curId) >>=
                      (\ tailL -> Right (h:tailL))
                    EQ -> (updateInList (tail alist) (tail keyList) curId) >>=
                      (\ tailL -> Right ((appendPred h curId):tailL))
                    GT -> error $ "Are you missing ids? greater than between sorted list heads in populateBackLinks: " ++ (show k)
        appendPred (ACFGBlock b predL s o t i) id =
          ACFGBlock p id:predL s o t i

-- TODO next:
--
-- alg 430 version to do Immediate dominator marking
-- block arg reconstruction (interval analysis? or just overestimate? See Prior TODO)
-- data type for blocks with formal args?
-- pipeline functions
-- CFG -> ACFG -> block_arg'd_cfg
