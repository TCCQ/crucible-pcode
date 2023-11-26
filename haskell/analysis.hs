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
  id :: Integer,
  stream :: [PInst]
  }
{- ^ These are a contiguous stream of instructions that we think are
 probably atomic with respect to clontrol flow. they have ids to talk
 about them indirectly. An id is a reference to the block starting at
 the associated instruction, though that mapping may be arbitary. This
 matters if you decide laters to split a block. The new block before
 the split should keep the same id, and the block after should get a
 new one. -}

terminating :: PBlock -> PInst
initial :: PBlock -> PInst
-- ^ just for readability and avoiding unwrapping
terminating (PBlock _id stream) = last stream
initial (PBlock _id stream) = head stream

splitBlock :: Integer -> PAddr -> PBlock -> Maybe (PBlock, PBlock)
-- ^ Split the Block at the given address, and return the truncated
-- original and the new successor, with the given id. Or don't, if
-- that PAddr isn't in this block.
splitBlock nid target (PBlock oid bstream) =
  let (before, after) = break ((== target) . location) bstream
  in
    if null after
    then
      Nothing
    else
      Just (PBlock oid before,
            PBlock nid after)

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
            blocks) >>= (\(PBlock id _) -> Just id)

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

data Sucessor =
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
  dom :: Either TargetExcuse Sucessor -- ^ IDS of blocks in the same graph that this dominates
  }
  {- ^ A single node in a CFG -}

splitCFGB :: Integer -> PAddr -> CFGBlock -> Maybe (CFGBlock, CFGBlock)
-- ^ Lift a block split to work on nodes. The later block inherits the
-- dominated blocks of the prior, and the prior gets just the later as
-- a fallthrough.
splitCFGB nid target (CFGBlock oblock odom) =
  (splitBlock nid target oblock) >>= (\(a,b) ->
                                        Just (
                                         CFGBlock a (Right (Fallthrough nid)),
                                         CFGBlock b odom
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
  cFGNextId :: Integer -- the first unused id, for use when splitting nodes
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

terminatingInstSplit :: PIStream -> PBlockSeq
-- ^ Split into blocks that end with a control flow
-- instruction. Returns the blocks and the first unused id number
terminatingInstSplit stream =
  uncurry PBlockSeq $ tis' stream 0
  where tis' :: PIStream -> Integer-> ([PBlock], Integer)
        tis' strm free =
          if null strm
          then
            ([], free) -- ^ base case
          else
            let (first, cf:rest) = break controlFlowP strm
                (tailL, unusedId) = tis' rest (free + 1)
            in
              ((PBlock free first):tailL, unusedId)

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
    CFG prefix unusedId
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
                          (PBlock nextId _):_rest ->
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



-- TODO not sure where to put this, but currently I am working under
-- the assumption that the initial block seq that is passed in for the
-- CFG first pass is of ALL the functions, since a call will basically
-- always take you out of the current function. Thus we should have
-- another pass after the initial CFG pass to mark which blocks are
-- the heads of functions and which aren't. Note that we have that
-- info from the dump, but currently it is being ignored.
