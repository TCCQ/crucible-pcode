-- This file should do analysis and manipulation of the PCode as
-- represented by the PCode Module.

import PCode



controlFlowP :: PInst -> Bool
-- ^ Is this instruction a control flow instruction?
controlFlowP (PInst _ popt _) =
          case popt of
            BRANCH _ -> True
            CBRANCH _ _ -> True
            BRANCHIND _ -> True
            CALL _ -> True
            CALLIND _ -> True
            RETURN _ -> True
            otherwise -> False

indirectP :: PInst -> Bool
-- ^ Does this instruction have runtime calculated control flow targets?
indirectP (PInst _ popt _) =
          case popt of
            BRANCHIND _ -> True
            CALLIND _ -> True
            RETURN _ -> True
            otherwise -> False

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
      -- we have a sucessor, of the sort that CFGBlock expects, but we might still have to split a block somewhere
      case successor of
        Right succ -> --we are done, no splitting necessary. We rewrap to change the Left type
          linkCFG (PBlockSeq blockStream unusedId) $ prefix ++ [CFGBlock curBlock (Right succ)]
        Left (UncondNeedSplit splitAddr) ->
          case splitCFGBInList unusedId splitAddr prefix of
            Just newPrefix ->
              -- It worked, we found it, now just recurse as normal, linking our newly inserted block
              linkCFG (PBlockSeq blockStream (unusedId + 1)) $ newPrefix ++ [CFGBlock curBlock (Right $ UncondTarget unusedId)]
            Nothing ->
              -- It didn't work, we need to look for a block that we haven't yet converted
              case splitBlockInList unusedId splitAddr (curBlock:blockStream) of
                Just (newBSHead:newBSTail) ->
                  -- found it, reucurse. We need to use the
                  -- replacement version of blockStream since we might
                  -- have split the block we are currently analyzing
                  linkCFG (PBlockSeq newBSTail (unusedId + 1)) $ prefix ++ [CFGBlock newBSHead (Right $ UncondTarget unusedId)]
                Nothing -> error $ "Couldn't locate the address to split on during CFG first pass: " ++ (show curInst)
        Left (CondNeedSplit splitAddr fallthroughId) ->
          case splitCFGBInList unusedId splitAddr prefix of
            Just newPrefix ->
              -- It worked, we found it, now just recurse as normal, linking our newly inserted block
              linkCFG (PBlockSeq blockStream (unusedId + 1)) $ newPrefix ++ [CFGBlock curBlock (Right $ CondTarget unusedId fallthroughId)]
            Nothing ->
              -- It didn't work, we need to look for a block that we haven't yet converted
              case splitBlockInList unusedId splitAddr (curBlock:blockStream) of
                Just (newBSHead:newBSTail) ->
                  -- found it, reucurse. We need to use the
                  -- replacement version of blockStream for both head
                  -- and tail since we might have split the block we
                  -- are currently analyzing
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
