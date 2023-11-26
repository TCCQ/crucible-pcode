-- This file should have types that will be of general use

-- TODO make this a nice list with shadowing ( but I also want all the POpt constructors)
module PCode where

import Numeric (showHex)
import Data.Text (Text, pack, unpack, empty, strip, splitOn)
import Data.Text.Read (decimal, hexadecimal)
import Data.List (elemIndex, stripPrefix, find)
import Data.Either (fromRight)
import Debug.Trace (trace)
-- import Data.Matrix



-- (Address Space Id, offset (signed for constants), Length/Size)
-- Length should always be >= 0 I think
data VarNode = VarNode {
  addrSpace :: String,
  vnOffset :: Integer,
  vnLength :: Integer
  } deriving (Show, Read)

-- -------------------------------------------------------------------
-- Pcode operations.
--
-- In general the order is the inputs in order and the output if there
-- is one.
--
-- See the reference, the formal semantics, or my writeup for what
-- they do.


-- Type for a single Pcode instruction.
data POpt =
  COPY VarNode VarNode |
  INT_ADD VarNode VarNode VarNode |
  BOOL_OR VarNode VarNode VarNode |
  LOAD VarNode VarNode VarNode |
  INT_SUB VarNode VarNode VarNode |
  FLOAT_EQUAL VarNode VarNode VarNode |
  STORE VarNode VarNode VarNode |
  INT_CARRY VarNode VarNode VarNode |
  FLOAT_NOTEQUAL VarNode VarNode VarNode |
  BRANCH VarNode | -- See the reference for interpretation
  INT_SCARRY VarNode VarNode VarNode |
  FLOAT_LESS VarNode VarNode VarNode |
  CBRANCH VarNode VarNode |
  INT_SBORROW VarNode VarNode VarNode |
  FLOAT_LESSEQUAL VarNode VarNode VarNode |
  BRANCHIND VarNode |
  INT_2COMP VarNode VarNode |
  FLOAT_ADD VarNode VarNode VarNode |
  CALL VarNode | -- In raw Pcode only ever one input
  INT_NEGATE VarNode VarNode |
  FLOAT_SUB VarNode VarNode VarNode |
  CALLIND VarNode |
  INT_XOR VarNode VarNode VarNode |
  FLOAT_MULT VarNode VarNode VarNode |
  INT_AND VarNode VarNode VarNode |
  FLOAT_DIV VarNode VarNode VarNode |
  RETURN VarNode | -- In raw Pcode only ever one input
  INT_OR VarNode VarNode VarNode |
  FLOAT_NEG VarNode VarNode |
  PIECE VarNode VarNode VarNode |
  INT_LEFT VarNode VarNode VarNode |
  FLOAT_ABS VarNode VarNode |
  SUBPIECE VarNode VarNode VarNode | -- Second input should be constant
  INT_RIGHT VarNode VarNode VarNode |
  FLOAT_SQRT VarNode VarNode |
  INT_EQUAL VarNode VarNode VarNode |
  INT_SRIGHT VarNode VarNode VarNode |
  FLOAT_CEIL VarNode VarNode |
  INT_NOTEQUAL VarNode VarNode VarNode |
  INT_MULT VarNode VarNode VarNode |
  FLOAT_FLOOR VarNode VarNode |
  INT_LESS VarNode VarNode VarNode |
  INT_DIV VarNode VarNode VarNode |
  FLOAT_ROUND VarNode VarNode |
  INT_SLESS VarNode VarNode VarNode |
  INT_REM VarNode VarNode VarNode |
  FLOAT_NAN VarNode VarNode |
  INT_LESSEQUAL VarNode VarNode VarNode |
  INT_SDIV VarNode VarNode VarNode |
  INT2FLOAT VarNode VarNode |
  INT_SLESSEQUAL VarNode VarNode VarNode |
  INT_SREM VarNode VarNode VarNode |
  FLOAT2FLOAT VarNode VarNode |
  INT_ZEXT VarNode VarNode |
  BOOL_NEGATE VarNode VarNode |
  TRUNC VarNode VarNode |
  INT_SEXT VarNode VarNode |
  BOOL_XOR VarNode VarNode VarNode |
  BOOL_AND VarNode VarNode VarNode |
  POPCOUNT VarNode VarNode
  deriving (Show, Read)

-- These Shouldn't appear in raw pcode
-- data CPOOLREF = -- variadic
-- data NEW = -- variadic
-- data USERDEFINED = -- variadic

-- A Machine address, a Pcode operation and the index of said
-- operation inside the machine instruction. This encoding allows for
-- operation on sequences of this type without data loss.
--
-- (machine addr, Opt, P-code offset)
data PInst = PInst {
  maddr :: Integer,
  opt :: POpt,
  pIOffset :: Integer
  } deriving (Show, Read)


type PAddr = (Integer, Integer)
-- ^ Readability, (maddr, offset) pair
--
-- This isn't a newtype with instanced Ord because I don't want to
-- have to unwrap PInst every time
comparePAddr :: PAddr -> PAddr -> Ordering
(am, ap) `comparePAddr` (bm, bp) =
  case am `compare` bm of
    LT -> LT
    GT -> GT
    EQ -> ap `compare` bp

location :: PInst -> PAddr
location (PInst m _ o) = (m, o)

at :: PInst -> PAddr -> Bool
-- ^ Is this instruction here? likely want infix for smooth reading
at (PInst imaddr _opt ioff) (bmaddr, boff) =
  (imaddr == bmaddr) && (ioff == boff)

branchTarget :: PInst -> PAddr
{- ^ Extract the correct target for branches and conditional
branches. Do the discriminating between pcode relative and regular
here. Only cares about the non-fallthrough target if there is more
than one possible successor instruction. -}

-- TODO this is incomeplete, but I think we should be good? I think it
-- should be enforced at compile time?
branchTarget inst@(PInst maddr (BRANCH (VarNode space vnoffset length)) pioffset) =
  if space == "constant"
  then error "TODO check the pcode spec for branch"
  else
    if space == "" --TODO what is the other special one?
    then error "TODO check spec"
    else error $ "Branch with unexpected space: " ++ space
branchTarget inst@(PInst maddr (CBRANCH _condition (VarNode space vnoffset length)) pioffset) =
  error "TODO"
branchTarget inst@(PInst maddr (CALL (VarNode space vnoffset length)) pioffset) =
  error "TODO"

type PIStream = [PInst]

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

data PBlockSeq = PBlockSeq {
  blocks :: [PBlock],
  pBSeqNextId :: Integer
  }
-- ^ Sequences of this type are expected to keep blocks in program order.

findByHead :: PBlockSeq -> PAddr -> Maybe Integer
-- ^ Just id of matching block in this seq, or Nothing if there isn't
-- one in this seq
findByHead (PBlockSeq blocks _) testAddr =
          (find (((flip at) testAddr) . initial)
            blocks) >>= (\(PBlock id _) -> Just id)

data TargetExcuse =
  Unanalyzed
  | Indirect
  | UncondNeedSplit PAddr
  | CondNeedSplit PAddr Integer --with fallthrough id for CBRANCH
  -- ^ These are a mix of shorterm and long term excuses, but we need
  -- them to be together, since they can be discovered at the same
  -- time

data Sucessor =
  Fallthrough Integer
  | UncondTarget Integer
  | CondTarget Integer Integer -- target, then fallthrough
  | IndirectSet [Integer]      -- TODO will we ever use this?

data CFGBlock = CFGBlock {
  block :: PBlock,
  dom :: Either TargetExcuse Sucessor -- ^ IDS of blocks in the same graph that this dominates
  }
  {- ^ A single node in a CFG for a single function -}

selfId :: CFGBlock -> Integer
-- ^ Saves you unwrapping. Might not be necessary
selfId (CFGBlock (PBlock id _) _) = id

splitBlock :: Integer -> PAddr -> PBlock -> Maybe (PBlock, PBlock)
-- ^ Split the Block at the given address, and return the truncated
-- original and the new successor, with the given Id. Or don't, if
-- that PAddr isn't in this block.
splitBlock nid target (PBlock oid bstream) =
  let (before, after) = break ((== target) . location) bstream
  in
    if null after
    then
      Nothing
    else
      Just (
           PBlock oid before,
           PBlock nid after
           )

splitBlockInList :: Integer -> PAddr -> [PBlock] -> Maybe [PBlock]
-- ^ Same thing, but just traverse the list for it and return the
-- replcaement list if possible. Nothing if the address doesn't match
-- anything
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
-- replcaement list if possible Nothing if the address doesn't match
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
  cFGNextId :: Integer
  }



