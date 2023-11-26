-- This file defines the PCode module. It's small and is limited to
-- (roughly) the units that the P-code spec cares about. Notably
-- sequences of instructions and CFGs are not here, but in the
-- analysis module instead.

module PCode where

data VarNode = VarNode {
  addrSpace :: String,
  vnOffset :: Integer,
  vnLength :: Integer
  } deriving (Show, Read)
-- ^ (Address Space Id, offset (signed for constants), Length/Size)
-- Length should always be >= 0 I think

-- -------------------------------------------------------------------
-- Pcode operations.
--
-- In general the order is the inputs in order and the output if there
-- is one.
--
-- See the reference, the formal semantics, or my writeup for what
-- they do.

-- Type for a single Pcode operation.
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
-- These shouldn't appear in raw pcode
-- data CPOOLREF = -- variadic
-- data NEW = -- variadic
-- data USERDEFINED = -- variadic

data PInst = PInst {
  maddr :: Integer,
  pIOffset :: Integer,
  opt :: POpt
  } deriving (Show, Read)
-- ^ A Machine address, the index of the operation inside the machine
-- instruction, and a Pcode operation. This encoding allows for
-- operation on sequences of this type without data loss.

controlFlowP :: PInst -> Bool
-- ^ Is this instruction a control flow instruction?
controlFlowP (PInst _ _ popt) =
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
indirectP (PInst _ _ popt) =
          case popt of
            BRANCHIND _ -> True
            CALLIND _ -> True
            RETURN _ -> True
            otherwise -> False

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
-- ^ Where is this instruction? readability and saved unwrapping
location (PInst m o _) = (m, o)

at :: PInst -> PAddr -> Bool
-- ^ Is this instruction here? likely want infix for smooth reading
at (PInst imaddr ioff _opt) (bmaddr, boff) =
  (imaddr == bmaddr) && (ioff == boff)

branchTarget :: PInst -> PAddr
{- ^ Extract the correct target for branches and conditional
branches. Do the discriminating between pcode relative and regular
here. Only cares about the non-fallthrough target if there is more
than one possible successor instruction. -}
branchTarget inst@(PInst maddr pioffset (BRANCH (VarNode space vnoffset length))) =
  if space == "constant"
  then error "TODO check the pcode spec for branch"
  else
    if space == "" --TODO what is the other special one?
    then error "TODO check spec"
    else error $ "Branch with unexpected space: " ++ space
branchTarget inst@(PInst maddr pioffset (CBRANCH _condition (VarNode space vnoffset length))) =
  error "TODO"
branchTarget inst@(PInst maddr pioffset (CALL (VarNode space vnoffset length))) =
  error "TODO"
-- TODO this is incomeplete, but I think we should be good? I think it
-- should be enforced at compile time?

