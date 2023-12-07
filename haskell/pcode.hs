-- This file defines the PCode module. It's small and is limited to
-- (roughly) the units that the P-code spec cares about. Notably
-- sequences of instructions and CFGs are not here, but in the
-- analysis module instead.

module PCode where

data VarNode = VarNode {
  addrSpace :: String,
  vnOffset :: Integer,
  vnLength :: Integer
  } deriving (Show, Read, Eq)
-- ^ (Address Space Id, offset (signed for constants), Length/Size)
-- Length should always be >= 0 I think

intersect :: VarNode -> VarNode -> Maybe VarNode
-- ^ Commutative intersection. Local to address space.
intersect a@(VarNode as ao al) b@(VarNode bs bo bl)
  | a0 > b0 = intersect b a
  | as /= bs = Nothing
  | otherwise =
    let ae = ao + al
        be = bo + bl
    in
      if bo > ae
      then
        Nothing
      else
        Just $ VarNode as bo $ (min (ae - bo) be) - bo

difference :: VarNode -> VarNode -> Maybe VarNode
-- ^ Non-commutative difference. Gives A \ (A n B), the remainder of a
-- after cutting out b.
difference a@(VarNode as ao al) b@(VarNode bs bo bl)
  | as /= bs = Nothing
  | otherwise =
    Just VarNode as ao $ (min (ao + al) bo) - ao

compareStart :: VarNode -> VarNode -> Maybe Ord
-- ^ Partial order on varnodes by starting address. Only defined in
-- the same address space
compareStart a@(VarNode as ao al) b@(VarNode bs bo bl)
  | as /= bs = Nothing
  | otherwise = Just compare ao bo

compareEnd :: VarNode -> VarNode -> Maybe Ord
-- ^ Partial order on varnodes by end address. Only defined in
-- the same address space
compareEnd a@(VarNode as ao al) b@(VarNode bs bo bl)
  | as /= bs = Nothing
  | otherwise = Just compare (ao + al) (bo + bl)

compareContains :: VarNode -> VarNode -> Maybe Ord
-- ^ Partial order on varnodes by interval inclusion. A > B if A
-- includes all of B. Only defined in the same address space.
compareContains a b
  | a0 > b0 = (compareContains b a) >>= (\case
                                            GT -> Just LT
                                            EQ -> Just EQ
                                            LT -> error "non-symmetric < during compareContains")
  | otherwise =
      surrounding <$> (compareStart a b) <*> (compareEnd a b)
  where surrounding = Just . (\cases { LT GT -> GT; EQ GT -> GT; GT GT -> LT;
                                       LT EQ -> GT; EQ EQ -> EQ; GT EQ -> LT;
                                       LT LT -> LT; EQ LT -> LT; GT LT -> LT})



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

-- TODO define these for inst or for opt?
-- recall that the varnode order is inputs then output
observes :: POpt -> [VarNode]
-- ^ What VarNodes does this operation look at? Which are inputs
-- basically. Note that this doesn't solve the problem of runtime
-- address memory reads. It counts the offset as an input, but doesn't
-- count the spot in memory. Do points-to analysis or something else
-- for that.
observes (COPY va _o)               = [va]
observes (INT_ADD va vb _o)         = [va, vb]
observes (BOOL_OR va vb _o)         = [va, vb]
observes (LOAD _const_id vb _o)     = [vb]
observes (INT_SUB va vb _o)         = [va, vb]
observes (FLOAT_EQUAL va vb _o)     = [va, vb]
observes (STORE _const_id vb _o)    = [vb]
observes (INT_CARRY va vb _o)       = [va, vb]
observes (FLOAT_NOTEQUAL va vb _o)  = [va, vb]
observes (BRANCH _target)           = [] --constant in that its value doesn't matter
observes (INT_SCARRY va vb _o)      = [va, vb]
observes (FLOAT_LESS va vb _o)      = [va, vb]
observes (CBRANCH _target vb)       = [vb]
observes (INT_SBORROW va vb _o)     = [va, vb]
observes (FLOAT_LESSEQUAL va vb _o) = [va, vb]
observes (BRANCHIND va)             = [va] --not handled specially here
observes (INT_2COMP va _o)          = [va]
observes (FLOAT_ADD va vb _o)       = [va, vb]
observes (CALL _target)             = [] --see branch
observes (INT_NEGATE va _o)         = [va]
observes (FLOAT_SUB va vb _o)       = [va, vb]
observes (CALLIND va)               = [va]
observes (INT_XOR va vb _o)         = [va, vb]
observes (FLOAT_MULT va vb _o)      = [va, vb]
observes (INT_AND va vb _o)         = [va, vb]
observes (FLOAT_DIV va vb _o)       = [va, vb]
observes (RETURN va)                = [va]
observes (INT_OR va vb _o)          = [va, vb]
observes (FLOAT_NEG va _o)          = [va]
observes (PIECE va vb _o)           = [va, vb]
observes (INT_LEFT va vb _o)        = [va, vb]
observes (FLOAT_ABS va _o)          = [va]
observes (SUBPIECE va vb _o)        = [va, vb]
observes (INT_RIGHT va vb _o)       = [va, vb]
observes (FLOAT_SQRT va _o)         = [va]
observes (INT_EQUAL va vb _o)       = [va, vb]
observes (INT_SRIGHT va vb _o)      = [va, vb]
observes (FLOAT_CEIL va _o)         = [va]
observes (INT_NOTEQUAL va vb _o)    = [va, vb]
observes (INT_MULT va vb _o)        = [va, vb]
observes (FLOAT_FLOOR va _o)        = [va]
observes (INT_LESS va vb _o)        = [va, vb]
observes (INT_DIV va vb _o)         = [va, vb]
observes (FLOAT_ROUND va _o)        = [va]
observes (INT_SLESS va vb _o)       = [va, vb]
observes (INT_REM va vb _o)         = [va, vb]
observes (FLOAT_NAN va _o)          = [va]
observes (INT_LESSEQUAL va vb _o)   = [va, vb]
observes (INT_SDIV va vb _o)        = [va, vb]
observes (INT2FLOAT va _o)          = [va]
observes (INT_SLESSEQUAL va vb _o)  = [va, vb]
observes (INT_SREM va vb _o)        = [va, vb]
observes (FLOAT2FLOAT va _o)        = [va]
observes (INT_ZEXT va _o)           = [va]
observes (BOOL_NEGATE va _o)        = [va]
observes (TRUNC va _o)              = [va]
observes (INT_SEXT va _o)           = [va]
observes (BOOL_XOR va vb _o)        = [va, vb]
observes (BOOL_AND va vb _o)        = [va, vb]
observes (POPCOUNT va _o)           = [va]

touches :: POpt -> [VarNode]
-- ^ What VarNodes does this operation produce at? Which varnodes are
-- outputs basically.
touches (COPY _va out)                = [out]
touches (INT_ADD _va _vb out)         = [out]
touches (BOOL_OR _va _vb out)         = [out]
touches (LOAD _const_id _vb out)      = [out]
touches (INT_SUB _va _vb out)         = [out]
touches (FLOAT_EQUAL _va _vb out)     = [out]
touches (STORE _const_id _vb out)     = [out]
touches (INT_CARRY _va _vb out)       = [out]
touches (FLOAT_NOTEQUAL _va _vb out)  = [out]
touches (BRANCH _target)              = []
touches (INT_SCARRY _va _vb out)      = [out]
touches (FLOAT_LESS _va _vb out)      = [out]
touches (CBRANCH _target vb)          = []
touches (INT_SBORROW _va _vb out)     = [out]
touches (FLOAT_LESSEQUAL _va _vb out) = [out]
touches (BRANCHIND va)                = []
touches (INT_2COMP _va out)           = [out]
touches (FLOAT_ADD _va _vb out)       = [out]
touches (CALL _target)                = []
touches (INT_NEGATE _va out)          = [out]
touches (FLOAT_SUB _va _vb out)       = [out]
touches (CALLIND va)                  = []
touches (INT_XOR _va _vb out)         = [out]
touches (FLOAT_MULT _va _vb out)      = [out]
touches (INT_AND _va _vb out)         = [out]
touches (FLOAT_DIV _va _vb out)       = [out]
touches (RETURN va)                   = []
touches (INT_OR _va _vb out)          = [out]
touches (FLOAT_NEG _va out)           = [out]
touches (PIECE _va _vb out)           = [out]
touches (INT_LEFT _va _vb out)        = [out]
touches (FLOAT_ABS _va out)           = [out]
touches (SUBPIECE _va _vb out)        = [out]
touches (INT_RIGHT _va _vb out)       = [out]
touches (FLOAT_SQRT _va out)          = [out]
touches (INT_EQUAL _va _vb out)       = [out]
touches (INT_SRIGHT _va _vb out)      = [out]
touches (FLOAT_CEIL _va out)          = [out]
touches (INT_NOTEQUAL _va _vb out)    = [out]
touches (INT_MULT _va _vb out)        = [out]
touches (FLOAT_FLOOR _va out)         = [out]
touches (INT_LESS _va _vb out)        = [out]
touches (INT_DIV _va _vb out)         = [out]
touches (FLOAT_ROUND _va out)         = [out]
touches (INT_SLESS _va _vb out)       = [out]
touches (INT_REM _va _vb out)         = [out]
touches (FLOAT_NAN _va out)           = [out]
touches (INT_LESSEQUAL _va _vb out)   = [out]
touches (INT_SDIV _va _vb out)        = [out]
touches (INT2FLOAT _va out)           = [out]
touches (INT_SLESSEQUAL _va _vb out)  = [out]
touches (INT_SREM _va _vb out)        = [out]
touches (FLOAT2FLOAT _va out)         = [out]
touches (INT_ZEXT _va out)            = [out]
touches (BOOL_NEGATE _va out)         = [out]
touches (TRUNC _va out)               = [out]
touches (INT_SEXT _va out)            = [out]
touches (BOOL_XOR _va _vb out)        = [out]
touches (BOOL_AND _va _vb out)        = [out]
touches (POPCOUNT _va out)            = [out]

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

