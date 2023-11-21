-- This file should have types that will be of general use

-- TODO make this a nice list with shadowing ( but I also want all the POpt constructors)
module PCode where

import Numeric (showHex)
import Data.Text (Text, pack, unpack, empty, strip, splitOn)
import Data.Text.Read (decimal, hexadecimal)
import Data.List (elemIndex, stripPrefix)
import Data.Either (fromRight)
import Debug.Trace (trace)
-- import Data.Matrix



-- (Address Space Id, offset (signed for constants), Length/Size)
-- Length should always be >= 0 I think
data VarNode = VarNode String Integer Integer deriving (Show, Read)

-- getters
-- addressSpace :: VarNode -> String
-- addressSpace (VarNode as _ _) = as

-- offset :: VarNode -> Integer
-- offset (VarNode _ off _) = off

-- length :: VarNode -> Integer
-- length (VarNode _ _ len) = len

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
data PInst = PInst Integer POpt Integer deriving (Show, Read)

-- -------------------------------------------------------------------
--
-- Blocks are sequences of Pcode that we have reason to believe are
-- atomic with respect to control flow.
--
-- Note that full confidence requires analysis of indirect branches

-- Atomic block with respect to control flow.
data PBlock = PBlock [PInst]

instance Show (PBlock) where
  show (PBlock list) =
    concat $ map ((++"\n") . show) list

-- -------------------------------------------------------------------
-- Function blocks
--
-- A high level block of code at the granularity of machine
-- instructions. These should be semantically meaningful divisions to
-- the programmer. I.E. a single subroutine in the original language
-- or something of that ilk.
--
-- These divisions are useful because it's hard to reason about the
-- program from the pcode at a level higher than this. If we take
-- these are C-esque functions, then they all have variable entry
-- points, and they return to a variable location from the stack. That
-- value is runtime dependent, and while we could draw up a list of
-- possibilities for many programs (first order programs if I had to
-- catagorize off the cuff :thinking:), these are divisions about
-- which there is principled and thus restrained operations. Putting
-- aside some optimization, this is a location that generally conforms
-- to a calling convention and thus we can easily divide along these
-- lines.
--
-- TODO we should check that our assumptions hold. I.E. things like
-- not jumping between different functions except with calls and
-- returns. How isolated are these? How strongly can we reason, and
-- how much semantically meaningful info can we recover? We don't want
-- to rehash Ghidra, so meaning recovery isn't really the focus.

data FuncBlock = FuncBlock String [PBlock]

-- TODO do we want this to match the dump like others?
instance Show FuncBlock where
  show (FuncBlock name blocks) =
    name ++ "\n" ++ (concat $ map ((++"\n") . show) blocks)

-- Big blocks, use Data.Text
--
-- TODO this produces unanalyzed funcblocks. Make that a type difference?
fromPrintedFuncBlock :: Text -> Maybe FuncBlock
fromPrintedFuncBlock text =
  let lines = splitOn (pack "\n") text
      mname = stripPrefix "FUNCTION\t\t" $ unpack $ head lines
      body = tail lines
      listmPInst = map (fromPrintedPInst . unpack) body
      listmaybetomaybelist [] = Just []
      listmaybetomaybelist (x:xs) = case x of
        Nothing -> Nothing
        Just a -> case listmaybetomaybelist xs of
          Nothing -> Nothing
          Just as -> Just (a:as)
      -- This function is by ChatGPT. Looks like there ought to be a
      -- monad solution, but I can't find one.
  in
    case listmaybetomaybelist listmPInst of
      Nothing -> Nothing
      Just lp -> case mname of
        Nothing -> Nothing
        Just name -> Just $ FuncBlock name $ [PBlock lp]




-- -------------------------------------------------------------------
-- Machine operations.
--
-- Simply groupings of Pcode operations. There can be internal pcode
-- relative control flow, but this is the granularity of machine
-- visible branches. So control flow graphs may not respect these
-- divisions, as a conditional branch that optionally sets things like
-- control registers may have a branch POpt in a non-tail position.
--
-- TODO test that claim
--
-- Unclear how useful this is, but it's an interesting and possibly
-- insightful semantic division.

-- Single Machine instruction. An sequence of PCode
-- Operations. Information like location are not internal.
-- data MOpt = [POpt]
