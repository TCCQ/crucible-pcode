-- This file should have types that will be of general use

import Numeric (showHex)
import Data.Text (Text, pack, empty)
import Data.Text.Read (decimal, hexadecimal)
import Data.Either (fromRight)
import Debug.Trace (trace)

-- (Address Space Id, offset (signed for constants), Length/Size)
-- Length should always be >= 0 I think
data VarNode = VarNode String Integer Integer

-- getters
addressSpace :: VarNode -> String
addressSpace (VarNode as _ _) = as

offset :: VarNode -> Integer
offset (VarNode _ off _) = off

length :: VarNode -> Integer
length (VarNode _ _ len) = len

-- Expects a parenthesized string representation like is produced by
-- the dumping script or the `vnShow` bellow
vnFromPrinted :: String -> Maybe VarNode
vnFromPrinted str =
  case break (==',') (tail (init str)) of
    (addr, ',':' ':rest) ->
      case break (==',') rest of
        (offstr, ',':' ':lenstr) ->
          -- only get here if it decomposes nicely
          let off = fromRight (-1, empty) (hexadecimal (pack offstr))
              len = fromRight (-1, empty) (decimal (pack lenstr))
          in
            case (fst off, fst len) of
              (-1, _) -> Nothing
              (_, -1) -> Nothing
              (o, l) -> Just $ VarNode addr o l
        otherwise -> Nothing
    otherwise -> Nothing

-- Should give matching results as the format given by the dumping
-- script
instance Show (VarNode) where
  show (VarNode as off len) =
    "(" ++ as ++ ", 0x" ++ showHex off ", " ++ (show len) ++ ")"

-- -------------------------------------------------------------------
-- Pcode operations.
--
-- In general the order is the inputs in order and the output if there
-- is one.
--
-- See the reference, the formal semantics, or my writeup for what
-- they do.


-- These Shouldn't appear in raw pcode
-- data PO_CPOOLREF = -- variadic
-- data PO_NEW = -- variadic
-- data PO_USERDEFINED = -- variadic

-- Type for a single Pcode instruction.
data POpt =
  PO_COPY VarNode VarNode |
  PO_INT_ADD VarNode VarNode VarNode |
  PO_BOOL_OR VarNode VarNode VarNode |
  PO_LOAD VarNode VarNode VarNode |
  PO_INT_SUB VarNode VarNode VarNode |
  PO_FLOAT_EQUAL VarNode VarNode VarNode |
  PO_STORE VarNode VarNode VarNode |
  PO_INT_CARRY VarNode VarNode VarNode |
  PO_FLOAT_NOTEQUAL VarNode VarNode VarNode |
  PO_BRANCH VarNode | -- See the reference for interpretation
  PO_INT_SCARRY VarNode VarNode VarNode |
  PO_FLOAT_LESS VarNode VarNode VarNode |
  PO_CBRANCH VarNode VarNode |
  PO_INT_SBORROW VarNode VarNode VarNode |
  PO_FLOAT_LESSEQUAL VarNode VarNode VarNode |
  PO_BRANCHIND VarNode |
  PO_INT_2COMP VarNode VarNode |
  PO_FLOAT_ADD VarNode VarNode VarNode |
  PO_CALL VarNode | -- In raw Pcode only ever one input
  PO_INT_NEGATE VarNode VarNode |
  PO_FLOAT_SUB VarNode VarNode VarNode |
  PO_CALLIND VarNode |
  PO_INT_XOR VarNode VarNode VarNode |
  PO_FLOAT_MULT VarNode VarNode VarNode |
  PO_INT_AND VarNode VarNode VarNode |
  PO_FLOAT_DIV VarNode VarNode VarNode |
  PO_RETURN VarNode | -- In raw Pcode only ever one input
  PO_INT_OR VarNode VarNode VarNode |
  PO_FLOAT_NEG VarNode VarNode |
  PO_PIECE VarNode VarNode VarNode |
  PO_INT_LEFT VarNode VarNode VarNode |
  PO_FLOAT_ABS VarNode VarNode |
  PO_SUBPIECE VarNode VarNode VarNode | -- Second input should be constant
  PO_INT_RIGHT VarNode VarNode VarNode |
  PO_FLOAT_SQRT VarNode VarNode |
  PO_INT_EQUAL VarNode VarNode VarNode |
  PO_INT_SRIGHT VarNode VarNode VarNode |
  PO_FLOAT_CEIL VarNode VarNode |
  PO_INT_NOTEQUAL VarNode VarNode VarNode |
  PO_INT_MULT VarNode VarNode VarNode |
  PO_FLOAT_FLOOR VarNode VarNode |
  PO_INT_LESS VarNode VarNode VarNode |
  PO_INT_DIV VarNode VarNode VarNode |
  PO_FLOAT_ROUND VarNode VarNode |
  PO_INT_SLESS VarNode VarNode VarNode |
  PO_INT_REM VarNode VarNode VarNode |
  PO_FLOAT_NAN VarNode VarNode |
  PO_INT_LESSEQUAL VarNode VarNode VarNode |
  PO_INT_SDIV VarNode VarNode VarNode |
  PO_INT2FLOAT VarNode VarNode |
  PO_INT_SLESSEQUAL VarNode VarNode VarNode |
  PO_INT_SREM VarNode VarNode VarNode |
  PO_FLOAT2FLOAT VarNode VarNode |
  PO_INT_ZEXT VarNode VarNode |
  PO_BOOL_NEGATE VarNode VarNode |
  PO_TRUNC VarNode VarNode |
  PO_INT_SEXT VarNode VarNode |
  PO_BOOL_XOR VarNode VarNode VarNode |
  PO_BOOL_AND VarNode VarNode VarNode
-- PO_POPCOUNT ? TODO

-- Expects the format given by dumping script
-- POptFromPrinted :: String -> Maybe POpt

-- Prints a representation that should match the above and the dumping
-- script

instance Show (POpt) where
  show (PO_COPY a b) = "COPY " ++ show a ++ " " ++ show b
  show (PO_INT_ADD a b c) = "INT_ADD " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_BOOL_OR a b c) = "BOOL_OR " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_LOAD a b c) = "LOAD " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_SUB a b c) = "INT_SUB " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_EQUAL a b c) = "FLOAT_EQUAL " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_STORE a b c) = "STORE " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_CARRY a b c) = "INT_CARRY " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_NOTEQUAL a b c) = "FLOAT_NOTEQUAL " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_BRANCH a) = "BRANCH " ++ show a
  show (PO_INT_SCARRY a b c) = "INT_SCARRY " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_LESS a b c) = "FLOAT_LESS " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_CBRANCH a b) = "CBRANCH " ++ show a ++ " " ++ show b
  show (PO_INT_SBORROW a b c) = "INT_SBORROW " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_LESSEQUAL a b c) = "FLOAT_LESSEQUAL " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_BRANCHIND a) = "BRANCHIND " ++ show a
  show (PO_INT_2COMP a b) = "INT_2COMP " ++ show a ++ " " ++ show b
  show (PO_FLOAT_ADD a b c) = "FLOAT_ADD " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_CALL a) = "CALL " ++ show a
  show (PO_INT_NEGATE a b) = "INT_NEGATE " ++ show a ++ " " ++ show b
  show (PO_FLOAT_SUB a b c) = "FLOAT_SUB " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_CALLIND a) = "CALLIND " ++ show a
  show (PO_INT_XOR a b c) = "INT_XOR " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_MULT a b c) = "FLOAT_MULT " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_AND a b c) = "INT_AND " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_DIV a b c) = "FLOAT_DIV " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_RETURN a) = "RETURN " ++ show a
  show (PO_INT_OR a b c) = "INT_OR " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_NEG a b) = "FLOAT_NEG " ++ show a ++ " " ++ show b
  show (PO_PIECE a b c) = "PIECE " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_LEFT a b c) = "INT_LEFT " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_ABS a b) = "FLOAT_ABS " ++ show a ++ " " ++ show b
  show (PO_SUBPIECE a b c) = "SUBPIECE " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_RIGHT a b c) = "INT_RIGHT " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_SQRT a b) = "FLOAT_SQRT " ++ show a ++ " " ++ show b
  show (PO_INT_EQUAL a b c) = "INT_EQUAL " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_SRIGHT a b c) = "INT_SRIGHT " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_CEIL a b) = "FLOAT_CEIL " ++ show a ++ " " ++ show b
  show (PO_INT_NOTEQUAL a b c) = "INT_NOTEQUAL " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_MULT a b c) = "INT_MULT " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_FLOOR a b) = "FLOAT_FLOOR " ++ show a ++ " " ++ show b
  show (PO_INT_LESS a b c) = "INT_LESS " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_DIV a b c) = "INT_DIV " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_ROUND a b) = "FLOAT_ROUND " ++ show a ++ " " ++ show b
  show (PO_INT_SLESS a b c) = "INT_SLESS " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_REM a b c) = "INT_REM " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT_NAN a b) = "FLOAT_NAN " ++ show a ++ " " ++ show b
  show (PO_INT_LESSEQUAL a b c) = "INT_LESSEQUAL " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_SDIV a b c) = "INT_SDIV " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT2FLOAT a b) = "INT2FLOAT " ++ show a ++ " " ++ show b
  show (PO_INT_SLESSEQUAL a b c) = "INT_SLESSEQUAL " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_INT_SREM a b c) = "INT_SREM " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_FLOAT2FLOAT a b) = "FLOAT2FLOAT " ++ show a ++ " " ++ show b
  show (PO_INT_ZEXT a b) = "INT_ZEXT " ++ show a ++ " " ++ show b
  show (PO_BOOL_NEGATE a b) = "BOOL_NEGATE " ++ show a ++ " " ++ show b
  show (PO_TRUNC a b) = "TRUNC " ++ show a ++ " " ++ show b
  show (PO_INT_SEXT a b) = "INT_SEXT " ++ show a ++ " " ++ show b
  show (PO_BOOL_XOR a b c) = "BOOL_XOR " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (PO_BOOL_AND a b c) = "BOOL_AND " ++ show a ++ " " ++ show b ++ " " ++ show c



-- A Machine address, a Pcode operation and the index of said
-- operation inside the machine instruction. This encoding allows for
-- operation on sequences of this type without data loss.
data PInst = PInst Integer POpt Integer

-- Formated like the dump
-- PInstFromPrinted :: String -> Maybe POpt

-- Prints a representation that should match the above and the dumping
-- script
instance Show (PInst) where
  show (PInst mpc p off) =
    showHex mpc $ ":" ++ showHex off (" " ++ show p)

-- TODO these two following sections are not clearly useful, so they
-- will remain commented for now

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
-- not jumping between different functions accept with calls and
-- returns. How isolated are these? How strongly can we reason, and
-- how much semantically meaningful info can we recover? We don't want
-- to rehash Ghidra, so meaning recovery isn't really the focus.



-- -------------------------------------------------------------------
--
-- Blocks are sequences of Pcode that we have reason to believe are
-- atomic with respect to control flow.
--
-- Note that full confidence requires analysis of indirect branches

-- Atomic block with respect to control flow.
data PBlock = PBlock [PInst]

-- TODO do we need a fromPrinted here?

-- PBlockShow :: PBlock -> String
