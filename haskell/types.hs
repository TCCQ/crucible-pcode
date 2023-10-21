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
vnShow :: VarNode -> String
vnShow (VarNode as off len) =
  "(" ++ as ++ ", 0x" ++ showHex off ", " ++ show len ++ ")"

-- -------------------------------------------------------------------
-- Pcode operations.
--
-- In general the order is the inputs in order and the output if there
-- is one.
--
-- See the reference, the formal semantics, or my writeup for what
-- they do.

data PO_COPY            = VarNode VarNode
data PO_INT_ADD         = VarNode VarNode VarNode
data PO_BOOL_OR         = VarNode VarNode VarNode
data PO_LOAD            = VarNode VarNode VarNode
data PO_INT_SUB         = VarNode VarNode VarNode
data PO_FLOAT_EQUAL     = VarNode VarNode VarNode
data PO_STORE           = VarNode VarNode VarNode
data PO_INT_CARRY       = VarNode VarNode VarNode
data PO_FLOAT_NOTEQUAL  = VarNode VarNode VarNode
data PO_BRANCH          = VarNode -- See the reference for interpretation
data PO_INT_SCARRY      = VarNode VarNode VarNode
data PO_FLOAT_LESS      = VarNode VarNode VarNode
data PO_CBRANCH         = VarNode VarNode
data PO_INT_SBORROW     = VarNode VarNode VarNode
data PO_FLOAT_LESSEQUAL = VarNode VarNode VarNode
data PO_BRANCHIND       = VarNode
data PO_INT_2COMP       = VarNode VarNode
data PO_FLOAT_ADD       = VarNode VarNode VarNode
data PO_CALL            = VarNode -- In raw Pcode only ever one input
data PO_INT_NEGATE      = VarNode VarNode
data PO_FLOAT_SUB       = VarNode VarNode VarNode
data PO_CALLIND         = VarNode
data PO_INT_XOR         = VarNode VarNode VarNode
data PO_FLOAT_MULT      = VarNode VarNode VarNode
data PO_INT_AND         = VarNode VarNode VarNode VarNode VarNode
data PO_FLOAT_DIV       = VarNode VarNode VarNode
data PO_RETURN          = VarNode -- In raw Pcode only ever one input
data PO_INT_OR          = VarNode VarNode VarNode
data PO_FLOAT_NEG       = VarNode VarNode
data PO_PIECE           = VarNode VarNode VarNode
data PO_INT_LEFT        = VarNode VarNode VarNode
data PO_FLOAT_ABS       = VarNode VarNode
data PO_SUBPIECE        = VarNode VarNode VarNode -- Second input should be constant
data PO_INT_RIGHT       = VarNode VarNode VarNode
data PO_FLOAT_SQRT      = VarNode VarNode
data PO_INT_EQUAL       = VarNode VarNode VarNode
data PO_INT_SRIGHT      = VarNode VarNode VarNode
data PO_FLOAT_CEIL      = VarNode VarNode
data PO_INT_NOTEQUAL    = VarNode VarNode VarNode
data PO_INT_MULT        = VarNode VarNode VarNode
data PO_FLOAT_FLOOR     = VarNode VarNode
data PO_INT_LESS        = VarNode VarNode VarNode
data PO_INT_DIV         = VarNode VarNode VarNode
data PO_FLOAT_ROUND     = VarNode VarNode
data PO_INT_SLESS       = VarNode VarNode VarNode
data PO_INT_REM         = VarNode VarNode VarNode
data PO_FLOAT_NAN       = VarNode VarNode
data PO_INT_LESSEQUAL   = VarNode VarNode VarNode
data PO_INT_SDIV        = VarNode VarNode VarNode
data PO_INT2FLOAT       = VarNode VarNode
data PO_INT_SLESSEQUAL  = VarNode VarNode VarNode
data PO_INT_SREM        = VarNode VarNode VarNode
data PO_FLOAT2FLOAT     = VarNode VarNode
data PO_INT_ZEXT        = VarNode VarNode
data PO_BOOL_NEGATE     = VarNode VarNode
data PO_TRUNC           = VarNode VarNode
data PO_INT_SEXT        = VarNode VarNode
data PO_BOOL_XOR        = VarNode VarNode VarNode
data PO_BOOL_AND        = VarNode VarNode VarNode
-- PO_POPCOUNT ? TODO

-- These Shouldn't appear in raw pcode
-- data PO_CPOOLREF = -- variadic
-- data PO_NEW = -- variadic
-- data PO_USERDEFINED = -- variadic

-- Type for a single Pcode instruction.
data POpt =
  PO_COPY | PO_INT_ADD | PO_BOOL_OR |
  PO_LOAD | PO_INT_SUB | POFLOAT_EQUAL |
  PO_STORE | PO_INT_CARRY | PO_FLOAT_NOTEQUAL |
  PO_BRANCH | PO_INT_SCARRY | PO_FLOAT_LESS |
  PO_CBRANCH | PO_INT_SBORROW | PO_FLOAT_LESSEQUAL |
  PO_BRANCHIND | PO_INT_2COMP | PO_FLOAT_ADD |
  PO_CALL | PO_INT_NEGATE | PO_FLOAT_SUB |
  PO_CALLIND | PO_INT_XOR | PO_FLOAT_MULT |
  PO_INT_AND | PO_FLOAT_DIV | PO_RETURN |
  PO_INT_OR | PO_FLOAT_NEG | PO_PIECE |
  PO_INT_LEFT | PO_FLOAT_ABS | PO_SUBPIECE |
  PO_INT_RIGHT | PO_FLOAT_SQRT | PO_INT_EQUAL |
  PO_INT_SRIGHT | PO_FLOAT_CEIL | PO_INT_NOTEQUAL |
  PO_INT_MULT | PO_FLOAT_FLOOR | PO_INT_LESS |
  PO_INT_DIV | PO_FLOAT_ROUND | PO_INT_SLESS |
  PO_INT_REM | PO_FLOAT_NAN | PO_INT_LESSEQUAL |
  PO_INT_SDIV | PO_INT2FLOAT | PO_INT_SLESSEQUAL |
  PO_INT_SREM | PO_FLOAT2FLOAT | PO_INT_ZEXT |
  PO_BOOL_NEGATE | PO_TRUNC | PO_INT_SEXT |
  PO_BOOL_XOR | PO_BOOL_AND

-- Expects the format given by the
POptFromPrinted :: String -> Maybe POpt

-- Prints a representation that should match the above and the dumping
-- script
POptShow :: POpt -> String

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
data MOpt = [POpt]

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

-- A named sequence of machine instructions associated with their
-- machine address.
data FunctionBlock = String [(Integer, MOpt)]

