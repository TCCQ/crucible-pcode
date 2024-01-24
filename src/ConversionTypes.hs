-- Types for converting to the crucible interface. Defined here to
-- avoid circular deps

module ConversionTypes where

import qualified Data.Map as Map
import Control.Lens
import Data.Parameterized.Ctx

import qualified Lang.Crucible.FunctionHandle as C.FuncH
import qualified Lang.Crucible.CFG.Generator as C.Gen
import qualified Lang.Crucible.CFG.Reg as C.Reg
import Lang.Crucible.Types

import PCode
import PreCrucible


-- NOTE:
--
-- hardcoded assumptions:
--
-- A register is exactly 8 bytes
-- There are no more than 256 registers
-- (there are *exactly*, but you don't have to use them)

{- | Register Set. The full register machine state.| -}
type RegisterSet =
  SymbolicStructType (EmptyCtx ::> (BaseBVType 64))

{- | The handle for a register used by crucible. Local to the CFG. This
   should be enough info for you to find the register of internal type
   `CReg`. See RegMap and generator state. | -}
type CMangagedRegHandle s = C.Reg.Reg s (BVType 64)

-- We want to use my reg id scheme instead of crucible's since we need
-- to distinguish registers across CFGs.

-- Every function has the same signature: all regs in, all regs
-- out. It's wasteful, but it's uniform.
type FunctionLocationMap = Map.Map PAddr (C.FuncH.FnHandle (SingleCtx RegisterSet) RegisterSet)

-- Specifically takes block ids (local to the current CFG), to the
-- crucible label for that block (regardless of if the block has been
-- generated). This needs to be pre-computed at the top of each CFG,
-- since we have forward and back jumps.
type LabelMap s = Map.Map Integer (C.Reg.Label s)

-- My internal IR reg to crucible's reg object. This is CFG local.
type RegMap s = Map.Map ConcreteReg (CMangagedRegHandle s)

-- The appropriate type for the state of the crucible generator
data CGenState s = CGenState {
  _funcLocMap :: FunctionLocationMap,
  _labelMap :: LabelMap s,
  _regMap :: RegMap s
  }
makeLenses ''CGenState

-- type alias for the generator. You want to use this everywhere. I'm
-- guessing that the list monad is what we want, since some pcode
-- statements may become more than one top-level action. Subject to
-- change.
type PCodeGenerator s a = C.Gen.Generator () s (CGenState) RegisterSet [] a
-- TODO it wants state to be a kind `* -> *` thing, but I don't plan on changing types, so I guess the `s` type will do that for me?

