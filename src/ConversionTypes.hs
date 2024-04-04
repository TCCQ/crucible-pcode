-- Types for converting to the crucible interface. Defined here to
-- avoid circular deps

module ConversionTypes where

import qualified Data.Map as Map
import Control.Lens
import Data.Parameterized.Ctx
import Control.Monad.State

import qualified Lang.Crucible.FunctionHandle as C.FuncH
import qualified Lang.Crucible.CFG.Generator as C.Gen
import qualified Lang.Crucible.CFG.Reg as C.Reg
import qualified Lang.Crucible.CFG.Expr as C.Expr
import Lang.Crucible.Types

import PCode

-- NOTE:
-- hardcoded assumptions:
-- A register is exactly 8 bytes


-- Positive compiletime restriction
data SomePos = forall n. 1 <= n => SomePos (NatRepr n)

-- Bitvector symoblic value. suitable to put in a Some
data BVApp s w = BVApp (NatRepr w) (C.Reg.Expr () s (BVType w))


{- | The handle for a register used by crucible. Local to the CFG. This
   should be enough info for you to find the register of internal type
   `CReg`. See RegMap and generator state. | -}
type CMangagedRegHandle s = C.Reg.Reg s (BVType 64)

{- | A concrete register for us is an 8byte register that is disjoint
   with all other concrete registers. They are indexed from zero, and
   correspond with the varnode register space such that a byte at
   address `a` will lie in register `a / 8`. This is implemented as a
   new type so that we can't do integer operations accidentally, and
   to allow for adding new fields later. | -}
newtype ConcreteReg = ConcreteReg {
  _rid :: Integer
  } deriving (Eq, Ord)
makeLenses ''ConcreteReg

-- My internal IR reg to crucible's reg object. This is CFG local.
type RegMap s = Map.Map ConcreteReg (CMangagedRegHandle s)

type PRegAssoc = StructType ((SingleCtx NatType) ::> (BVType 64))
type PRegBundle = SequenceType PRegAssoc

{- | All the information we need to pass between functions on call /
   return. Full register state, etc. | -}
type FuncStateBundle = PRegBundle

-- We want to use my reg id scheme instead of crucible's since we need
-- to distinguish registers across CFGs.

-- Every function has the same signature: all regs in, all regs
-- out. It's wasteful, but it's uniform.
type CPFunction = C.FuncH.FnHandle (SingleCtx FuncStateBundle) FuncStateBundle
type FunctionLocationMap = Map.Map PAddr CPFunction

-- Specifically takes block ids (local to the current CFG), to the
-- crucible label for that block (regardless of if the block has been
-- generated). This needs to be pre-computed at the top of each CFG,
-- since we have forward and back jumps.
type LabelMap s = Map.Map Integer (C.Reg.Label s)

newtype OffsetLenPair = OffsetLenPair (Integer, Integer) deriving (Eq, Ord)

-- Store the contents of the the "unique" address space. Should be
-- single write and must match on both offset and length. 100% CFG
-- local, and in fact machine instruction local possibly?
type TemporaryMap s = Map.Map OffsetLenPair (Some (BVApp s))

-- The appropriate type for the state of the crucible generator
data CGenState s = CGenState {
  _funcLocMap :: FunctionLocationMap,
  _labelMap :: LabelMap s,
  _regMap :: RegMap s,
  _tmpMap :: TemporaryMap s
  }
makeLenses ''CGenState

-- type alias for the generator. You want to use this everywhere. I'm
-- guessing that the list monad is what we want, since some pcode
-- statements may become more than one top-level action. Subject to
-- change.
type PCodeGenerator s a = C.Gen.Generator () s (CGenState) FuncStateBundle (Either String) a

-- A functional that expects a properly sized bitvector value and
-- produces some generator action. This is used to represent whatever
-- sort of write/store we need for the bitvector.
data BVDest s w = BVDest (NatRepr w) ((BVApp s w) -> PCodeGenerator s ())
