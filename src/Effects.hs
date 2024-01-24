-- Spiritually an extension of the PCode module that knows about both
-- the semantic meanings of PCode instructions and the structure of
-- the crucible generator. Seperate for readability and avoiding
-- circular deps.

module Effects where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.BitVector.Sized
import qualified Data.Map as Map
import Data.Text (pack)
import GHC.TypeNats

import Lang.Crucible.CFG.Generator
import Lang.Crucible.CFG.Reg
import Lang.Crucible.CFG.Expr
import Lang.Crucible.Types
import What4.ProgramLoc

import PCode
import ConversionTypes
import PreCrucible

instPos :: PInst -> Position
instPos (PInst loc _) =
  OtherPos $ pack $ "(" ++ (show (loc^.maddr)) ++ "," ++ (show (loc^.offset)) ++ ")"

{- | General purpose varnode conversion. This is what you want for
 parsing the input varnodes of data-manipulation instructions. This
 function is not enough to deal with the (possible) data manipulation
 instructions that write to more than one register, as this will
 provide an single large input, but you likely want to do that split
 on the back (write) end anyway. | -}
varToSource :: VarNode -> PCodeGenerator s (Expr () s (BVType 64))
varToSource vn
  | len /= 8 = error "Varnode length in source."
    -- TODO this is way less general than I want
  | arSp == "constant" =
      error "TODO: fix type level constraints for BVLit"
      -- BVLit (knownNat 64) $ mkBV (len * 8) off
  | arSp == "register" =
      let rmap = view regMap $ get
          involvedCRegs = concretify vn --list of regs (ascending)
      in
        return $ foldr1 (flip (BVConcat 64 64)) $ map (readReg . (Map.lookup rmap)) involvedCRegs
        -- TODO there is definitely more that I need to do here, ain't
        -- no way this typechecks
        --
        -- TODO this forces an endianess assumption (currently little
        -- via flip), since we don't know that in the reg space, the
        -- register with lower offset has lower order bits, but that's
        -- the assumption here. Not easily fixed.
  | arSp == "ram" =
      -- TODO all memory acesses are fully arbitrary for now. Consider
      -- using the LLVM memory model
      mkFresh $ FreshConstant (BaseBVRepr 64) Nothing
  | arSp == "temporary" =
      error "temp space currently unsupported"
  | otherwise =
      error $ "unexpected address space: " ++ arSp
  where len = vn^.vnLength
        off = vn^.vnOffset
        arSp = vn^.addrSpace

{- | Takes a varnode and interpretes it as a destination varnode for a
   data manipulation instruction. Returns a function that expects a
   value-level input (what is to be written), and produces a generator
   action that writes said value to the appropriate location according
   to the initial varnode. | -}
varToDestination :: VarNode -> ((Expr () s (BVType 64)) -> PCodeGenerator s ())
-- TODO fixed sized BV for now
varToDestination vn
  | len == 0 =
      error "TODO zero length varnode. Ignorable but suspicious."
  | len > 8 =
      error "TODO multi-register writebacks"
  | len `mod` 8 /= 0 =
      error "TODO sub-register writebacks"
  | arSp == "constant" =
      error "Can't assign to the constant address space"
  | arSp == "register" =
      -- NOTE this is a full register writeback, see guards
      let rmap = view regMap $ get
          involvedCRegs = concretify vn --list of regs (ascending)
          -- NOTE see above, we only care about the first element
      in
        assignReg ((Map.!) rmap (head involvedCRegs))

  | arSp == "ram" =
      -- TODO see varToSource, ignore writes
      const (return ())
  | arSp == "temporary" =
      error "temp space currently unsupported"
  | otherwise =
      error $ "unexpected address space: " ++ arSp
  where len = vn^.vnLength
        off = vn^.vnOffset
        arSp = vn^.addrSpace


-- TODO what should the inner type here be? Most instructions don't
-- return anything, but block-terminating ones return and EndState,
-- which I think I want to feed into the SSA conversion, but idk.
-- singleInstructionAction :: PInst -> PCodeGenerator s a
-- singleInstructionAction inst
--   | terminalP inst =
--     -- TODO pick block terminating type
--     a
--   | floatP inst =
--     error "TODO add float support"
--   | otherwise =


{- | Intentionally partial over operations. Builds a generator action
   that captures the semantic operation of the arithmetic instruction
   | -}
arithAction :: POpt -> PCodeGenerator s ()
arithAction (COPY va out) =
  varToSource va >>= (varToDestination out)
-- arithAction (INT_ADD va vb out) =
-- arithAction (BOOL_OR va vb out) =
-- arithAction (INT_SUB va vb out) =
-- arithAction (INT_CARRY va vb out) =
-- arithAction (INT_SCARRY va vb out) =
-- arithAction (INT_SBORROW va vb out) =
-- arithAction (INT_2COMP va out) =
-- arithAction (INT_NEGATE va out) =
-- arithAction (INT_XOR va vb out) =
-- arithAction (INT_AND va vb out) =
-- arithAction (INT_OR va vb out) =
-- arithAction (PIECE va vb out) =
-- arithAction (INT_LEFT va vb out) =
-- arithAction (SUBPIECE va vb out) =
-- arithAction (INT_RIGHT va vb out) =
-- arithAction (INT_EQUAL va vb out) =
-- arithAction (INT_SRIGHT va vb out) =
-- arithAction (INT_NOTEQUAL va vb out) =
-- arithAction (INT_MULT va vb out) =
-- arithAction (INT_LESS va vb out) =
-- arithAction (INT_DIV va vb out) =
-- arithAction (INT_SLESS va vb out) =
-- arithAction (INT_REM va vb out) =
-- arithAction (INT_LESSEQUAL va vb out) =
-- arithAction (INT_SDIV va vb out) =
-- arithAction (INT_SLESSEQUAL va vb out) =
-- arithAction (INT_SREM va vb out) =
-- arithAction (INT_ZEXT va out) =
-- arithAction (BOOL_NEGATE va out) =
-- arithAction (TRUNC va out) =
-- arithAction (INT_SEXT va out) =
-- arithAction (BOOL_XOR va vb out) =
-- arithAction (BOOL_AND va vb out) =
-- arithAction (POPCOUNT va out) =
