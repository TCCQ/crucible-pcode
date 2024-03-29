-- Spiritually an extension of the PCode module that knows about both
-- the semantic meanings of PCode instructions and the structure of
-- the crucible generator. Seperate for readability and avoiding
-- circular deps.

module Effects where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Function (fix)
import Data.BitVector.Sized
import qualified Data.Map as Map
import Data.Text (pack)

import GHC.TypeNats
import Data.Kind
import Data.Parameterized.NatRepr
import Data.Parameterized.Some
import Data.Parameterized.Classes (polyEqF)
import Data.Type.Equality (testEquality, (:~:)(..))

import Lang.Crucible.CFG.Generator
import Lang.Crucible.CFG.Reg as Reg
import Lang.Crucible.CFG.Expr
import Lang.Crucible.Types
import What4.ProgramLoc

import PCode
import Analysis
import ConversionTypes
import PreCrucible

instance MonadFail (Either String) where
  fail = Left

instPos :: PInst -> Position
instPos (PInst loc _) =
  OtherPos $ pack $ "(" ++ (show (loc^.maddr)) ++ "," ++ (show (loc^.offset)) ++ ")"

mkSomePos :: Integer -> Maybe SomePos
mkSomePos x = do
  Some xRepr <- someNat x
  LeqProof <- isPosNat xRepr
  return (SomePos xRepr)

appToDest :: forall s. Some (BVApp s) -> Some (BVDest s) -> PCodeGenerator s ()
appToDest (Some outVal@(BVApp outW _outExpr)) (Some (BVDest inW inLoc)) =
  case testEquality outW inW of
    Just Refl -> inLoc outVal
    Nothing -> lift $ Left "Widths didn't match between value and destination."

-- TODO Make this an extension app.

-- bvPaste :: (1 <= width, 1 <= len, idx + len <= width)
--         => (NatRepr idx) -- starting here
--         -> (NatRepr len)
--         -> (NatRepr width)
--         -> (BVApp len) -- pasting this
--         -> (BVApp width)   -- into this
--         -> BVApp width

-- -- bvPaste x l w new (BVApp old) =
--   let
--     alignedNew = BVRol w (BVZext w l new) (IntegerToBV w (intValue x))
--     oldLowMask = case isPosNat x of
--       Just LeqProof -> BVZext w x (BVTrunc x w old)
--       Nothing ->
--         IntegerToBV w 0
--     oldHighLen = subNat w (addNat x l)
--     oldHighOffset = (IntegerToBV w ((intValue x) + (intValue l)))
--     oldHigh =
--       (BVZext w oldHighLen) . (BVTrunc oldHighLen w) (BVRor w old (IntegerToBV w oldHighOffset))
--     oldHighMask =
--       BVRol w oldHigh oldHighOffset
--     oldMask = BVOr w oldHighMask oldLowMask
--   in
--     BVOr w oldMask alignedNew

{- | General purpose varnode conversion. This is what you want for
 parsing the input varnodes of data-manipulation instructions. This
 function is not enough to deal with the (possible) data manipulation
 instructions that write to more than one register, as this will
 provide an single large input, but you likely want to do that split
 on the back (write) end anyway. | -}
varToSource :: forall s. VarNode -> PCodeGenerator s (Some (BVApp s))
varToSource vn
  -- len /= 8 = error "Varnode length in source."
    -- TODO this is way less general than I want
  | arSp == "ram" =
      -- TODO all memory acesses are fully arbitrary for now. Consider
      -- using the LLVM memory model
      error ""
      -- liftM AtomExpr $ mkFresh (BaseBVRepr (knownNat @64)) Nothing
  | arSp == "temporary" =
      error "temp space currently unsupported"
  | arSp == "constant" =
      error "TODO: fix type level constraints for BVLit"
      -- BVLit (knownNat 64) $ mkBV (len * 8) off
  | arSp == "register" =
    do
      rmap <- get >>= return . (view regMap)
      headCReg:[] <- return $ concretify vn
      -- TODO this uses MonadFail for all ways this can fail
      reg <- return $ (Map.!) rmap headCReg

      case (do
               -- exactly one register, but we still need to isolate the bits we care about
               Some idxE <- someNat off
               SomePos lenE <- mkSomePos len
               SomePos width <- mkSomePos 64
               Refl <- testEquality width (knownNat @64)
               LeqProof <- testLeq (addNat idxE lenE) width
               return $ do
                 regBV <- readReg reg
                 slice <- return $ Reg.App $ BVSelect idxE lenE width regBV
                 wrapped <- return $ BVApp lenE slice
                 return $ Some wrapped
           ) of
        Just ret -> ret
        Nothing -> lift $ Left $ "Error in varToSource. Do you have a well formed varnode? : " ++ (show vn)
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
varToDestination :: forall s. VarNode -> PCodeGenerator s (Some (BVDest s))
varToDestination vn
  -- len /= 8 = error "Varnode length in source."
    -- TODO this is way less general than I want
  | arSp == "ram" =
      -- TODO all memory acesses are fully arbitrary for now. Consider
      -- using the LLVM memory model
      error ""
      -- liftM AtomExpr $ mkFresh (BaseBVRepr (knownNat @64)) Nothing
  | arSp == "temporary" =
      error "temp space currently unsupported"
  | arSp == "constant" =
      error "Constant Address Space as destination of P-Code operation"
  | arSp == "register" =
      do
        rmap <- get >>= return . (view regMap)
        headCReg:[] <- return $ concretify vn
        -- TODO this uses MonadFail for all ways this can fail
        reg <- return $ (Map.!) rmap headCReg
        case (do
                 -- exactly one register, but we still need to isolate the bits we care about
                 Some idxE <- someNat off
                 SomePos lenE <- mkSomePos len
                 SomePos width <- mkSomePos 64
                 Refl <- testEquality width (knownNat @64)
                 LeqProof <- testLeq (addNat idxE lenE) width

                 -- We need to construct the existential inside this
                 -- do, because ghc under-generalizes its type
                 -- otherwise.
                 return $ Some $ BVDest lenE (\(BVApp inW inner) -> do
                                          Refl <- lift $ case (testEquality inW lenE) of
                                            Just Refl -> Right Refl
                                            Nothing -> Left "Width didn't match between BVDest and BVApp"
                                          inExpr <- return inner
                                          -- nval <- error "" -- TODO (readReg reg) >>= (return . (BVPaste idxE lenE width inExpr))
                                          -- assignReg reg nval --TODO need wrapping to get a eval?
                                          error ""
                                      )
             ) of
          Just ret -> return $ ret
          Nothing -> lift $ Left $ "Error during varToDest. Do you have a well formed varnode? :" ++ (show vn)
  | otherwise =
      error $ "unexpected address space: " ++ arSp
  where len = vn^.vnLength
        off = vn^.vnOffset
        arSp = vn^.addrSpace

{- | Intentionally partial over operations. Builds a generator action
   that captures the semantic operation of the arithmetic instruction
   | -}
arithAction :: POpt -> PCodeGenerator s ()
arithAction (COPY va out) =
  join $ appToDest <$> (varToSource va) <*> (varToDestination out)
arithAction (INT_ADD va vb out) =
  join $ appToDest <$> (do
                           a@(Some(BVApp awidth aexpr)) <- varToSource va
                           b@(Some(BVApp bwidth bexpr)) <- varToSource vb
                           Just Refl <- return $ testEquality awidth bwidth
                           Just LeqProof <- return $ isPosNat awidth
                           oexpr <- return $ Reg.App $ BVAdd awidth aexpr bexpr
                           return $ Some $ BVApp awidth oexpr
                       ) <*> (varToDestination out)
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
arithAction (LOAD addrSpace offset dest) = do
  Some (BVDest width dFunc) <- varToDestination dest
  Just LeqProof <- return $ isPosNat width
  unconstrained <- mkFresh (BaseBVRepr width) Nothing
  ucApp <- return $ BVApp width (AtomExpr unconstrained)
  dFunc ucApp
arithAction (STORE addrSpace offset src) =
  return ()


{- | Similar to above, but for terminating statements.

TODO how do tail calls fit into this? I don't model them because I
don't model calling convention stack stuff, so I don't think I can
actually catch when they happen. I guess they aren't supported. But
they aren't in C so it should be fine. | -}
termAction :: Successor -> PCodeGenerator s a
termAction succ =
  case succ of
      Fallthrough nextId -> do
        blockLabelMap <- liftM (view labelMap) get
        nextLabel <- return $ (Map.!) blockLabelMap nextId
        jump nextLabel

      UncondTarget targetId -> do
        blockLabelMap <- liftM (view labelMap) get
        targetLabel <- return $ (Map.!) blockLabelMap targetId
        jump targetLabel

      CondTarget flagV targetId nextId -> do
        blockLabelMap <- liftM (view labelMap) get
        targetLabel <- return $ (Map.!) blockLabelMap targetId
        nextLabel <- return $ (Map.!) blockLabelMap nextId
        flag <- do
            Some (BVApp width val) <- varToSource flagV
            Just LeqProof <- return $ isPosNat width
            capp <- return $ BVNonzero width val
            return $ Reg.App capp
        branch flag targetLabel nextLabel

      ExternReturn -> do
        regMap <- liftM (view regMap) get
        rSet <- error "TODO convert into RegisterSet"
        returnFromFunction rSet

blockAction :: Reg.Label s -> ACFGBlock -> PCodeGenerator s ()
blockAction blockLabel block =
  defineBlock blockLabel
  (do
      instStream <- return $ init (block^.aBlock^.stream)
                    -- doesn't include terminating inst
      sequence (map (arithAction . (view opt)) instStream)
      (termAction (block^.aSuccs)))

