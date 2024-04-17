
-- This module contains the last few steps before we feed our IR into
-- crucible. Particularly this covers the space between an AugCFG and
-- the crucible generator interface (and friends).

module PreCrucible where

import Data.List
import qualified Data.Set as Set
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Text (pack)

import PCode
import Analysis
import ConversionTypes
import Effects

import Data.Parameterized.Ctx
import Data.Parameterized.Context hiding (view)
import Lang.Crucible.FunctionHandle
import What4.FunctionName
import qualified Lang.Crucible.CFG.Reg as C.Reg
import Lang.Crucible.CFG.Generator

{- | Make the map from the address of the block head to the function
   handle for each the function. Should be used a single time for the
   full list of functions. | -}
mkFlMap :: [(String, AugCFG)] -> IO FunctionLocationMap
mkFlMap plist = do
  halloc <- newHandleAllocator
  foldM (\ !priorMap (name, acfg) ->
                     Map.insert (startLoc acfg) <$> (single halloc name) <*> (return priorMap)) Map.empty plist
  where single :: HandleAllocator -> String -> IO CPFunction
        single ha name = do
          fname <- return $ (functionNameFromText . pack) name
          mkHandle ha fname
        startLoc aug =
          ((view location) . head) $ (view (aBlock . stream)) $ head (aug^.aBlockList)

{- | Make the map of locations blockids to generator labels for a given
   cfg. | -}
mkLabelMap :: AugCFG -> PCodeGenerator s (LabelMap s)
mkLabelMap acfg =
  (liftM Map.fromList) $ sequence $ fmap blockToIdLabelPair (acfg^.aBlockList)
  where blockToIdLabelPair :: ACFGBlock -> PCodeGenerator s ((Integer, C.Reg.Label s))
        blockToIdLabelPair block = do
          l <- newLabel
          return (block^.aBlock^.blockId, l)

type PRegAssocRepr = StructRepr ((SingleCtx NatType) ::> (BVType 64))

{- | Make all the registers that the cfg generator expects to be able to
   access. | -}
mkRegMap :: Assignment (Atom s) (SingleCtx FuncStateBundle) -> PCodeGenerator s (RegMap s)
mkRegMap initials =
  case (decompose initials) of
    (Empty, fsb) -> do
      -- fsb is atom of a sequence
      foldAssocM fsb
  where singleReg :: (Atom s PRegAssoc) -> PCodeGenerator s ((ConcreteReg, CMangagedRegHandle s))
        singleReg assoc = do
          -- decompose pair into offset index and value
          pairSize <- return $ Size ((SingleCtx NatType) ::> (BVType 64))
          Just (Some offsetI) <- intIndex 0 pairSize
          Just (Some valI) <- intIndex 1 pairSize
          offset <- return $ GetStruct assoc offsetI NatRepr
          val <- return $ GetStruct assoc valI (BVRepr 64)
          creg <- return $ ConcreteReg offset
          crucibleRegisterHandle <- newReg val
          return (creg, crucibleRegisterHandle)
        foldAssocM :: (Atom s PRegBundle) -> PCodeGenerator s (RegMap s)
        foldAssocM aseq = do
          done <- return $ SequenceIsNil PRegAssocRepr aseq
          ifteM (done)
            (return Map.Empty)
            (do
                h <- return $ SequenceHead PRegAssocRepr aseq
                t <- return $ SequenceTail PRegAssocRepr aseq
                rPair <- singleReg h
                liftM ((uncurry Map.insert) rPair) (foldAssocM t))


{- | Produce the generator action internal to this cfg. Basically do the
   block actions for all the blocks. | -}
intoFuncDef :: FunctionLocationMap -> AugCFG -> FunctionDef () CGenState FuncStateBundle FuncStateBundle (Either String)
intoFuncDef funcMap acfg =
  case (unsnoc retBlocks) of
    Nothing -> error "no returns from this function"
    Just (otherExits, finalExit) ->
      let gAction = (mapM singleBlockAction internalBlocks) >>
                       (mapM singleBlockAction otherExits) >>
                       (singleBlockAction finalExit)
      in
        (\ funcArgAssignment ->
           -- since the state has cfg local stuff, we can only make it
           -- inside the generator. So we have a fake initial state
           -- and our actions starts by filling it in
           ((CGenState fLocMap Map.empty Map.empty),
            (
              do
                state <- get
                lmap <- mkLabelMap acfg
                rmap <- mkRegMap acfg --creates and fills the regmap
                put $ ((set labelMap lmap) . (set regMap rmap)) state

                intoGenerator fLocMap acfg
            )))
    -- this is odd, but basically I want each action to be evaluated
    -- once and the last one has to be something that returns the
    -- proper type for the definition
  where isRetBlock block =
          case (last (block^.aBlock^.stream)) of
            _ (RETURN _) -> True
            _ _ -> False
        retBlocks = filter isRetBlock (acfg^.aBlockList)
        internalBlocks = filter (not . isRetBlock) (acfg^.aBlockList)
        singleBlockAction block = do
          labelMap <- get^.labelMap
          blockAction ((Map.!) labelMap (block^.aBlock^.blockId)) block

-- -------------------------------------------------------------------
--
-- Historical stuff that I don't think I need anymore

-- type AMap = Map.Map Integer ACFGBlock

-- {- | Discards program order for access speed | -}
-- acfgToMap :: AugCFG -> AMap
-- acfgToMap acfg =
--   foldl'
--   (\ !(pmap) inBlock ->
--      Map.insert (inBlock^.aBlock^.blockId) inBlock pmap)
--   Map.empty
--   (acfg^.aBlockList)


-- {- | Includes self. | -}
-- allSuccessor :: AMap -> Integer -> [Integer]
-- allSuccessor inmap bid =
--   foldr union [bid] $ map (allSuccessor inmap) $ nextList bid
--   where nextList :: Integer -> [Integer]
--         nextList = (\case { Just l -> l; Nothing -> []}) .
--           intoList .
--           (view aSuccs) .
--           ((Map.!) inmap)

-- -- T ODO is this the right place to stop caring about
-- -- memory/references, and only registers?

-- {- | Collect the varnodes that need to be block arguments. Return a map
--    from a block id to the full set of register varnodes that it should
--    be concerned with as arguments. | -}
-- collectBlockArgs :: AugCFG -> Map.Map Integer [VarNode]
-- collectBlockArgs acfg =
--   foldl' cbaStep Map.empty $ acfg^.aBlockList
--   where blockMap = acfgToMap acfg
--         cbaStep :: Map.Map Integer [VarNode] -> ACFGBlock -> Map.Map Integer [VarNode]
--         cbaStep !priorMap curBlock =
--           let cid = curBlock^.aBlock^.blockId
--               idList = allSuccessor blockMap cid
--               rawVNList = foldl'
--                 (++)                                    -- merge
--                 []                                      -- from empty
--                 (map
--                   ((view aObserves) . ((Map.!) blockMap)) -- the observations
--                   idList)                               -- of self and decedents
--           in
--             Map.insert
--               cid
--               (filter ((== "register") . (view addrSpace)) rawVNList)
--               priorMap


-- {- | Collect the registers for feeding into crucible. Uses a set because
--    we don't want duplicates, and sets support fast operations for
--    things like detecting registers we haven't seen before. | -}
-- concretifyBlockArgs :: [VarNode] -> Set.Set ConcreteReg
-- concretifyBlockArgs vnList =
--   Set.fromList $ concat $ map concretify vnList
