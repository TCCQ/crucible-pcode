
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
  fmap Map.fromList $ sequence $ fmap blockToIdLabelPair (acfg^.aBlockList)
  where blockToIdLabelPair :: ACFGBlock -> PCodeGenerator s ((Integer, C.Reg.Label s))
        blockToIdLabelPair block = do
          l <- newLabel
          return (block^.aBlock^.blockId, l)

-- TODO right signature here?
{- | Make all the registers that the cfg generator expects to be able to
   access. | -}
mkRegMap :: Assignment (Atom s) (SingleCtx FuncStateBundle) -> PCodeGenerator s (RegMap s)
mkRegMap initials =
  error "TODO mkRegMap"

{- | Produce the generator action internal to this cfg. Basically do the
   block actions for all the blocks. | -}
intoGenerator :: FunctionLocationMap -> AugCFG -> PCodeGenerator s ()
intoGenerator funcMap acfg = do
  lmap <- mkLabelMap acfg
  rmap <- error "TODO" --mkRegMap acfg
  tmap <- return Map.empty

  actionList <- mapM (\block ->
                       blockAction ((Map.!) lmap (block^.aBlock^.blockId)) block
                       ) (acfg^.aBlockList)
  -- TODO this seems odd in terms of types, but I think still
  -- works. The info I care about is in the monad not in the value. We
  -- just have to use this monad when producing a CFG for this
  -- function
  return ()
-- TODO use langston's help, get proper function level return type

{- | Change our generator action into a Function Definition by supplying
   initial state and spinning up the default register contents and
   whatnot. | -}
-- mkFunctionDef :: FunctionLocationMap -> AugCFG ->
--                  FunctionDef () CGenState (SingleCtx FuncStateBundle) FuncStateBundle (Either String)
-- mkFunctionDef fLocMap acfg =
--   (\ initialAssignment ->
--       -- since the state has cfg local stuff, we can only make it
--       -- inside the generator, so we have a fake initial state, and
--       -- our actions starts by filling it in
--      ((CGenState fLocMap Map.empty Map.empty),
--       (
--          do
--            state <- get
--            lmap <- mkLabelMap acfg
--            rmap <- mkRegMap acfg
--            -- TODO use intialAssignment to fill the RegMap
--            put $ ((set labelMap lmap) . (set regMap rmap)) state
--            -- Now our initial state is set up
--            intoGenerator fLocMap acfg
--       )))

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

-- -- TODO is this the right place to stop caring about
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
