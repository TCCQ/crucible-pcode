-- This module contains the last few steps before we feed our IR into
-- crucible. Particularly this covers the space between an AugCFG and
-- the crucible generator interface (and friends).

module PreCrucible where

import Data.List
import qualified Data.Set as Set
import Control.Lens
import qualified Data.Map as Map

import PCode
-- import qualified VarNodeUtils as VN
import Analysis


type AMap = Map.Map Integer ACFGBlock

{- | Discards program order for access speed | -}
acfgToMap :: AugCFG -> AMap
acfgToMap acfg =
  foldl'
  (\ !(pmap) inBlock ->
     Map.insert (inBlock^.aBlock^.blockId) inBlock pmap)
  Map.empty
  (acfg^.aBlockList)


{- | Includes self. | -}
allSuccessor :: AMap -> Integer -> [Integer]
allSuccessor inmap bid =
  foldr union [bid] $ map (allSuccessor inmap) $ nextList bid
  where nextList :: Integer -> [Integer]
        nextList = (\case { Just l -> l; Nothing -> []}) .
          intoList .
          (view aSuccs) .
          ((Map.!) inmap)

-- TODO is this the right place to stop caring about
-- memory/references, and only registers?

{- | Collect the varnodes that need to be block arguments. Return a map
   from a block id to the full set of register varnodes that it should
   be concerned with as arguments. | -}
collectBlockArgs :: AugCFG -> Map.Map Integer [VarNode]
collectBlockArgs acfg =
  foldl' cbaStep Map.empty $ acfg^.aBlockList
  where blockMap = acfgToMap acfg
        cbaStep :: Map.Map Integer [VarNode] -> ACFGBlock -> Map.Map Integer [VarNode]
        cbaStep !priorMap curBlock =
          let cid = curBlock^.aBlock^.blockId
              idList = allSuccessor blockMap cid
              rawVNList = foldl'
                (++)                                    -- merge
                []                                      -- from empty
                (map
                  ((view aObserves) . ((Map.!) blockMap)) -- the observations
                  idList)                               -- of self and decedents
          in
            Map.insert
              cid
              (filter ((== "register") . (view addrSpace)) rawVNList)
              priorMap

{- | A concrete register for us is an 8byte register that is dijoint
   with all other concrete registers. They are indexed from zero, and
   correspond with the varnode register space such that a byte at
   address `a` will lie in register `a / 8`. This is implemented as a
   new type so that we can't do integer operations accidentally, and
   to allow for adding new fields later. | -}
newtype ConcreteReg = ConcreteReg {
  _rid :: Integer
  } deriving (Eq, Ord)
makeLenses ''ConcreteReg

{- | Turn register varnodes into concrete registers. VarNodes can become
   zero or more than one register if they are zero length or length
   greater than 8.| -}
concretify :: VarNode -> [ConcreteReg]
concretify vn
  | vn^.addrSpace /= "register" = error "concretify non-register varnode"
  | otherwise =
    (\v -> if
        | v^.vnLength == 0 -> []
        | v^.vnLength <= 8 -> [(ConcreteReg (v^.vnOffset `div` 8))]
        | otherwise ->
          ((ConcreteReg (v^.vnOffset `div` 8))):(concretify (shorten vn))) vn
  where shorten = (over vnOffset (8+)) . (over vnLength ((-8)+))

{- | Collect the registers for feeding into crucible. Uses a set because
   we don't want duplicates, and sets support fast operations for
   things like detecting registers we haven't seen before. | -}
concretifyBlockArgs :: [VarNode] -> Set.Set ConcreteReg
concretifyBlockArgs vnList =
  Set.fromList $ concat $ map concretify vnList

