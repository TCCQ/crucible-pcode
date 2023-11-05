-- This file should do analysis and manipulation of the PCode as
-- represented by the PCode Module.

import PCode



-- Is this instruction a control flow instruction?
controlFlowP :: PInst -> Bool
controlFlowP (PInst _ popt _) =
          case popt of
            PO_BRANCH _ -> True
            PO_CBRANCH _ _ -> True
            PO_BRANCHIND _ -> True
            PO_CALL _ -> True
            PO_CALLIND _ -> True
            PO_RETURN _ -> True
            otherwise -> False

-- The initial reading in of a function block reads it a single large
-- block. But even without serious analysis we know that's not likely
-- to be realisitic. So we need to subdivide. This is the first pass.
--
-- It's left values are strings explaining things that can go wrong.
controlFlowSubdivision :: FuncBlock -> Either String FuncBlock
controlFlowSubdivision (FuncBlock name blockList) =
  case blockList of
    [PBlock list] -> --single block, expected
      Right $ FuncBlock name $ map PBlock $ splitOnCF list
    _ -> Left "Wrong number of blocks for the function. Has analysis already been done?"
  where
        -- [PInst] -> [[PInst]] (breaks *after* each non-linear instruction)
        --
        -- Defined in terms of [PInst] instead of PBlock to avoid
        -- unwrapping and rewrapping on subcalls
        splitOnCF list =
          case list of
            [] -> []
            otherwise -> let (first, rest) = break controlFlowP list
                         in [first] ++ (splitOnCF rest)







