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

-- Does this instruction have runtime calculated control flow targets?
indirectP :: PInst -> Bool
indirectP (PInst _ popt _) =
          case popt of
            PO_BRANCHIND _ -> True
            PO_CALLIND _ -> True
            PO_RETURN _ -> True
            otherwise -> False


-- Does this block have an indirect terimantor?
indTermP :: PBlock -> Bool
indTermP (PBlock instList) =
  indirectP $ last instList


-- list of compile time next instruction locations, as (machine
-- address, pcode offset) pairs
--
-- Left values are things that can go wrong
ctSuccessors :: PBlock -> Either String [(Integer, Integer)]
ctSuccessors (PBlock instList) =
  if indirectP terminating
  then Left "This block ends in an indirect instruction, this function is for compile time stuff."
  else case terminating of
              PO_BRANCH target ->
                if (addressSpace target == "ram")
                then Right [(offset target, 0)]
                     -- Single successor, first pcode op of the pointed to machine instruction
                else if (addressSpace target == "constant")
                     then Right [(termAddr, termOffet + (offset target))]
                          -- Single, p-code relative branch
                     else Left "Branch without constant or ram address space."
              PO_CBRANCH _ target ->
                if (addressSpace target == "ram")
                then Right [(offset target, 0), fallthrough]
                     -- Branch or fallthrough, see above
                else if (addressSpace target == "constant")
                     then Right [(termAddr, termOffet + (offset target)), fallthrough]
                          -- p-code relative branch or fallthrough, see above
                     else Left "CBranch without constant or ram address space."
              PO_CALL _ -> -- Same as branch
                if (addressSpace target == "ram")
                then Right [(offset target, 0)]
                     -- Single successor, first pcode op of the pointed to machine instruction
                else if (addressSpace target == "constant")
                     then Left "Call with pcode relative target."
                     else Left "Call without constant or ram address space."
              otherwise ->
                Left "Unexpected terminating instruction. Is this a non control flow terminated block?"
  where terminating@(PInst termAddr _ termOffset) = last instList
        fallthroughOffset = -- TODO !!!! is this safe as just an offset? and how much? !!!!
        fallthrough = (termAddr + fallthroughOffset, 0)


matchBlock :: FuncBlock -> (Integer, Integer) -> Maybe PBlock
matchBlock (FuncBlock _ blockList) (maddr, poff) =
  recurList blockList (maddr, poff)
  -- TODO do I need to do this? Is there a performance hit if I just
  -- do it in terms of FuncBlock? There is a clear wrapping/
  -- unwrapping, but also I only use one part, so maybe it would
  -- optimize it out?
  where recurList [] _ = Nothing
        recurList bl (m, p) =
             case head $ head bl of
               m _ p -> Just $ head bl
               otherwise -> recurList (tail bl) (m, p)

matchIndex :: FuncBlock -> (Integer, Integer) -> Maybe Integer

matchIndex (FuncBlock _ blockList) (maddr, poff) =
  recurList blockList (maddr, poff) 0
  where recurList [] _ _ = Nothing
        recurList bl (m, p) idx =
             case head $ head bl of
               m _ p -> Just idx
               otherwise -> recurList (tail bl) (m, p) (idx+1)


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

-- This represents the directed control flow graph of a
-- function. Blocks are indexed by their index in the FuncBlock this
-- matrix represents (TODO is that number stable?).
--
-- entry i,j is 1 if block j can succeed block i (forward link)
-- -1 if block i can succeed block j (backward link)
-- 0 if they are not reachable by one another in one block step.
--
-- self loops are marked 2, as they are both 1 and -1
--
-- Indirect jumps will just be marked with 3, and later passes can do
-- interval analysis if they want
--
-- TODO make the contents a type
data FuncGraph = FuncGraph Matrix Integer

ctMakeGraph :: funcblock -> funcgraph
ctMakeGraph fb@(FuncBlock _ blockList) =
  FuncGraph (matrix n n generator) n
  where n = (length blockList)
        connected :: PBlock -> Integer -> Integer first secondIdx =
          -- applied only to i <= j, 2, 3, -1 outputs are handled higher
          if elem secondIdx blockSuccList
          then 1
          else 0
          where firstSucc = case ctSuccessors first of
                  Right a -> a
                  Left msg -> error msg
                  -- Can't really pass my errors up more here very
                  -- easily. Might as well just panic
                -- firstSucc :: [(Integer, Integer)]
                blockSuccList = map (\ addr ->
                                       case matchIndex fb addr of
                                         Just i ->  i
                                         Nothing -> error "Non-indirect jump out of function?") firstSucc
                -- blockSuccList :: [Integer], should be applied only to non-Ind terminated blocks
        generator i j =
                if indTermP $ blockList !! i
                then 3 --block out whole col for ind
                else if j > i
                     then -1 * (generator j i)
                     else case connected (blockList !! i) j of
                            0 -> 0
                            1 -> if i == j
                                 then 2
                                 else 1



