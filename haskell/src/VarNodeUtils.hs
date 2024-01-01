-- This file contains some specialized functions for VarNodes that are
-- used elsewhere to calculate affected and observed areas and
-- whatnot.

{-# LANGUAGE LambdaCase #-}

module VarNodeUtils where

import PCode

import Data.Maybe (fromJust)
import Data.List (foldl')

-- -------------------------------------------------------------------
-- Basic operations and comparisons. Returns Nothing when things are
-- incomparable.

intersect :: VarNode -> VarNode -> Maybe VarNode
-- ^ Commutative intersection. Local to address space.
intersect a@(VarNode as ao al) b@(VarNode bs bo bl)
  | ao > bo = intersect b a
  | as /= bs = Nothing
  | otherwise =
    let ae = ao + al
        be = bo + bl
    in
      if bo > ae
      then
        Nothing
      else
        Just $ VarNode as bo $ (min (ae - bo) be) - bo

difference :: VarNode -> VarNode -> Maybe [VarNode]
-- ^ Non-commutative difference. Gives A \ (A n B), the remainder of a
-- after cutting out b. Note that this may result in more than one
-- interval if b is a strict subset. Returns an empty list rather than
-- an empty VarNode if a is subsumed in b.
difference a@(VarNode as ao al) b@(VarNode bs bo bl)
  | as /= bs = Nothing
  | otherwise = Just $
    let startRel = fromJust (compareStart a b)
        endRel = fromJust (compareEnd a b)
        ae = ao + al
        be = bo + bl
    in --safe, done addrspace check
      case startRel of
        GT -> -- a starts after b
          case compare ao be of
            GT -> [a] -- b is entirely before, keep everything
            EQ -> [a] -- they touch but don't overlap
            LT -> case endRel of  -- a starts mid b
              LT -> []            -- a is contained in b
              EQ -> []            -- a is contained in b
              GT -> [VarNode as be (ae - be)] -- a extends past b
        EQ -> -- a and b start at the same place
          case endRel of
            LT -> [] -- nothing left
            EQ -> [] -- nothing left
            GT ->    -- a is larger, take end
              [VarNode as be (al - bl)]
        LT -> -- a starts before b
          case compare ae bo of
            LT -> [a] -- a entirely before
            EQ -> [a] -- touch but keep all
            GT -> case endRel of -- possible overlap
              LT ->              -- a overlaps, get just start
                [VarNode as ao (bo - ao)]
              EQ -> [VarNode as ao (bo - ao)] -- same thing
              GT ->                           -- a hangs off both ends
                [VarNode as ao (bo - ao), VarNode as be (be - ae)]

compareStart :: VarNode -> VarNode -> Maybe Ordering
-- ^ Partial order on varnodes by starting address. Only defined in
-- the same address space
compareStart (VarNode as ao _al) (VarNode bs bo _bl)
  | as /= bs = Nothing
  | otherwise = Just $ compare ao bo

compareEnd :: VarNode -> VarNode -> Maybe Ordering
-- ^ Partial order on varnodes by end address. Only defined in
-- the same address space
compareEnd (VarNode as ao al) (VarNode bs bo bl)
  | as /= bs = Nothing
  | otherwise = Just $ compare (ao + al) (bo + bl)

compareContains :: VarNode -> VarNode -> Maybe Ordering
-- ^ Partial order on varnodes by interval inclusion. A > B if A
-- includes all of B. Only defined in the same address space.
compareContains a@(VarNode _as ao _al) b@(VarNode _bs bo _bl)
  | ao > bo = (compareContains b a) >>= (\case
                                            GT -> Just LT
                                            EQ -> Just EQ
                                            LT -> error "non-symmetric < during compareContains")
  | otherwise =
      surrounding <$> (compareStart a b) <*> (compareEnd a b) >>= id
  where surrounding = (\cases { LT GT -> Just GT; EQ GT -> Just GT; GT GT -> Nothing;
                                LT EQ -> Just GT; EQ EQ -> Just EQ; GT EQ -> Nothing;
                                LT LT -> Nothing; EQ LT -> Nothing; GT LT -> Nothing})

-- -------------------------------------------------------------------
-- Tools for iterated applications of the above, with some insight
-- about what a VarNode represents. Basically these have some idea
-- about reads and writes.

-- TODO so this is basically an admission that I want to use the
-- register addr space like a second memory space, since that's how
-- pcode thinks about it. This, based on the difficulty / model of
-- ram, will be quite costly and maybe will need some tooling, but I
-- think is the way to go. I can maybe do some work and see if I can
-- get as far as binning the registers, and then figure out what the
-- llvm version does for subsets of registers, since that is not
-- uncommon.
--
-- Current thought is to do all of this stuff here, and then cast to
-- disjoint 8byte registers right before we feed it into
-- crucible. Particularly if we pick the granularity of dependence as
-- whole 8byte registers. That dicards most of the work here, but
-- allows us to change out mind in the future.
addObserved :: ([VarNode], [VarNode]) -> VarNode -> ([VarNode], [VarNode])
-- ^ Add a varnode to the observed list of a according to the
-- semantics of a block-aggregate observation list. Ie. don't include
-- a varnode that has already been written to, or one that is a direct
-- subset of an existing one. Assumes the passed lists are in program
-- order. Note that another pass is needed to fully eliminte some
-- redundancies.
--
-- Runtime is not obvious in the worst case due to splitting, but
-- average should be close to O(n) in each arg.
--
-- Touch then Observe:
-- Super then sub -> drop sub
-- sub then super -> difference (TODO sure?)
-- incomp -> inc both
--
-- Observe then Observe:
-- Superset then subset -> drop subset
-- Subset then superset -> include both (corrected elsewhere)
-- Incomparible pair -> include both (TODO are we sure, could be diff?)
addObserved !(observed, touched) !testNode
  | null observed && null touched =
      -- no conflicts, include it
      ([testNode], [])
  | isEmpty testNode =
      -- this test isn't a real dep, since it has no content, return inputs
      (observed, touched)
  | otherwise =
      -- we want to exhaust the observation list first, then the touch list
        let postObs = checkObsList observed testNode
            postTouch = concat $ map (checkTouchList touched) postObs
        in
          (observed ++ postTouch, touched)
  where
    -- What to include of this VarNode in the obs list, considering only the existing observation list
    checkObsList :: [VarNode] -> VarNode -> [VarNode]
    checkObsList preceding toTest
      | preceding == [] = [toTest]
      | ph <- head preceding,
        precedingRest <- tail preceding =
          case (compareContains ph toTest) of
            Nothing -> -- incomparable
              checkObsList precedingRest toTest
            Just GT -> -- prior superset
              []
            Just EQ -> -- duplicate
              []
            Just LT -> -- prior subset
              checkObsList precedingRest toTest
      -- What to include of this VarNode in the obs list, checking against the touch list
    checkTouchList :: [VarNode] -> VarNode -> [VarNode]
    checkTouchList touchList toTest
      | touchList == [] = [toTest]
      | th <- head touchList,
        tRest <- tail touchList =
            case (compareContains th toTest) of
              Nothing -> -- incomparable
                checkTouchList tRest toTest
              Just GT -> -- prior superset
                []
              Just EQ -> -- prior exact write
                []
              Just LT -> -- prior write to part of it
                concat $ map (checkTouchList tRest) $ fromJust $ difference toTest th
                -- split into possibly more than one leftover piece and
                -- keep checking. fromJust is safe because compareContains
                -- already checks for incomparably.

addTouched :: ([VarNode], [VarNode]) -> VarNode -> ([VarNode], [VarNode])
-- ^ Add a varnode to the touched list of a according to the semantics
-- of a block-aggregate touch list. Ie. don't include a varnode that a
-- direct subset of an existing one. Assumes the passed lists are in
-- program order. Note that another pass is needed to fully eliminte
-- some orders of redundancies.
--
-- O(n) in second arg
--
-- Touch first:
-- Super then sub -> drop sub
-- sub then super -> inc both (corrected elsewhere)
-- incomp -> inc both
addTouched !(observed, touched) !test
  | null touched =
    (observed, [test]) -- nothing to compare with
  | otherwise =
    let nextT = head touched
        touchedRest = tail touched
    in
      case compareContains test nextT of
        Nothing -> -- incomparable, continue
          let (o,t) = addTouched (observed, touchedRest) test
          in
            (o, nextT:t)
        Just GT -> -- superset, include and filter later
          let (o,t) = addTouched (observed, touchedRest) test
          in
            (o, nextT:t)
        Just EQ -> -- dup, eliminate, return inputs
          (observed, touched)
        Just LT -> -- subset, eliminate, return inputs
          (observed, touched)

eliminateSubsets :: [VarNode] -> [VarNode]
-- ^ Eliminates anything that is a subset of a later element. O(n^2).
eliminateSubsets [] = []
eliminateSubsets (vh:[]) = [vh]
eliminateSubsets (vh:vr) =
  es' [] vh vr
  where shouldRemoveP test inList =
          compareContains test inList == Just LT
        shouldRemoveLP test vlist =
          foldl' (flip ((||) . (shouldRemoveP test))) False vlist
        -- es' :: considered -> current -> remaining -> output
        es' prior cur [] =
            prior++[cur]
        es' prior cur later@(lh:lr) =
          if shouldRemoveLP cur later
          then
            es' prior lh lr
          else
            es' (prior++[cur]) lh lr
