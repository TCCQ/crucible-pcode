-- This module is the last stop before crucible land, and is mostly
-- for transforming my various data structures and types into things
-- to feed to the CFG generator module.

module IntoGenerator where

import qualified Data.Map as Map
import Control.Lens

-- import qualified Crucible.Lang.Crucible.FunctionHandle as C.FuncH
-- import qualified Crucible.Lang.Crucible.CFG.Generator as C.Gen
-- import qualified Crucible.Lang.Crucible.CFG.Reg as C.Reg
-- import qualified Crucible.Lang.Crucible.CFG.Generator as C.Gen

import PCode
import VarNodeUtils
import Analysis
import PreCrucible
import ConversionTypes

-- We call into the CFG with the register set, and immediately
-- populate the local registers. Block movement with arguments is all
-- in the registers, but is still annotated. At the end of the CFG, we
-- load our RegisterSet back with whatever we took from it (or
-- everything).
--
-- After a returning call, we need to apply the RegisterSet to the
-- current register state.

-- So the step of our generator should take a PInst and just generate
-- a simple generator action, with no extra structure. All the smart
-- stuff happens at the start and end of each CFG, on both sides of
-- the call / return.
