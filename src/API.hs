module API where

-- import System.Environment

import Control.Lens
import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Text (pack)


import FromDump (parseDump)
import PCode
import Analysis
import ConversionTypes
import PreCrucible
import Effects

import Data.Parameterized.Some
import Data.Parameterized.Nonce (globalNonceGenerator)
import Lang.Crucible.FunctionHandle
import qualified Lang.Crucible.CFG.Reg as C.Reg
import Lang.Crucible.CFG.Generator
import What4.FunctionName
import What4.ProgramLoc (Position(..))

pCodeMain :: FilePath -> IO ()
pCodeMain dumpFile = do
      fileContents <- parseDump dumpFile
      case fileContents of
        Right functionList -> sequence (map (putStr . (view name)) functionList) >> return ()
        Left parseError -> putStr parseError
      -- We start our analysis here
      --
      -- TODO if there was an interactive loop, it would also go here

processFunc :: PFunction -> Either String (String, AugCFG)
processFunc func = do
  cfg <- initialGraph func
  commited <- return $ cfgCommitNonIndirect cfg
  contents <- mToE "cfg wasn't stable. Indirect branches maybe?" (augmentCfg commited)
  return (func^.name, contents)
  where mToE = (\msg v -> case v of
                   Just v -> Right v
                   Nothing -> Left msg)

intoRegCFG :: PAddr -> PCodeGenerator s () -> FnHandle fargs fret -> Either String (C.Reg.CFG () s cinit cret)
intoRegCFG startAddr genState funcHandle = do
  reportPos <- return $ BinaryPos (pack "PCode machine offset in program binary") (fromIntegral (startAddr^.maddr))
  -- (someCFG, []) <- defineFunction reportPos (Some globalNonceGenerator) funcHandle
  return $ error "TODO"


