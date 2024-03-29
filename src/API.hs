module API where

-- import System.Environment

import Control.Lens

import FromDump (parseDump)
import Analysis

pCodeMain :: FilePath -> IO ()
pCodeMain dumpFile = do
      fileContents <- parseDump dumpFile
      case fileContents of
        Right functionList -> sequence (map (putStr . (view name)) functionList) >> return ()
        Left parseError -> putStr parseError
      -- We start our analysis here
      --
      -- TODO if there was an interactive loop, it would also go here

processFunc :: PFunction -> Either String AugCFG
processFunc func = do
  cfg <- initialGraph func
  commited <- return $ cfgCommitNonIndirect cfg
  mToE "cfg wasn't stable. Indirect branches maybe?" (augmentCfg commited)
  where mToE = (\msg v -> case v of
                   Just v -> Right v
                   Nothing -> Left msg)

