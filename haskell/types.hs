-- This file should have types that will be of general use

import Numeric (showHex)
import Data.Text (Text, pack, empty)
import Data.Text.Read (decimal, hexadecimal)
import Data.Either (fromRight)
import Debug.Trace (trace)

-- (Address Space Id, offset (signed for constants), Length/Size)
-- Length should always be >= 0 I think
data VarNode = VarNode String Integer Integer

-- getters
addressSpace :: VarNode -> String
addressSpace (VarNode as _ _) = as

offset :: VarNode -> Integer
offset (VarNode _ off _) = off

length :: VarNode -> Integer
length (VarNode _ _ len) = len

vnFromPrinted :: String -> Maybe VarNode
vnFromPrinted str =
  case break (==',') (tail (init str)) of
    (addr, ',':' ':rest) ->
      case break (==',') rest of
        (offstr, ',':' ':lenstr) ->
          -- only get here if it decomposes nicely
          let off = fromRight (-1, empty) (hexadecimal (pack offstr))
              len = fromRight (-1, empty) (decimal (pack lenstr))
          in
            case (fst off, fst len) of
              (-1, _) -> Nothing
              (_, -1) -> Nothing
              (o, l) -> Just $ VarNode addr o l
        otherwise -> Nothing
    otherwise -> Nothing



-- Should give matching results as the format given by the dumping
-- script
vnShow :: VarNode -> String
vnShow (VarNode as off len) =
  "(" ++ as ++ ", 0x" ++ showHex off ", " ++ show len ++ ")"

