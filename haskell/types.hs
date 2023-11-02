-- This file should have types that will be of general use

import Numeric (showHex)
import Data.Text (Text, pack, unpack, empty, strip, splitOn)
import Data.Text.Read (decimal, hexadecimal)
import Data.List (elemIndex)
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

-- Expects a parenthesized string representation like is produced by
-- the dumping script or the `vnShow` bellow
fromPrintedVarNode :: String -> Maybe VarNode
fromPrintedVarNode str =
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
instance Show (VarNode) where
  show (VarNode as off len) =
    "(" ++ as ++ ", 0x" ++ showHex off ", " ++ (show len) ++ ")"

-- -------------------------------------------------------------------
-- Pcode operations.
--
-- In general the order is the inputs in order and the output if there
-- is one.
--
-- See the reference, the formal semantics, or my writeup for what
-- they do.


-- Type for a single Pcode instruction.
data POpt =
  PO_COPY VarNode VarNode |
  PO_INT_ADD VarNode VarNode VarNode |
  PO_BOOL_OR VarNode VarNode VarNode |
  PO_LOAD VarNode VarNode VarNode |
  PO_INT_SUB VarNode VarNode VarNode |
  PO_FLOAT_EQUAL VarNode VarNode VarNode |
  PO_STORE VarNode VarNode VarNode |
  PO_INT_CARRY VarNode VarNode VarNode |
  PO_FLOAT_NOTEQUAL VarNode VarNode VarNode |
  PO_BRANCH VarNode | -- See the reference for interpretation
  PO_INT_SCARRY VarNode VarNode VarNode |
  PO_FLOAT_LESS VarNode VarNode VarNode |
  PO_CBRANCH VarNode VarNode |
  PO_INT_SBORROW VarNode VarNode VarNode |
  PO_FLOAT_LESSEQUAL VarNode VarNode VarNode |
  PO_BRANCHIND VarNode |
  PO_INT_2COMP VarNode VarNode |
  PO_FLOAT_ADD VarNode VarNode VarNode |
  PO_CALL VarNode | -- In raw Pcode only ever one input
  PO_INT_NEGATE VarNode VarNode |
  PO_FLOAT_SUB VarNode VarNode VarNode |
  PO_CALLIND VarNode |
  PO_INT_XOR VarNode VarNode VarNode |
  PO_FLOAT_MULT VarNode VarNode VarNode |
  PO_INT_AND VarNode VarNode VarNode |
  PO_FLOAT_DIV VarNode VarNode VarNode |
  PO_RETURN VarNode | -- In raw Pcode only ever one input
  PO_INT_OR VarNode VarNode VarNode |
  PO_FLOAT_NEG VarNode VarNode |
  PO_PIECE VarNode VarNode VarNode |
  PO_INT_LEFT VarNode VarNode VarNode |
  PO_FLOAT_ABS VarNode VarNode |
  PO_SUBPIECE VarNode VarNode VarNode | -- Second input should be constant
  PO_INT_RIGHT VarNode VarNode VarNode |
  PO_FLOAT_SQRT VarNode VarNode |
  PO_INT_EQUAL VarNode VarNode VarNode |
  PO_INT_SRIGHT VarNode VarNode VarNode |
  PO_FLOAT_CEIL VarNode VarNode |
  PO_INT_NOTEQUAL VarNode VarNode VarNode |
  PO_INT_MULT VarNode VarNode VarNode |
  PO_FLOAT_FLOOR VarNode VarNode |
  PO_INT_LESS VarNode VarNode VarNode |
  PO_INT_DIV VarNode VarNode VarNode |
  PO_FLOAT_ROUND VarNode VarNode |
  PO_INT_SLESS VarNode VarNode VarNode |
  PO_INT_REM VarNode VarNode VarNode |
  PO_FLOAT_NAN VarNode VarNode |
  PO_INT_LESSEQUAL VarNode VarNode VarNode |
  PO_INT_SDIV VarNode VarNode VarNode |
  PO_INT2FLOAT VarNode VarNode |
  PO_INT_SLESSEQUAL VarNode VarNode VarNode |
  PO_INT_SREM VarNode VarNode VarNode |
  PO_FLOAT2FLOAT VarNode VarNode |
  PO_INT_ZEXT VarNode VarNode |
  PO_BOOL_NEGATE VarNode VarNode |
  PO_TRUNC VarNode VarNode |
  PO_INT_SEXT VarNode VarNode |
  PO_BOOL_XOR VarNode VarNode VarNode |
  PO_BOOL_AND VarNode VarNode VarNode |
  PO_POPCOUNT VarNode VarNode

-- These Shouldn't appear in raw pcode
-- data PO_CPOOLREF = -- variadic
-- data PO_NEW = -- variadic
-- data PO_USERDEFINED = -- variadic

-- The number of arguments to expect given the string name of the
-- optcode
numArgs :: String -> Maybe Integer
numArgs opt =
  case opt of
    "COPY"            -> Just 2
    "INT_ADD"         -> Just 3
    "BOOL_OR"         -> Just 3
    "LOAD"            -> Just 3
    "INT_SUB"         -> Just 3
    "FLOAT_EQUAL"     -> Just 3
    "STORE"           -> Just 3
    "INT_CARRY"       -> Just 3
    "FLOAT_NOTEQUAL"  -> Just 3
    "BRANCH"          -> Just 1
    "INT_SCARRY"      -> Just 3
    "FLOAT_LESS"      -> Just 3
    "CBRANCH"         -> Just 2
    "INT_SBORROW"     -> Just 3
    "FLOAT_LESSEQUAL" -> Just 3
    "BRANCHIND"       -> Just 1
    "INT_2COMP"       -> Just 2
    "FLOAT_ADD"       -> Just 3
    "CALL"            -> Just 1
    "INT_NEGATE"      -> Just 2
    "FLOAT_SUB"       -> Just 3
    "CALLIND"         -> Just 1
    "INT_XOR"         -> Just 3
    "FLOAT_MULT"      -> Just 3
    "INT_AND"         -> Just 3
    "FLOAT_DIV"       -> Just 3
    "RETURN"          -> Just 1
    "INT_OR"          -> Just 3
    "FLOAT_NEG"       -> Just 2
    "PIECE"           -> Just 3
    "INT_LEFT"        -> Just 3
    "FLOAT_ABS"       -> Just 2
    "SUBPIECE"        -> Just 3
    "INT_RIGHT"       -> Just 3
    "FLOAT_SQRT"      -> Just 2
    "INT_EQUAL"       -> Just 3
    "INT_SRIGHT"      -> Just 3
    "FLOAT_CEIL"      -> Just 2
    "INT_NOTEQUAL"    -> Just 3
    "INT_MULT"        -> Just 3
    "FLOAT_FLOOR"     -> Just 2
    "INT_LESS"        -> Just 3
    "INT_DIV"         -> Just 3
    "FLOAT_ROUND"     -> Just 2
    "INT_SLESS"       -> Just 3
    "INT_REM"         -> Just 3
    "FLOAT_NAN"       -> Just 2
    "INT_LESSEQUAL"   -> Just 3
    "INT_SDIV"        -> Just 3
    "INT2FLOAT"       -> Just 2
    "INT_SLESSEQUAL"  -> Just 3
    "INT_SREM"        -> Just 3
    "FLOAT2FLOAT"     -> Just 2
    "INT_ZEXT"        -> Just 2
    "BOOL_NEGATE"     -> Just 2
    "TRUNC"           -> Just 2
    "INT_SEXT"        -> Just 2
    "BOOL_XOR"        -> Just 3
    "BOOL_AND"        -> Just 3
    "POPCOUNT"        -> Just 2
    otherwise         -> Nothing

strToPOCon1 :: String -> Maybe (VarNode -> POpt)
strToPOCon1 opt =
  case opt of
    "BRANCH"          -> Just PO_BRANCH
    "BRANCHIND"       -> Just PO_BRANCHIND
    "CALL"            -> Just PO_CALL
    "CALLIND"         -> Just PO_CALLIND
    "RETURN"          -> Just PO_RETURN
    otherwise         -> Nothing

strToPOCon2 :: String -> Maybe (VarNode -> VarNode -> POpt)
strToPOCon2 opt =
  case opt of
    "COPY"            -> Just PO_COPY
    "CBRANCH"         -> Just PO_CBRANCH
    "INT_2COMP"       -> Just PO_INT_2COMP
    "INT_NEGATE"      -> Just PO_INT_NEGATE
    "FLOAT_NEG"       -> Just PO_FLOAT_NEG
    "FLOAT_ABS"       -> Just PO_FLOAT_ABS
    "FLOAT_SQRT"      -> Just PO_FLOAT_SQRT
    "FLOAT_CEIL"      -> Just PO_FLOAT_CEIL
    "FLOAT_FLOOR"     -> Just PO_FLOAT_FLOOR
    "FLOAT_ROUND"     -> Just PO_FLOAT_ROUND
    "FLOAT_NAN"       -> Just PO_FLOAT_NAN
    "INT2FLOAT"       -> Just PO_INT2FLOAT
    "FLOAT2FLOAT"     -> Just PO_FLOAT2FLOAT
    "INT_ZEXT"        -> Just PO_INT_ZEXT
    "BOOL_NEGATE"     -> Just PO_BOOL_NEGATE
    "TRUNC"           -> Just PO_TRUNC
    "INT_SEXT"        -> Just PO_INT_SEXT
    "POPCOUNT"        -> Just PO_POPCOUNT
    otherwise         -> Nothing

strToPOCon3 :: String -> Maybe (VarNode -> VarNode -> VarNode -> POpt)
strToPOCon3 opt =
  case opt of
    "INT_ADD"         -> Just PO_INT_ADD
    "BOOL_OR"         -> Just PO_BOOL_OR
    "LOAD"            -> Just PO_LOAD
    "INT_SUB"         -> Just PO_INT_SUB
    "FLOAT_EQUAL"     -> Just PO_FLOAT_EQUAL
    "STORE"           -> Just PO_STORE
    "INT_CARRY"       -> Just PO_INT_CARRY
    "FLOAT_NOTEQUAL"  -> Just PO_FLOAT_NOTEQUAL
    "INT_SCARRY"      -> Just PO_INT_SCARRY
    "FLOAT_LESS"      -> Just PO_FLOAT_LESS
    "INT_SBORROW"     -> Just PO_INT_SBORROW
    "FLOAT_LESSEQUAL" -> Just PO_FLOAT_LESSEQUAL
    "FLOAT_ADD"       -> Just PO_FLOAT_ADD
    "FLOAT_SUB"       -> Just PO_FLOAT_SUB
    "INT_XOR"         -> Just PO_INT_XOR
    "FLOAT_MULT"      -> Just PO_FLOAT_MULT
    "INT_AND"         -> Just PO_INT_AND
    "FLOAT_DIV"       -> Just PO_FLOAT_DIV
    "INT_OR"          -> Just PO_INT_OR
    "PIECE"           -> Just PO_PIECE
    "INT_LEFT"        -> Just PO_INT_LEFT
    "SUBPIECE"        -> Just PO_SUBPIECE
    "INT_RIGHT"       -> Just PO_INT_RIGHT
    "INT_EQUAL"       -> Just PO_INT_EQUAL
    "INT_SRIGHT"      -> Just PO_INT_SRIGHT
    "INT_NOTEQUAL"    -> Just PO_INT_NOTEQUAL
    "INT_MULT"        -> Just PO_INT_MULT
    "INT_LESS"        -> Just PO_INT_LESS
    "INT_DIV"         -> Just PO_INT_DIV
    "INT_SLESS"       -> Just PO_INT_SLESS
    "INT_REM"         -> Just PO_INT_REM
    "INT_LESSEQUAL"   -> Just PO_INT_LESSEQUAL
    "INT_SDIV"        -> Just PO_INT_SDIV
    "INT_SLESSEQUAL"  -> Just PO_INT_SLESSEQUAL
    "INT_SREM"        -> Just PO_INT_SREM
    "BOOL_XOR"        -> Just PO_BOOL_XOR
    "BOOL_AND"        -> Just PO_BOOL_AND
    otherwise         -> Nothing

-- The use here is the first token and the rest of the line both as
-- strings. It's generic to allow operating on strings with a split
-- type operation in a Monadic fashion.
--
-- TODO MAJOR DANGER ZONE. GET OUTSIDE EYES --------------------------

-- TODO I'm taking the "no errors / undefined behavior" attitude
-- here, but idk if that's best

-- The idea here is that you have two maybes of the same type. Then
-- you can apply functions through the maybe, and functions are
-- applied to both. If you wrap functions in TokenRest, then the
-- second is applied to both arguments, and if it is Nothing, then it
-- produces a Nothing Pair

newtype TokenRest s = TR (Maybe s, Maybe s)

instance Functor TokenRest where
  fmap f (TR (Just t, Just s)) = TR (Just (f t), Just (f s))
  fmap f (TR (Just t, Nothing)) = TR (Just (f t), Nothing)
  fmap f (TR (Nothing, Just s)) = TR (Nothing, Just (f s))
  fmap f (TR (Nothing, Nothing)) = TR (Nothing, Nothing)
  -- TODO is ^ right?

instance Applicative TokenRest where
  pure a = TR (Nothing, Just a)
  TR (_, Just f) <*> TR (Just t, Just s) = TR (Just (f t), Just (f s))
  TR (_, Just f) <*> TR (Just t, Nothing) = TR (Just (f t), Nothing)
  TR (_, Just f) <*> TR (Nothing, Just s) = TR (Nothing, Just (f s))
  TR (_, Just f) <*> TR (Nothing, Nothing) = TR (Nothing, Nothing)
  TR (_, Nothing) <*> TR (_, _) = TR (Nothing, Nothing)
  -- TODO ^ same question

instance Monad TokenRest where
  return = pure
  TR (_, Just a) >>= f = f a
  -- This ^ line is the only reason we are doing any of this
  TR (_, Nothing) >>= f = TR (Nothing, Nothing)
  -- TODO ^ same question
  -- discards the first element and replaces it with the result of f


parseSingle :: String -> TokenRest String
parseSingle "" = TR (Nothing, Nothing)
parseSingle str =
  let stripped = unpack $ strip $ pack str
      space = elemIndex ' ' stripped
      paren = elemIndex ')' stripped
  in
    if head stripped == '('
    then case paren of
           -- we are looking for parens, case on the close location
           Just p -> TR (Just (take (p + 1) stripped), Just (drop (p + 2) stripped))
           Nothing -> TR (Nothing, Just str)
    else case space of
           -- we are looking for spaces, case on space location
           Just s -> TR (Just (take s stripped), Just (drop (s + 1) stripped))
           Nothing -> TR (Nothing, Just str)


-- Expects the format given by dumping script
fromPrintedPOpt :: String -> Maybe POpt
fromPrintedPOpt str =
  case (return str >>= parseSingle) of
    TR (Nothing, Nothing) -> Nothing -- Should never happen
    TR (Just opt, Nothing) -> Nothing -- No zero arg opts
    TR (Nothing, Just rest) -> Nothing -- Should never happen
    out1@(TR (Just opt, Just rest)) -> -- main case
      case numArgs opt of
        Just 1 -> case (out1 >>= parseSingle) of
          TR (Nothing, _) -> Nothing
          TR (marg1, _) ->
            strToPOCon1 opt <*> (marg1 >>= fromPrintedVarNode)
            -- we can save tine and not unwrap the Maybe since we want
            -- to return one anway. Yay Monads!
        Just 2 -> case (out1 >>= parseSingle) of
          TR (Nothing, _) -> Nothing
          TR (_, Nothing) -> Nothing
          out2@(TR (marg1, _)) -> case (out2 >>= parseSingle) of
            TR (Nothing, _) -> Nothing
            TR (marg2, _) -> strToPOCon2 opt <*>
                                  (marg1 >>= fromPrintedVarNode) <*>
                                  (marg2 >>= fromPrintedVarNode)
        Just 3 -> case (out1 >>= parseSingle) of
          TR (Nothing, _) -> Nothing
          TR (_, Nothing) -> Nothing
          out2@(TR (marg1, _)) -> case (out2 >>= parseSingle) of
            TR (Nothing, _) -> Nothing
            TR (_, Nothing) -> Nothing
            out3@(TR (marg2, _)) -> case (out3 >>= parseSingle) of
              TR (Nothing, _) -> Nothing
              TR (marg3, _) -> strToPOCon3 opt <*>
                                    (marg1 >>= fromPrintedVarNode) <*>
                                    (marg2 >>= fromPrintedVarNode) <*>
                                    (marg3 >>= fromPrintedVarNode)


-- TODO END DANGER ZONE ----------------------------------------------


-- Prints a representation that should match the above and the dumping
-- script
instance Show (POpt) where
  show (PO_COPY a b) = "COPY " ++ show a ++ ", " ++ show b
  show (PO_INT_ADD a b c) = "INT_ADD " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_BOOL_OR a b c) = "BOOL_OR " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_LOAD a b c) = "LOAD " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_SUB a b c) = "INT_SUB " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_EQUAL a b c) = "FLOAT_EQUAL " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_STORE a b c) = "STORE " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_CARRY a b c) = "INT_CARRY " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_NOTEQUAL a b c) = "FLOAT_NOTEQUAL " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_BRANCH a) = "BRANCH " ++ show a
  show (PO_INT_SCARRY a b c) = "INT_SCARRY " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_LESS a b c) = "FLOAT_LESS " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_CBRANCH a b) = "CBRANCH " ++ show a ++ ", " ++ show b
  show (PO_INT_SBORROW a b c) = "INT_SBORROW " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_LESSEQUAL a b c) = "FLOAT_LESSEQUAL " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_BRANCHIND a) = "BRANCHIND " ++ show a
  show (PO_INT_2COMP a b) = "INT_2COMP " ++ show a ++ ", " ++ show b
  show (PO_FLOAT_ADD a b c) = "FLOAT_ADD " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_CALL a) = "CALL " ++ show a
  show (PO_INT_NEGATE a b) = "INT_NEGATE " ++ show a ++ ", " ++ show b
  show (PO_FLOAT_SUB a b c) = "FLOAT_SUB " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_CALLIND a) = "CALLIND " ++ show a
  show (PO_INT_XOR a b c) = "INT_XOR " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_MULT a b c) = "FLOAT_MULT " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_AND a b c) = "INT_AND " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_DIV a b c) = "FLOAT_DIV " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_RETURN a) = "RETURN " ++ show a
  show (PO_INT_OR a b c) = "INT_OR " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_NEG a b) = "FLOAT_NEG " ++ show a ++ ", " ++ show b
  show (PO_PIECE a b c) = "PIECE " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_LEFT a b c) = "INT_LEFT " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_ABS a b) = "FLOAT_ABS " ++ show a ++ ", " ++ show b
  show (PO_SUBPIECE a b c) = "SUBPIECE " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_RIGHT a b c) = "INT_RIGHT " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_SQRT a b) = "FLOAT_SQRT " ++ show a ++ ", " ++ show b
  show (PO_INT_EQUAL a b c) = "INT_EQUAL " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_SRIGHT a b c) = "INT_SRIGHT " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_CEIL a b) = "FLOAT_CEIL " ++ show a ++ ", " ++ show b
  show (PO_INT_NOTEQUAL a b c) = "INT_NOTEQUAL " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_MULT a b c) = "INT_MULT " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_FLOOR a b) = "FLOAT_FLOOR " ++ show a ++ ", " ++ show b
  show (PO_INT_LESS a b c) = "INT_LESS " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_DIV a b c) = "INT_DIV " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_ROUND a b) = "FLOAT_ROUND " ++ show a ++ ", " ++ show b
  show (PO_INT_SLESS a b c) = "INT_SLESS " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_REM a b c) = "INT_REM " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT_NAN a b) = "FLOAT_NAN " ++ show a ++ ", " ++ show b
  show (PO_INT_LESSEQUAL a b c) = "INT_LESSEQUAL " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_SDIV a b c) = "INT_SDIV " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT2FLOAT a b) = "INT2FLOAT " ++ show a ++ ", " ++ show b
  show (PO_INT_SLESSEQUAL a b c) = "INT_SLESSEQUAL " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_INT_SREM a b c) = "INT_SREM " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_FLOAT2FLOAT a b) = "FLOAT2FLOAT " ++ show a ++ ", " ++ show b
  show (PO_INT_ZEXT a b) = "INT_ZEXT " ++ show a ++ ", " ++ show b
  show (PO_BOOL_NEGATE a b) = "BOOL_NEGATE " ++ show a ++ ", " ++ show b
  show (PO_TRUNC a b) = "TRUNC " ++ show a ++ ", " ++ show b
  show (PO_INT_SEXT a b) = "INT_SEXT " ++ show a ++ ", " ++ show b
  show (PO_BOOL_XOR a b c) = "BOOL_XOR " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_BOOL_AND a b c) = "BOOL_AND " ++ show a ++ ", " ++ show b ++ ", " ++ show c
  show (PO_POPCOUNT a b) = "POPCOUNT " ++ show a ++ ", " ++ show b



-- A Machine address, a Pcode operation and the index of said
-- operation inside the machine instruction. This encoding allows for
-- operation on sequences of this type without data loss.
data PInst = PInst Integer POpt Integer

-- Not quite the same format as the dump, includes the offset
-- explicitly
instance Show (PInst) where
  show (PInst mpc p off) =
    showHex mpc $ "\t" ++ showHex off ("\t" ++ show p)

fromPrintedPInst :: String -> Maybe PInst
fromPrintedPInst line =
  let tokens = splitOn (pack "\t") (pack line)
      eaddress = hexadecimal $ head tokens
      eoffset = decimal $ head $ tail tokens
      eitherToMaybe = (\ e -> case e of
                          Left _ -> Nothing
                          Right (a, _) -> Just a)
      maddress = eitherToMaybe eaddress
      moffset = eitherToMaybe eoffset
      rest = unpack $ head $ drop 2 tokens
  in
    (Just PInst) <*> maddress <*> (fromPrintedPOpt rest) <*> moffset

-- TODO these two following sections are not clearly useful, so they
-- will remain commented for now

-- -------------------------------------------------------------------
-- Machine operations.
--
-- Simply groupings of Pcode operations. There can be internal pcode
-- relative control flow, but this is the granularity of machine
-- visible branches. So control flow graphs may not respect these
-- divisions, as a conditional branch that optionally sets things like
-- control registers may have a branch POpt in a non-tail position.
--
-- TODO test that claim
--
-- Unclear how useful this is, but it's an interesting and possibly
-- insightful semantic division.

-- Single Machine instruction. An sequence of PCode
-- Operations. Information like location are not internal.
-- data MOpt = [POpt]

-- -------------------------------------------------------------------
-- Function blocks
--
-- A high level block of code at the granularity of machine
-- instructions. These should be semantically meaningful divisions to
-- the programmer. I.E. a single subroutine in the original language
-- or something of that ilk.
--
-- These divisions are useful because it's hard to reason about the
-- program from the pcode at a level higher than this. If we take
-- these are C-esque functions, then they all have variable entry
-- points, and they return to a variable location from the stack. That
-- value is runtime dependent, and while we could draw up a list of
-- possibilities for many programs (first order programs if I had to
-- catagorize off the cuff :thinking:), these are divisions about
-- which there is principled and thus restrained operations. Putting
-- aside some optimization, this is a location that generally conforms
-- to a calling convention and thus we can easily divide along these
-- lines.
--
-- TODO we should check that our assumptions hold. I.E. things like
-- not jumping between different functions accept with calls and
-- returns. How isolated are these? How strongly can we reason, and
-- how much semantically meaningful info can we recover? We don't want
-- to rehash Ghidra, so meaning recovery isn't really the focus.



-- -------------------------------------------------------------------
--
-- Blocks are sequences of Pcode that we have reason to believe are
-- atomic with respect to control flow.
--
-- Note that full confidence requires analysis of indirect branches

-- Atomic block with respect to control flow.
data PBlock = PBlock [PInst]

instance Show (PBlock) where
  show (PBlock list) =
    concat $ map show list
