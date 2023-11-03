import System.Environment

main = do
  args <- getArgs
  case args of
    [name] -> do
      contents <- readFile name
      -- We start our analysis here
      --
      -- TODO if there was an interactive loop, it would also go here
      putStr contents
    _ -> error "Wrong number of arguments. You need to pass the file name as an arguement!"
