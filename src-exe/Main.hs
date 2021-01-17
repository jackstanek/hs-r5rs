module Main where

import Parse (parseProgram)

import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings newLoop
  where loop :: String -> InputT IO ()
        loop prevInput = do
          minput <- getInputLine $ if null prevInput then ">>> " else "... "
          case minput of
            Nothing -> return ()
            Just input -> let inputSoFar = prevInput ++ input in
                          case (parseProgram "stdin" inputSoFar, input) of
                            (Right result, _) -> do outputStrLn $ show result
                                                    newLoop
                            (Left err, "") -> do outputStrLn $ show err
                                                 newLoop
                            (Left _, _) -> loop $ inputSoFar ++ "\n"
        newLoop = loop ""
