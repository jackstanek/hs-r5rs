module Main where

import Parse (parseExpr)

import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings newLoop
  where loop :: String -> InputT IO ()
        loop prevInput = do
          minput <- getInputLine $ if null prevInput then ">>> " else "... "
          case minput of
            Nothing -> return ()
            Just input -> let inputSoFar = prevInput ++ input in
                          case (parseExpr "stdin" inputSoFar, input) of
                            (Right result, _) -> do outputStrLn $ show result
                                                    newLoop
                            (Left err, "") -> do outputStrLn $ show err
                                                 newLoop
                            (Left _, _) -> loop $ inputSoFar ++ "\n"
        newLoop = loop ""
