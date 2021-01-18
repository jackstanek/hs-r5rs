module Main where

import Eval (execute)
import Parse (parseProgram)

import System.Console.Haskeline

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f e = case e of
  Left a -> Left $ f a
  Right a -> Right a

main :: IO ()
main = runInputT defaultSettings newLoop
  where loop :: String -> InputT IO ()
        loop prevInput = do
          minput <- getInputLine $ if null prevInput then ">>> " else "... "
          case minput of
            Nothing -> return ()
            Just input -> let inputSoFar = prevInput ++ input in
                          case (mapLeft show (parseProgram "stdin" inputSoFar) >>= execute, input) of
                            (Right result, _) -> do outputStrLn $ show $ result
                                                    newLoop
                            (Left err, "") -> do outputStrLn err
                                                 newLoop
                            (Left _, _) -> loop $ inputSoFar ++ "\n"
        newLoop = loop ""
