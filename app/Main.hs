module Main
  ( main
  ) where

import           Control.Exception           (bracketOnError)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Set                    (fromList)
import           HW3.Action                  (HiPermission (..), runHIO)
import           HW3.Evaluator               (eval)
import           HW3.Parser                  (parse)
import           HW3.Pretty                  (prettyValue)
import           System.Console.Haskeline    (defaultSettings, getInputLine,
                                              outputStrLn)
import           System.Console.Haskeline.IO (InputState, cancelInput,
                                              closeInput, initializeInput,
                                              queryInput)
import           Text.Megaparsec.Error       (errorBundlePretty)

main :: IO ()
main =
  bracketOnError
    (initializeInput defaultSettings)
    cancelInput
    (\hd -> loop hd >> closeInput hd)
  where
    loop :: InputState -> IO ()
    loop hd = do
      minput <- queryInput hd (getInputLine "hi> ")
      case minput of
        Nothing -> return ()
        Just input -> case parse input of
          Left errorParse -> queryInput hd $ outputStrLn $ errorBundlePretty errorParse
          Right expr -> do
            value <- liftIO $ runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
            queryInput hd $
              outputStrLn $ case value of
                Left errorEval -> show errorEval
                Right val      -> show $ prettyValue val
      loop hd
