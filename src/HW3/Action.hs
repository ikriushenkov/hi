module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  )
where

import           Control.Exception.Base (Exception, throwIO)
import qualified Data.ByteString        as B
import           Data.Set               (Set)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8')
import qualified Data.Text.IO           as TIO
import           Data.Time.Clock.POSIX  (getCurrentTime)
import           HW3.Base               (HiAction (..), HiMonad, HiValue,
                                         runAction)
import           HW3.Evaluator          (ToValue, toValue)
import           System.Directory       (createDirectory, doesDirectoryExist,
                                         getCurrentDirectory, listDirectory,
                                         setCurrentDirectory)
import           System.Random.Stateful (getStdRandom, randomR)

-- | Permission for action
data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Exception that is thrown during the execution of the action
newtype PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

-- | IO for Hi to execute actions with permissions
newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}

instance Functor HIO where
  fmap f (HIO g) = HIO (fmap f . g)

instance Applicative HIO where
  pure x = HIO (\_ -> return x)
  (<*>) (HIO f) (HIO x) = HIO (\p -> f p <*> x p)

instance Monad HIO where
  (>>=) (HIO x) f = HIO $ \p -> do
    value <- x p
    runHIO (f value) p

runActionWithPermission :: ToValue a => HiPermission -> IO a -> HIO HiValue
runActionWithPermission p value = HIO $ \ps ->
  if p `elem` ps
    then toValue <$> value
    else throwIO $ PermissionRequired p

runActionWithoutPermissions :: ToValue a => IO a -> HIO HiValue
runActionWithoutPermissions value = HIO $ \_ -> toValue <$> value

instance HiMonad HIO where
  runAction HiActionCwd = runActionWithPermission AllowRead (T.pack <$> getCurrentDirectory)
  runAction (HiActionChDir dir) = runActionWithPermission AllowRead (setCurrentDirectory dir)
  runAction (HiActionRead path) =
    runActionWithPermission
      AllowRead
      ( do
          isDir <- doesDirectoryExist path
          if isDir
            then toValue <$> (map T.pack <$> listDirectory path)
            else do
              content <- B.readFile path
              return $ case decodeUtf8' content of
                Left _    -> toValue content
                Right str -> toValue str
      )
  runAction (HiActionMkDir dir) = runActionWithPermission AllowWrite (createDirectory dir)
  runAction (HiActionWrite path str) = runActionWithPermission AllowWrite (B.writeFile path str)
  runAction HiActionNow = runActionWithPermission AllowTime getCurrentTime
  runAction (HiActionRand from to) = runActionWithoutPermissions (getStdRandom (randomR (from, to)))
  runAction (HiActionEcho str) = runActionWithPermission AllowWrite (TIO.putStrLn str)
