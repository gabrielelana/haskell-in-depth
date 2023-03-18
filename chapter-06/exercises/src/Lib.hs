{-# LANGUAGE RankNTypes #-}

module Lib where

-- import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (singleton)
import System.Posix.Unistd (sleep)
import Data.Time.Clock.POSIX
-- import TextShow

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- singleton :: a -> [a]
-- singleton x = [x]

-- 1

-- We want to write a program that manipulates protected data. This means that,
-- in various point of our program, we might ask the user for their password in
-- order to the data. Of course, access to the protected data can fail if the
-- password is wrong.

data ProtectedData a = ProtectedData !String !a

accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData p a)
  | s == p = Just a
  | otherwise = Nothing

-- 1.1

-- Your task is to implement the Protected monad that will ask the password to
-- the user when trying to access the data, and fail in case of error. The data
-- is stored in a reader monad.

type Protected s a = MaybeT (Reader (ProtectedData s)) a

run :: ProtectedData s -> Protected s a -> Maybe a
run p m = runReader (runMaybeT m) p

access :: String -> Protected a a
access = MaybeT . asks . accessData

-- access s = do
  -- a <- MaybeT $ asks $ accessData s
  -- pure a

  -- a <- MaybeT $ asks $ accessData s
  -- pure a
  -- MaybeT $ pure a

  -- a <- lift ask
  -- MaybeT $ pure $ accessData s a


-- 1.2

-- Instead of asking the programmer to enter the password, it would be better to
-- ask the user directly. Improve your Protected monad like so.

type ProtectedIO s a = MaybeT (ReaderT (ProtectedData s) IO) a

runIO :: ProtectedData s -> ProtectedIO s a -> IO (Maybe a)
runIO p m = runReaderT (runMaybeT m) p

accessIO :: ProtectedIO a a
accessIO = liftIO getLine >>= MaybeT . asks . accessData

-- 2

-- We will consider the following types

data Item = Msg !String
          | Section !String ![Item]
          deriving (Show, Eq)

type Log = [Item]

type Logging a = Writer Log a

-- 2.1

-- ‘log s‘ logs the messages ‘s‘.
log :: Show t => t -> Logging ()
log = tell . singleton . Msg . show

-- ‘withSection s m‘ executes m and add its log in a section titled ‘s‘.
withSection :: String -> Logging a -> Logging a
withSection s m = pass $ do
  x <- m
  pure (x, singleton . Section s)

-- withSection s m = pass $ (, Section s) <$> m
-- withSection s m = pass $ fmap (, Section s) m
-- withSection s = censor $ singleton . Section s

runLogging :: Logging a -> (a, Log)
runLogging = runWriter

-- 2.2

-- Sometimes, it is useful to have timestamps in logs. For this purpose, we can
-- use the following haskell functions:
-- getPOSIXTime :: IO POSIXTime

-- Extend the Logging monad to be able to call IO actions. Do you need to change
-- the type of runLogging?

-- Extend Item, log and withSection to always register timestamps.

data ItemTS = MsgTS POSIXTime !String
            | SectionTS POSIXTime POSIXTime !String ![ItemTS]
            deriving (Show, Eq)

type LogTS = [ItemTS]

type LoggingTS a = WriterT LogTS IO a

logTS :: Show t => t -> LoggingTS ()
logTS t = do
  ts <- liftIO getPOSIXTime
  tell [MsgTS ts (show t)]

-- In the case of withSection, you should register two timestamps: one before
-- and one after. Test your implementation to ensure that you record time
-- correctly, by using System.Posix.Unistd.sleep for example.

withSectionTS :: String -> LoggingTS a -> LoggingTS a
withSectionTS s m = pass $ do
  start <- liftIO getPOSIXTime
  x <- m
  stop <- liftIO getPOSIXTime
  return (x, \w -> [SectionTS start stop s w])
-- withSectionTS s m = do
--   start <- liftIO getPOSIXTime
--   (a, w) <- liftIO $ runLoggingTS m
--   stop <- liftIO getPOSIXTime
--   writer (a, [SectionTS start stop s w])

runLoggingTS :: LoggingTS a -> IO (a, LogTS)
runLoggingTS = runWriterT

doSomething :: LoggingTS ()
doSomething = do
  logTS "before doing something"
  _ <- liftIO $ sleep 5
  logTS "after doing something"
  -- pure ()

-- instance TestShow ItemTS where
