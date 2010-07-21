module ConcurrentUsers
  ( UserSet
  , UserToken
  , UserCount
  , newUserSet
  , userCount
  , trackNewUser
  , trackUser
  , untrackUser
  , isUserTracked
  , tokenToString
  , tokenFromString
  , reapStaleUsers
  )
  where

import           Control.Concurrent.STM
import           Control.Monad (liftM, when)
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V1
import           Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe

type UserCount = Int

newtype UserToken = UT { uuid :: UUID } deriving (Eq, Ord)

type AccessMap = Map UserToken UTCTime

newtype UserSet = UserSet (TVar AccessMap)

newUserSet :: IO UserSet
newUserSet = liftM UserSet (newTVarIO Map.empty)

userCount :: UserSet -> IO UserCount
userCount (UserSet tvar) = liftM Map.size (readTVarIO tvar)

isUserTracked :: UserToken -> UserSet -> IO Bool
isUserTracked token (UserSet tvar) = liftM (Map.member token) (readTVarIO tvar)

trackNewUser :: UserSet -> IO (Maybe UserToken)
trackNewUser userSet = do
  possibleToken <- liftM (liftM UT) nextUUID

  when (isJust possibleToken) $
    trackUser (fromJust possibleToken) userSet

  return possibleToken

trackUser :: UserToken -> UserSet -> IO ()
trackUser token userSet = do
  now <- getCurrentTime
  modify (Map.insert token now) userSet

untrackUser :: UserToken -> UserSet -> IO ()
untrackUser token userSet = modify (Map.delete token) userSet

reapStaleUsers :: DiffTime -> UserSet -> IO ()
reapStaleUsers diffTime userSet = do
  now <- getCurrentTime
  let nominalDiffTime = fromRational . toRational $ diffTime
      staleBoundary = (- nominalDiffTime) `addUTCTime` now
  modify (Map.filter (> staleBoundary)) userSet

modify :: (AccessMap -> AccessMap) -> UserSet -> IO ()
modify f (UserSet tvar) = atomically $ do
  set <- readTVar tvar
  writeTVar tvar (f set)

tokenToString :: UserToken -> ByteString
tokenToString = Char8.pack . show . uuid

tokenFromString :: ByteString -> Maybe UserToken
tokenFromString = (liftM UT) . fromString . Char8.unpack

