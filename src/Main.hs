module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe

main :: IO ()
main = do
  putStrLn "hello world"

myBind :: Monad f => f a -> (a -> f b) -> f b
myBind fa afb = join $ fmap afb fa 

itBindTryOne :: Functor f => IdentityT f a -> (a -> b) -> f b
itBindTryOne (IdentityT fa) ab = fmap (ab) fa

iTBind :: Monad f => IdentityT f a -> (a -> IdentityT f b) -> IdentityT f b
iTBind (IdentityT fa) ab = IdentityT $ join $ fmap (runIdentityT . ab) fa

getName :: MaybeT IO String
getName = do
  input <- liftIO getLine
  if input == ""
    then MaybeT $ return Nothing
    else MaybeT $ return $ Just $ "Name: " ++  input ++ "\n"
  
getNumber :: String -> MaybeT IO String
getNumber str = do
  input <- liftIO getLine
  if (length input) /= 7
    then MaybeT $ return Nothing
    else MaybeT $ return $ Just $ str ++ "Phone Number: " ++ input ++ "\n" 

getStreetName :: String -> MaybeT IO String
getStreetName str = do
  input <- liftIO getLine
  if input == ""
    then MaybeT $ return Nothing
    else MaybeT $ return $ Just $ str ++ "Street Name: " ++ input ++ "\n"

compositionMethod :: IO (Maybe String)
compositionMethod = runMaybeT $ getName >>=
                               getNumber >>=
                               getStreetName

compAlt :: IO (Maybe String)
compAlt = runMaybeT $ do
  a <- getName
  b <- getNumber a
  c <- getStreetName b
  return c

allTogetherNow :: IO ()
allTogetherNow = do
  result <- compositionMethod
  case result of
    Just str -> putStrLn str
    Nothing -> putStrLn "Failure!"

aTNv2 :: IO ()
aTNv2 = do
  result <- compAlt
  case result of
    Just str -> putStrLn str
    Nothing -> putStrLn "Failure!"
