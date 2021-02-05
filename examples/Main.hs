{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- deserialisePEST,

import qualified Codec.CBOR.Decoding
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Concurrent (threadDelay)
import Control.Pest.Par
  ( PEST,
    deserialisePEST,
    pest,
    unPest,
    unsafeSerialisePEST,
  )
import Data.Foldable (for_)

main :: IO ()
main = do
  putStrLn "Starting..."
  putStrLn "Naive eager"
  exampleTask (expensiveTask 2) id
  putStrLn "Naive lazy"
  exampleTask (\i () -> expensiveTask 1 i) (\a -> a ())
  putStrLn "Par PEST"
  exampleTask (pest (static expensiveTask3)) unPest
  putStrLn "Par PEST suspended early"

expensiveTask3 :: Integer -> Integer
expensiveTask3 = expensiveTask 3

expensiveTask4 :: Integer -> Integer
expensiveTask4 = expensiveTask 4

expensiveTask :: Integer -> Integer -> Integer
expensiveTask base n | n <= 1 = base
expensiveTask base n = expensiveTask base (n - 1) + expensiveTask base (n -2)

exampleTask :: (Show b, Num t1) => (t1 -> t2) -> (t2 -> b) -> IO ()
exampleTask spark collect = do
  let !res = spark 40
  for_ [0 .. 10] $ \n -> do
    print n
    threadDelay 1000000
  print ("Result", length (show $ collect res))

parPestSuspended :: IO ()
parPestSuspended = do
  let !res = pest (static expensiveTask4) 40
  putStrLn "Storing PEST"
  let stored = toLazyByteString $ unsafeSerialisePEST res
  putStrLn "Restoring PEST"
  let (Right (_, !res2)) =
        deserialiseFromBytes (deserialisePEST @Integer @Integer) stored
  print ("Result", length (show $ unPest res2))
