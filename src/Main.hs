{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

import Data.Loc.Pos
import Data.Loc.Loc
import Data.Loc.Span

import Data.List


spans :: [Span]
spans = [
  fromTo (loc (fromInteger 1) (fromInteger 1)) (loc (fromInteger 1) (fromInteger 42))
  , fromTo (loc (fromInteger 1) (fromInteger 2)) (loc (fromInteger 1) (fromInteger 3))
  ]

main :: IO ()
main = do
  -- putStrLn $ show $ sort spans
  -- result <- fnTarget helpersIO "aaa"
  result <- fnTarget helpersIO "Question"
  print result

ask :: String -> IO String
ask question = do 
  print question
  answer <- getLine
  return $ question ++ " " ++ answer


-- data Helpers a m where
--   HelpersIO :: Monad m => (a -> m a)
-- }

data Helpers a m where
  MonadicHelpers :: Monad m =>
                      { fn1 :: (a -> m a)
                      , fn2 :: (a -> m a)
                      } -> Helpers a m

helpersIO = MonadicHelpers
            { fn1 = ask
            , fn2 = ask . reverse
            }

-- data Helpers a m = Helpers {
--   fn1 :: (a -> m a)
-- }

-- helpersIO = Helpers {
--   fn1 = ask
-- }


fnTarget :: Monad m => Helpers String m -> String -> m String
fnTarget helpers q = do
  result1 <- (fn1 helpers) q
  result2 <- (fn2 helpers) q
  return $ result1 ++ " " ++ result2



permutate :: (Eq a) => [a] -> [[a]]
permutate [] = [[]]
permutate l  = [a:x | a <- l, x <- permutate (filter ((/=) a) l)]
  
  