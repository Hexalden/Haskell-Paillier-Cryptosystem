{- |
Module      :  generateMillerRabinPrime
Description :  This program contain functions permiting to generate a number which
                validate a Miller-Rabin primality test.
Copyright   :  (c) Franck Metier
License     :  MIT

Maintainer  :  quadrique.sec@gmail.com
Stability   :  experimental
Portability :  portable

-}

module GenerateMillerRabinPrime
( getMillerRabinProbablePrime
, getRandomInteger
) where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import Data.Bits
import System.Environment
import System.Random
import System.Entropy



-- | Generate a random number which validate a Miller Rabin primality test.
-- This function is IO because the random bytes are generated from the system
-- source of entropy (which cause the code to be impure).
getMillerRabinProbablePrime :: Int          -- ^ Size in bytes of the random number to generate
                            -> IO Integer   -- ^ Probable random prime
getMillerRabinProbablePrime size = do
    randomNumber <- getRandomInteger $ size `div` 8
    if isTrivialPrime randomNumber
        then getMillerRabinProbablePrime size
        else  if isTrivialMillerRabinPrime randomNumber
                  then return randomNumber
                  else getMillerRabinProbablePrime size



-- | Generate a random integer of the size in argument.
getRandomInteger :: Int -> IO Integer
getRandomInteger size = do
    randomBytes <- getEntropy size
    return $ BS.foldl' f 0 randomBytes
  where
    f a b = a `shiftL` 8 .|. fromIntegral b


-- |Test if a number is a trivial prime.
isTrivialPrime :: Integer -> Bool
isTrivialPrime number = foldl (\acc x -> acc || (number `rem` x) == 0) False listOfPrimes
  where
    listOfPrimes = [2] --, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131]


-- | Test if a number is a trivial Miller Rabin Witness.
isTrivialMillerRabinPrime :: Integer -> Bool
isTrivialMillerRabinPrime n
    | isTrivialWitness    = False
    | otherwise           = True
  where
    listOfPrimes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131]
    isTrivialWitness = foldl (\acc x -> acc || (isMillerRabinWitnessStep1 n) x) False listOfPrimes


-- | Test if a number is a Miller Rabin witness of a tested number.
isMillerRabinWitnessStep1 :: Integer -> Integer -> Bool
isMillerRabinWitnessStep1 number witnessTested
    | witnessTested > 1 && number >= 3
                              = if x == 1 || x == number - 1
                                    then False
                                    else isMillerRabinWitnessStep2 number (s, d) witnessTested
    | otherwise               = False
  where
    (s, d) = computeSandD number 1
    x = fastModularExpo witnessTested d number


-- | This function compute S and D for the Miller Rabin Witness test.
-- We need to find s and d as (n - 1) = (2 ^ s) * d
computeSandD :: Integer -> Integer -> (Integer, Integer)
computeSandD n s =  if remainder == 0
                        then (s, d)
                        else computeSandD n (s + 1)
  where
    (quotient, remainder) = quotRem (n - 1) (2 ^ s)
    d = quotient


-- | This function compute the square of a nulber modulus a number n until
-- the result is n - 1 or until we have done a certain number of tests
isMillerRabinWitnessStep2 :: Integer -> (Integer, Integer) -> Integer -> Bool
isMillerRabinWitnessStep2 n (s, d) witnessTested
    | s > 1     = if x == n - 1
                      then False
                      else isMillerRabinWitnessStep2 n ((s - 1),d) witnessTested
    | otherwise = True
  where
    x = fastModularExpo witnessTested ((2 ^ s) * d) n


-- | This function compute a fast modular exponentiation
fastModularExpo :: Integer -> Integer -> Integer -> Integer
fastModularExpo base 1 modulo = mod base modulo
fastModularExpo base power modulo
    | even power = mod ((fastModularExpo base (power `div` 2) modulo) ^ 2) modulo
    | odd  power = mod ((fastModularExpo base ((power - 1) `div` 2) modulo) ^ 2 * base) modulo
