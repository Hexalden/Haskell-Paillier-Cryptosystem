{- |
Module      :  Paillier Cryptosystem
Description :  This program contain functions permiting to generate Paillier cryptosystem keys,
                encrypt a message, and decrypt an encrypted message.
Copyright   :  (c) Franck Metier
License     :  MIT

Maintainer  :  quadrique.sec@gmail.com
Stability   :  experimental
Portability :  portable


This source code provide functions to create a full Paillier cryptosystem:
  -> Key generation
  -> Encryption
  -> Decryption

The Key generation function use the GenerateMillerRabinPrime code to
generate cryptographicaly secure random and highly probable prime numbers.

However, another mean could be used by only changing the corresponding lines.
-}

module PaillierCryptosystem
( paillierGenerateKeys
, paillierEncryption
, paillierDecryption
) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Binary.Get
import System.Environment
import System.Random
import System.Entropy
import qualified Data.ByteString as BS
import Data.Bits

import GenerateMillerRabinPrime

------ Exported functions

-- | This function generate the pair of keys for the Paillier Cryptosystem.
-- The first pair is the private key : (lambda, mu)
-- The second pair is the public key : (n, g)
paillierGenerateKeys :: Int   -- size in bites, limited to 8192 for performance reasons
                        -> IO (Maybe ((Integer, Integer),   -- (lambda, mu) the private key
                                      (Integer, Integer)))  -- (n, g) the public key
paillierGenerateKeys size
    | size <= 0   = return Nothing
    | size > 8192 = return Nothing
    | otherwise   = do
                        p <- getMillerRabinProbablePrime size
                        q <- getMillerRabinProbablePrime size
                        let
                            n = p * q
                            lambda = lcm (p - 1) (q - 1)
                        (g, mu) <- paillierSelectGandMu lambda n
                        return $ Just ((lambda, mu), (n, g))

-- | This function encrypt a message using the public key.
paillierEncryption :: (Integer, Integer)    -- public key
                      -> Integer            -- message
                      -> IO (Maybe Integer) -- IO for random, Maybe if problem with message size or key
paillierEncryption (n, g) m
    | n <= 0  ||  g <= 0  = return Nothing
    | m >= n              = return Nothing
    | otherwise           = do
                                newStdGen
                                gen <- getStdGen
                                let (randomNumber, _) = randomR (1, n) gen
                                return $ Just $ (((fastModularExpo g m (n ^ 2)) * (fastModularExpo randomNumber n (n ^ 2))) `mod` (n ^ 2))


-- | This function decrypt an encrypted message using both the public and private keys.
paillierDecryption :: (Integer, Integer)      -- public key
                      -> (Integer, Integer)   -- private key
                      -> Integer              -- encrypted message
                      -> Maybe Integer        -- Maybe if problem with message size or key
paillierDecryption (n, g) (lambda, mu) c
    | n <= 0  ||  g <= 0    = Nothing
    | c >= n ^ 2            = Nothing
    | otherwise             = Just $ mod (numerator * mu) n
  where
    numerator = (u - 1) `div` n
    u = fastModularExpo c lambda (n ^ 2)



------ Non exported functions

-- | This function is used during the key pair generation, in order to compute
-- the public half key g and the corresponding private half key mu.
paillierSelectGandMu :: Integer -> Integer -> IO (Integer, Integer)
paillierSelectGandMu lambda n
  = do
        newStdGen
        gen <- getStdGen
        let
          (g, _) = randomR (1, n ^ 2) $ gen
          a = ((fastModularExpo g lambda (n ^ 2)) - 1) `div` n
          mu = modularMultiplicativeInverse a n
        if gcd a n == 1
            then  if mu == Nothing
                      then paillierSelectGandMu lambda n
                      else return (g, fromJust mu)
            else paillierSelectGandMu lambda n


-- | Thus function compute an extended Greatest Common Divisor using the extended euclidean algorithm.
-- The function return the greatest common divisor of the two integer a and b in argument, but also the
-- coefficients of Bézout's identity s and t.
extendedEuclideanAlgo :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclideanAlgo a 0 = (a, 1, 0)
extendedEuclideanAlgo a b = (g, t, s - quotient * t)
  where
    (quotient, remain) = quotRem a b
    (g, s, t) = extendedEuclideanAlgo b remain


-- | This function compute a modular multiplicative inverse.
modularMultiplicativeInverse :: Integer -> Integer -> Maybe Integer
modularMultiplicativeInverse toInverse modular =
    if g == 1
        then  if s < 0
                  then Just (s + modular)
                  else Just s
        else Nothing
  where
    (g, s, _) = extendedEuclideanAlgo toInverse modular


-- | This function compute a fast modular exponentiation
fastModularExpo :: Integer -> Integer -> Integer -> Integer
fastModularExpo base 1 modulo = mod base modulo
fastModularExpo base power modulo
  | even power = mod ((fastModularExpo base (power `div` 2) modulo) ^ 2) modulo
  | odd  power = mod ((fastModularExpo base ((power - 1) `div` 2) modulo) ^ 2 * base) modulo


-- | Generate a random integer of the size in argument.
getRandomInteger :: Int -> IO Integer
getRandomInteger size = do
                            randomBytes <- getEntropy size
                            return $ BS.foldl' f 0 randomBytes
  where
    f a b = a `shiftL` 8 .|. fromIntegral b
