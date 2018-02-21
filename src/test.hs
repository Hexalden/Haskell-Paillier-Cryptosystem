import Data.Maybe
import System.Exit
import Control.Monad

import GenerateMillerRabinPrime
import PaillierCryptosystem

main :: IO ()
main = do
  let size = 2048
  putStrLn ("Key size: " ++ show size)

  --((lambda, mu), (n, g)) <- paillierGenerateKeys 512
  keys <- paillierGenerateKeys size
  when (keys == Nothing) $ die "Error during key generation !"

  let ((lambda, mu), (n, g)) = fromJust keys

  putStrLn ("lambda: " ++ show lambda)
  putStrLn ""

  putStrLn ("mu: " ++ show mu)
  putStrLn ""

  putStrLn ("n: " ++ show n)
  putStrLn ""

  putStrLn ("g: " ++ show g)
  putStrLn ""
  putStrLn ""

  messageToEncrypt <- getRandomInteger (1000 `div` 8)
  putStrLn ("message to encrypt: " ++ show messageToEncrypt)
  putStrLn ""


  encryptedMessage <- paillierEncryption (n, g) messageToEncrypt
  putStrLn ("encrypted message: " ++ show encryptedMessage)
  putStrLn ""


  let decryptedMessage = paillierDecryption (n, g) (lambda, mu) $ fromJust encryptedMessage
  putStrLn ("decrypted message: " ++ show decryptedMessage)
  putStrLn ""


  putStrLn ("Is decrypted message same as initial message ?")
  putStrLn $ show $ messageToEncrypt == fromJust decryptedMessage
