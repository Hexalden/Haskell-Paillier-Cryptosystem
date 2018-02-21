# Haskell Paillier Cryptosystem
A simple implementation of the Paillier Cryptosystem in Haskell.

## Information
This is a library providing a full cryptosystem of the Paillier cryptosystem:
 * a key generation function
 * an encryption function
 * a decryption function

The library also provide a function to generate highly probable prime numbers using the Miller-Rabin test.

## How to
Just include PaillierCryptosystem.hs and GenerateMillerRabinPrime.hs in your project to use the cryptosystem.

### Functions available
There is three function available to use the cryptosystem, they are in PaillierCryptosystem.hs:

  - **paillierGenerateKeys**, which take an Int for the size of the keys as argument, and return both the public and private keys.
      *Note that the public key is (n, g) and the private key is (lambda, mu) and not (p, q) the two primes.*
      *For more information, please read the [original paper](http://www.cs.tau.ac.il/~fiat/crypt07/papers/Pai99pai.pdf)*
      
  - **paillierEncryption**, which take the public key and the message to encrypt as arguments, and return the encrypted message.
  
  - **paillierDecryption**, which take the public key, the private key and the encrypted message as arguments, and return the decrypted message.
