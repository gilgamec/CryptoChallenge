-- Test for Challenges 47 and 48.
-- Decrypts RSA-encrypted data via a padding oracle,
-- with "Hollywood-style" updating.

module Main where

import Bytes ( HasBytes(..), convBytes )
import Padding.PKCS1 ( padPKCS1, validatePKCS1 )
import PublicKey ( KeyPair(..), publicPart )
import PublicKey.RSA ( genRSAKeyPair, rsaBlockSize, cryptRSA )

import Challenge48 ( pkcs1Oracle, bb98Attack )

import Data.Char ( isPrint )
import Data.Foldable ( traverse_ )
import Data.Maybe ( fromJust )

-- Printable version of bytes, where nonprintable characters
-- are replaced by dots.
showChars :: HasBytes text => text -> String
showChars = map mkPrintable . convBytes
 where
  mkPrintable ch
    | isPrint ch = ch
    | otherwise = '.'

test :: HasBytes plaintext => plaintext -> IO ()
test plaintext = do
  -- Find the RSA modulus bitsize needed to hold the cipher.
  -- Padding is at least 11 bytes long (00 02 <8 random bytes> 00).
  let bitSize = 8 * (11 + numBytes plaintext)
  kp <- genRSAKeyPair bitSize
  putStrLn $ "Generated "++show bitSize++"-bit RSA key pair"

  -- Pad and encrypt the message.
  let pubKey = publicPart kp
      blockSize = rsaBlockSize pubKey
  padded <- fromJust <$> padPKCS1 2 blockSize plaintext
  let ctext = cryptRSA (kpPublic kp) padded

  -- Decrypt the message with the oracle and print out each upper bound.
  let oracle = pkcs1Oracle kp
      decrypts = bb98Attack pubKey oracle ctext
  traverse_ (putStrLn . showChars) decrypts

  if validatePKCS1 blockSize (last decrypts) == Just (convBytes plaintext)
    then putStrLn "Successfully decrypted!"
    else putStrLn "Failed to decrypt!"

main = do
  putStrLn "Test for Challenge 47"
  test "kick it, CC"

  putStrLn "\n"

  putStrLn "Test for Challenge 48"
  test $ "I'm rated \"R\"...this is a warning, ya better void / "++
         "Poets are paranoid, DJ's D-stroyed"
