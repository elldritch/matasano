import Test.Hspec (hspec, describe, it, shouldBe, pending, runIO, SpecWith)

import Matasano (solutions)

main :: IO ()
main = do
  s1q4data <- readFile "test/1-4-data"
  hspec $ do
    describe "Set 1: Basics" $ do
      let qs = head solutions
      let q1 = head qs
      it "1. Convert hex to base64" $
        q1 ["49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"] `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

      let q2 = qs !! 1
      it "2. Fixed XOR" $
        q2 ["1c0111001f010100061a024b53535009181c", "686974207468652062756c6c277320657965"] `shouldBe` "746865206b696420646f6e277420706c6179"

      let q3  = qs !! 2
      it "3. Single-byte XOR cipher" $
        q3 ["1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"] `shouldBe` "Cooking MC's like a pound of bacon"

      let q4 = qs !! 3
      it "4. Detect single-character XOR" $
        q4 (lines s1q4data) `shouldBe` "Now that the party is jumping\n"

      let q5 = qs !! 4
      it "5. Implement repeating-key XOR" $
        q5 ["ICE", "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"] `shouldBe` "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

      -- let q6 = qs !! 5
      it "6. Break repeating-key XOR" pending

      -- let q7 = qs !! 6
      it "7. AES in ECB mode" pending

      -- let q8 = qs !! 7
      it "8. Detect AES in ECB mode" pending

    describe "Utility functions" $ it "should be tested" pending
