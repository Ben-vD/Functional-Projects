module FullMatchSpec (spec) where

import Engine (matches)
import Regex (Regex (..))
import Test.Hspec

spec :: Spec
spec =
  describe "Full matching" $ do
    describe "epsilon" $ do
      it "matches empty string" $ do
        matches Epsilon "" `shouldBe` True

      it "does not match non-empty string" $ do
        matches Epsilon "a" `shouldBe` False

    describe "character" $ do
      it "matches character" $ do
        matches (Char 'a') "a" `shouldBe` True

      it "does not match character" $ do
        matches (Char 'a') "b" `shouldBe` False

      it "does not match empty" $ do
        matches (Char 'a') "" `shouldBe` False

    describe "alternation" $ do
      it "matches left" $ do
        matches (Alternation [Char 'a', Char 'b']) "a" `shouldBe` True

      it "matches right" $ do
        matches (Alternation [Char 'a', Char 'b']) "b" `shouldBe` True

      it "matches neither" $ do
        matches (Alternation [Char 'a', Char 'b']) "c" `shouldBe` False

    describe "intersection" $ do
      it "matches both" $ do
        matches (Intersection [Char 'a', Char 'a']) "a" `shouldBe` True

      it "matches left only" $ do
        matches (Intersection [Char 'a', Char 'b']) "a" `shouldBe` False

      it "matches right only" $ do
        matches (Intersection [Char 'a', Char 'b']) "b" `shouldBe` False

    describe "concatenation" $ do
      it "matches sequence" $ do
        matches (Concatenation (Char 'a') (Char 'b')) "ab" `shouldBe` True

      it "does not match sequence" $ do
        matches (Concatenation (Char 'a') (Concatenation (Char 'b') (Char 'c'))) "ab" `shouldBe` False

      it "matches nullable first" $ do
        matches (Concatenation Epsilon (Char 'x')) "x" `shouldBe` True

    describe "negation" $ do
      it "does not match character" $ do
        matches (Negation (Char 'a')) "b" `shouldBe` True

      it "matches not not" $
        do
          matches (Negation (Negation (Alternation [Char 'a', Concatenation (Char 'a') (Char 'b')]))) "ab"
          `shouldBe` matches (Alternation [Char 'a', Concatenation (Char 'a') (Char 'b')]) "ab"

    describe "star" $ do
      it "matches empty" $ do
        matches (Star (Char 'a')) "" `shouldBe` True

      it "matches single" $ do
        matches (Star (Char 'a')) "a" `shouldBe` True

      it "matches many" $ do
        matches (Star (Alternation [Char 'a', Concatenation (Char 'a') (Char 'b')])) "aaaabababaab" `shouldBe` True

      it "does not match" $ do
        matches (Star (Alternation [Char 'a', Concatenation (Char 'a') (Char 'b')])) "aaaabbababaabc" `shouldBe` False

    describe "range" $ do
      it "matches 2-3 a's" $ do
        matches (Range (Char 'a') 2 (Just 3)) "aa" `shouldBe` True

      it "does not match 2-3 a's" $ do
        matches (Range (Char 'a') 2 (Just 3)) "aaaa" `shouldBe` False

      it "matches star" $ do
        matches (Range (Alternation [Char 'a', Concatenation (Char 'b') (Star Any)]) 0 Nothing) "abxyabb"
          `shouldBe` matches (Star (Alternation [Char 'a', Concatenation (Char 'b') (Star Any)])) "abxyabb"
          
--------------------------------------------------
    -- Additional test cases
    describe "epsilon (additional)" $ do
      it "matches nested epsilon" $ do
        matches (Concatenation Epsilon Epsilon) "" `shouldBe` True

      it "does not match epsilon with character" $ do
        matches (Concatenation Epsilon (Char 'a')) "" `shouldBe` False

    describe "character (additional)" $ do
      it "matches special character" $ do
        matches (Char '@') "@" `shouldBe` True

      it "does not match different special character" $ do
        matches (Char '@') "#" `shouldBe` False

    describe "alternation (additional)" $ do
      it "matches with longer alternation list" $ do
        matches (Alternation [Char 'a', Char 'b', Char 'c', Char 'd']) "c" `shouldBe` True

      it "does not match with longer alternation list" $ do
        matches (Alternation [Char 'a', Char 'b', Char 'c', Char 'd']) "e" `shouldBe` False

    describe "intersection (additional)" $ do
      it "matches complex intersection" $ do
        matches (Intersection [Alternation [Char 'a', Char 'b'], Alternation [Char 'b', Char 'c']]) "b" `shouldBe` True

      it "does not match complex intersection" $ do
        matches (Intersection [Alternation [Char 'a', Char 'b'], Alternation [Char 'c', Char 'd']]) "b" `shouldBe` False

    describe "concatenation (additional)" $ do
      it "matches longer sequence" $ do
        matches (Concatenation (Char 'a') (Concatenation (Char 'b') (Concatenation (Char 'c') (Char 'd')))) "abcd" `shouldBe` True

      it "does not match longer sequence" $ do
        matches (Concatenation (Char 'a') (Concatenation (Char 'b') (Concatenation (Char 'c') (Char 'd')))) "abc" `shouldBe` False

    describe "negation (additional)" $ do
      it "matches negation of sequence" $ do
        matches (Negation (Concatenation (Char 'a') (Char 'b'))) "ac" `shouldBe` True

      it "does not match negation of sequence" $ do
        matches (Negation (Concatenation (Char 'a') (Char 'b'))) "ab" `shouldBe` False

    describe "star (additional)" $ do
      it "matches empty with complex star" $ do
        matches (Star (Concatenation (Char 'a') (Char 'b'))) "" `shouldBe` True

      it "matches multiple repetitions with complex star" $ do
        matches (Star (Concatenation (Char 'a') (Char 'b'))) "ababab" `shouldBe` True

      it "does not match with complex star" $ do
        matches (Star (Concatenation (Char 'a') (Char 'b'))) "ababac" `shouldBe` False

    describe "range (additional)" $ do
      it "matches exact range" $ do
        matches (Range (Char 'a') 3 (Just 3)) "aaa" `shouldBe` True

      it "does not match exact range" $ do
        matches (Range (Char 'a') 3 (Just 3)) "aa" `shouldBe` False

      it "matches minimum range" $ do
        matches (Range (Char 'b') 1 Nothing) "bbbb" `shouldBe` True

      it "does not match minimum range" $ do
        matches (Range (Char 'b') 1 Nothing) "" `shouldBe` False