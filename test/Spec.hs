{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified RegexEquality
import Regex

equal a b = res == Nothing where
    Right res = RegexEquality.counterexample a b

expectLeft (Left _) = True
expectLeft (Right _) = False

main :: IO ()
main = hspec $ do

    describe "RegexEquality.equal" $ do
        it "works on identical regex" $
            equal "a" "a"

        it "finds difference" $
            not $ equal "a" "b"

        it "works on equivalent regex" $
            equal "a|a" "a"

        it "complicated pair" $
            equal "((a+)*)+" "a*"

    describe "Parsing" $ do
        let a = Character 'a'
        let b = Character 'b'
        let c = Character 'c'


        it "single letter" $
            parse "a" `shouldBe` Right a

        it "multiple letters" $
            parse "bac" `shouldBe` Right (Consecutive [b, a, c])

        it "simple or" $
            parse "a|b" `shouldBe` Right (OneOf [a, b])

        it "or with multiple letters on the sides" $
            parse "ab|bc" `shouldBe` Right (OneOf [Consecutive [a, b], Consecutive [b, c]])

        it "star" $
            parse "a*" `shouldBe` Right (NTimes a)

        it "plus" $
            parse "a+" `shouldBe` Right (OneOrMore a)

        it "single parens" $
            parse "(ab)*" `shouldBe` Right (NTimes $ Consecutive [a, b])

        it "escaped special character" $
            parse "\\*" `shouldBe` Right (Character '*')

        it "fails on unescaped special character" $
            expectLeft $ parse "+"

        it "parses ? in parens" $
            parse "(a+b?)" `shouldBe` Right (Consecutive [OneOrMore a, Maybe b])