import Test.Hspec
import Codec.Multipath
import Data.ByteString

main :: IO ()
main = hspec $ do
    describe "Codec.Multipath" $ do
        describe "fromString" $ do
            it "parses strings" $ do
                expectParse "" []
                expectParse "/" [""]
                expectParse "/a/\\/b/µ\\\\" ["a", "/b", "µ\\"]
                expectParse "//a/ //" ["", "a", " ", "", ""]
            it "raises errors when it cannot parse string" $ do
                expectFailedParse "a/b/c"
        describe "Show instance" $ do
            it "converts multipath to string" $ do
                (show $ Multipath ["a", "/b", "µ\\"]) `shouldBe` "/a/\\/b/µ\\\\"
        describe "toByteString" $ do
            it "serialzes multipath" $ do
                toByteString (Multipath ["a", "µ"]) `shouldBe` pack [47, 97, 47, 194, 181]
        describe "fromByteString" $ do
            it "deserialzes multipath" $ do
                fromByteString (pack [47, 97, 47, 194, 181]) `shouldBe` Right (Multipath ["a", "µ"])

expectParse s m = fromString s `shouldBe` (Right $ Multipath m)

expectFailedParse s = case fromString s of
    Right _ -> expectationFailure $ "parse \"" ++ s ++ "\" should fail"
    Left _ -> return ()
