import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta

import Ini

-- Natural transformation from Result to Maybe
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main =
  hspec $ do
    describe "Assignment Parsing" $
      it "can parse a simple assignment" $ do
        let m = parseString assignment mempty "woot=blah"
        (maybeSuccess m) `shouldBe` Just ("woot", "blah")
    describe "Header Parsing" $
      it "can parse a simple header" $ do
        let m = parseString header mempty "[header]"
        (maybeSuccess m) `shouldBe` Just (Header "header")
    describe "Comment parsing" $ do
      it "Can skip comment before a header" $ do
        let p = skipComments >> header
            i = ";woot\n[blah]"
            m = parseString p mempty i
        (maybeSuccess m) `shouldBe` Just (Header "blah")


