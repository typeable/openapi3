module SpecCommon where

import Data.Aeson
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.Typeable
import qualified Data.Vector as Vector

import Test.Hspec
import Test.QuickCheck (Arbitrary, property)

isSubJSON :: Value -> Value -> Bool
isSubJSON Null _ = True
isSubJSON (Object x) (Object y) = HashMap.keys x == HashMap.keys i && F.and i
  where
    i = HashMap.intersectionWith isSubJSON x y
isSubJSON (Array xs) (Array ys) = Vector.length xs == Vector.length ys && F.and (Vector.zipWith isSubJSON xs ys)
isSubJSON x y = x == y

(<=>) :: (Eq a, Show a, ToJSON a, FromJSON a, HasCallStack) => a -> Value -> Spec
x <=> js = do
  it "encodes correctly" $ do
    toJSON x `shouldBe` js
  it "decodes correctly" $ do
    fromJSON js `shouldBe` Success x
  it "roundtrips: eitherDecode . encode" $ do
    eitherDecode (encode x) `shouldBe` Right x
  it "roundtrips with toJSON" $ do
    eitherDecode (encode $ toJSON x) `shouldBe` Right x
  it "roundtrips with toEncoding" $ do
    eitherDecode (toLazyByteString $ fromEncoding $ toEncoding x) `shouldBe` Right x

testJsonIdempotency :: (Eq a, Show a, Arbitrary a, FromJSON a, ToJSON a, Typeable a, HasCallStack) =>
  Proxy a -> Spec
testJsonIdempotency px = describe (show ty) $ do
  it "roundtrips: decode . encode" $ property $ \x ->
    decode (encode x) == Just (x `asProxyTypeOf` px)
  where
    ty = typeOf $ undefined `asProxyTypeOf` px
