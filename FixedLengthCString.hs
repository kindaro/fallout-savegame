
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FixedLengthCString (FixedLengthCString) where

import           Data.ByteString hiding (length)
import qualified Data.ByteString as ByteString
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Serialize
import           Data.String
import           GHC.TypeLits
import           Prelude         hiding (length)
import           Test.QuickCheck

newtype FixedLengthCString (n :: Nat) = FixedLengthCString { str :: ByteString }
    deriving Eq

getProxy :: KnownNat n => FixedLengthCString n -> Proxy n
getProxy _ = Proxy

length :: KnownNat n => FixedLengthCString n -> Int
length = fromInteger . natVal . getProxy

toByteString :: KnownNat n => FixedLengthCString n -> ByteString
toByteString t@(FixedLengthCString s) =
    let n = length t
        s' = ByteString.take (n - 1) s
    in  s' <> ByteString.replicate (n - ByteString.length s') 0

fromByteString :: forall n. KnownNat n => ByteString -> Maybe (FixedLengthCString n)
fromByteString s | ByteString.length s >= n = Nothing
                 | otherwise = Just . FixedLengthCString . fst . spanEnd (== 0) $ s
  where n = fromInteger . natVal $ (Proxy :: Proxy n)

instance KnownNat n => Read (FixedLengthCString n) where
    readsPrec _ = \s -> case fromByteString . fromString . read $ s of
        Nothing -> [ ]
        Just t  -> [(t, "")]

instance KnownNat n => Show (FixedLengthCString n) where
    show = show . ByteString.init . toByteString

instance forall n. KnownNat n => Serialize (FixedLengthCString n) where
    get = do
        bytes <- getBytes n
        case fromByteString bytes of
            Nothing -> fail "FixedLengthCString.get: Inconsistent length. This should not happen."
            Just t  -> return t
      where n = fromInteger . natVal $ (Proxy :: Proxy n)

    put = put . toByteString

instance KnownNat n => Arbitrary (FixedLengthCString n) where
    arbitrary = do
        m <- choose (0, n - 1)
        s <- vector m :: Gen String
        let e = error "FixedLengthCString.arbitrary: Inconsistent length. \
                                \This should not happen."
            bs = fromMaybe e (fromByteString . fromString $ s)
        return bs
      where n = fromInteger . natVal $ (Proxy :: Proxy n)
