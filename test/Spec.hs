{-# LANGUAGE ScopedTypeVariables #-}

module Spec where

import           Data.AEq
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           TNorms

newtype Norm = Norm { unNorm :: Double } deriving ( Show, Eq, Ord )

instance Num Norm where
  (Norm a) + (Norm b) = Norm (a + b)
  (Norm a) * (Norm b) = Norm (a * b)
  abs = Norm . abs . unNorm
  signum = Norm . signum . unNorm
  negate = Norm . negate . unNorm
  fromInteger = Norm . fromInteger

instance Fractional Norm where
    fromRational = Norm . fromRational
    recip = Norm . recip . unNorm

instance AEq Norm where
  (Norm a) ~== (Norm b) = a ~== b

instance Arbitrary Norm where
  arbitrary = Norm <$> choose (0, 1)

tNormPropertiesTest :: (Norm -> Norm -> Norm) -> Test
tNormPropertiesTest t = testGroup "t-norm properties"
  [ testProperty "Commutativity" $ \(a :: Norm) b ->
      t a b ~== t b a

  , testProperty "Monotonicity" $ \(c :: Norm) d ->
      forAll (arbitrary `suchThat` (<= c))  $ \a ->
      forAll (arbitrary `suchThat` (<= d))  $ \b ->
      t a b <= t c d

  , testProperty "Associativity" $ \(a :: Norm) b c ->
      t a (t b c) ~== t (t a b) c

  , testProperty "Identity" $ \(a :: Norm) ->
      t a (Norm 1) ~== a
  ]

main :: IO ()
main = defaultMain
  [ testGroup "Lukasiewicz t-norm" [ tNormPropertiesTest luk ]
  , testGroup "Minimum t-norm" [ tNormPropertiesTest min ]
  , testGroup "Product t-norm" [ tNormPropertiesTest prod ]
  , testGroup "Drastic t-norm" [ tNormPropertiesTest drast ]
  , testGroup "Nilpotent minimum" [ tNormPropertiesTest nilpotent ]
  , testGroup "Hamacher product" [ tNormPropertiesTest hamacher ]
  ]
