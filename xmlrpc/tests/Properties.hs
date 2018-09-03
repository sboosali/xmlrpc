{-# LANGUAGE OverloadedStrings #-}


main = return ()

{-

--------------------------------------------------
--------------------------------------------------

import Network.XmlRpc.Internals

import System.Time

import qualified Data.ByteString      as SB
import qualified Data.ByteString.Lazy as LB

import "string-conv" Data.String.Conv


import "mtl" Control.Monad.Except

import Control.Monad
import Control.Monad.Identity
import Data.Char
import Data.List
import Data.Maybe
import Data.Foldable
import Data.String
import Prelude

import Test.QuickCheck
--import Tasty.Test.QuickCheck

--------------------------------------------------
--------------------------------------------------

main = do

  sequence_
    [ quickCheck prop_CallParseRender
    , quickCheck prop_ResponseParseRender
    ]

--------------------------------------------------

prop_CallParseRender :: MethodCall -> Bool
prop_CallParseRender expected = runExcept  go

  where

  rendered = s (renderCall expected)

  s :: LB.ByteString -> String
  s = toS

  compare :: MethodCall -> Err Identity Bool
  compare actual = return (actual == expected)

  go = (parseCall rendered >>= compare) `catchError` (\_ -> return False)

  -- go :: Except String Bool -> Bool
  -- go m = either2bool (runExcept m)

  either2bool :: Either e Bool -> Bool
  either2bool = either (const False) id

--------------------------------------------------

prop_ResponseParseRender :: MethodResponse -> Bool
prop_ResponseParseRender r = testErr $
  (==r) <$> (parseResponse (s (renderResponse r)))
  where
  s :: LB.ByteString -> String
  s = toS

--------------------------------------------------

{-
xmlRpcId1 :: (XmlRpcType a, XmlRpcType b, Eq b) => (a -> b) -> a -> Bool
xmlRpcId1 f x = f x == f x'
    where MethodCall _ [x'] $ parseCall $ renderCall $ MethodCall "test" [toValue x]
-}

--------------------------------------------------

liftArb :: Arbitrary a => (a -> b) -> Gen b
liftArb f = liftM f arbitrary

liftArb2 :: (Arbitrary a,Arbitrary b) => (a -> b -> c) -> Gen c
liftArb2 f = liftM2 f arbitrary arbitrary

arbitraryList :: Gen a -> Int -> Gen [a]
arbitraryList g 0 = liftM (:[]) g
arbitraryList g n = oneof [liftM (:[]) g, liftM2 (:) g (arbitraryList g (n-1))]

--------------------------------------------------

instance Arbitrary Month where
    arbitrary = oneof (map return months)

instance CoArbitrary Month where
    coarbitrary m = variant (fromJust (findIndex (==m) months))

months = [January, February, March, 
    April, May, June,
    July, August, September, 
    October, November, December]

--------------------------------------------------

instance Arbitrary CalendarTime where
  arbitrary = do
    y <- arbitrary
    mo <- arbitrary
    d <- arbitrary
    h <- arbitrary
    mi <- arbitrary
    s <- arbitrary
    p <- arbitrary
    return CalendarTime {
             ctYear = abs y,
             ctMonth = mo,
             ctDay = abs d,
             ctHour = abs h,
             ctMin = abs mi,
             ctSec = abs s,
             ctPicosec = abs p,
             -- don't care about these:
             ctWDay = Monday,
             ctYDay = 0,
             ctTZName = "UTC",
             ctTZ = 0,
             ctIsDST = False
            }


arbitraryId :: Gen String
arbitraryId = sized (arbitraryList (elements nameChars))
    where nameChars = ['a'..'z'] ++ ['0'..'9'] ++ "_.:/"

arbitraryPrintableString :: Gen String
arbitraryPrintableString = sized (arbitraryList (elements printableChars))
    where printableChars = filter (not . isControl) ['\0'..'\255']

arbitraryString :: Gen String
arbitraryString = sized (arbitraryList (elements ['\0'..'\255']))

arbitraryPrintableByteString :: Gen SB.ByteString
arbitraryPrintableByteString = fromString <$> arbitraryPrintableString

arbitraryByteString :: Gen SB.ByteString
arbitraryByteString = fromString <$> arbitraryString

--------------------------------------------------

instance Arbitrary MethodCall where
    arbitrary = liftM2 MethodCall arbitraryId arbitrary

--------------------------------------------------

instance Arbitrary MethodResponse where
    arbitrary = oneof [
           liftArb Return
          , liftM2 Fault arbitrary arbitraryPrintableString
          ]

--------------------------------------------------

instance Arbitrary Value where
  arbitrary = sized arbValue
    where
    arbValue n = oneof 
      [ liftArb ValueInt
      , liftArb ValueBool
      , liftM ValueString arbitraryPrintableString
       -- exposes bugs
       --        , liftM ValueString arbitraryString
      , liftArb ValueDouble
       -- exposes bugs
       --        , liftArb ValueDateTime
      , liftM ValueBase64 arbitraryByteString
      , liftM ValueStruct 
                   (arbitraryList (liftM2 (,) 
           arbitraryId (arbValue (n-1)))
                      (n-1)
       )
      , liftM ValueArray (arbitraryList (arbValue (n-1)) (n-1))
      ]

--------------------------------------------------

testErr :: Err Identity Bool -> Bool
testErr m = runIdentity (handleError (\_ -> return False) m)

--------------------------------------------------

-}