{-# LANGUAGE ConstraintKinds #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Network.XmlRpc.Types where

--------------------------------------------------



-------------------------------------------------

import Prelude_xmlrpc

--------------------------------------------------
--------------------------------------------------

-- | a @ConstraintKind@ requiring both serialization and deserialization.

type XmlRpc a = (ToXmlRpc a, FromXmlRpc a)

--------------------------------------------------

{- | Clients(/Callers) must serialize (the arguments of) their request into XML-RPC

Unless the method being called is @void@ (i.e. returns @()@), they can then deserialize the response via 'FromXmlRpc'.

-}

class ToXmlRpc a where

  toValue :: a -> Value

  -- default toValue
  --   :: (Generic a, GToJSON Value 0 (Rep a)) 
  --   => a -> Value

  getType :: a -> Type

  -- default getType
  --   :: (Generic a, GToJSON Value 0 (Rep a)) 
  --   => a -> Type

--------------------------------------------------

{- | Servers(/Handlers) must deserialize the request they've received from XML-RPC.

Unless the method being handled is @void@ (i.e. returns @()@), they can then serialize the response via 'ToXmlRpc'.

-}

class FromXmlRpc a where

  fromValue :: Value -> Parser a

  -- default fromValue 
  --   :: (Generic a, GFromJSON 0 (Rep a))
  --   => Value -> Parser a

  fromValueList :: [Value] -> Parser a
  -- fromValueList 

--------------------------------------------------

-- | unary 'XmlRpc'.

type XmlRpc1 f = (ToXmlRpc1 f, FromXmlRpc1 f)

class ToXmlRpc1 f where

class FromXmlRpc1 f where

--------------------------------------------------

-- | binary 'XmlRpc'.
type XmlRpc2 p = (ToXmlRpc2 p, FromXmlRpc2 p)

class ToXmlRpc2 p where

class FromXmlRpc2 p where

--------------------------------------------------

-- | higher-order 'XmlRpc'.
type XmlRpcF r = (ToXmlRpcF r, FromXmlRpcF r)

class ToXmlRpcF r where

class FromXmlRpcF r where

--------------------------------------------------

{-| 

-}

data Value

--------------------------------------------------

{-| 

-}

data Type

--------------------------------------------------

{-| 

@~ ExceptT String m a@

-}

data Parser a

-- instance Monad     Parser
-- instance MonadFail Parser

--------------------------------------------------

{-| 

@~ Either String a@

-}

data Result a

--------------------------------------------------
--------------------------------------------------
{- NOTES






class XmlRpcType a where

data XmlRpcValue a where
 XmlRpc :: XmlRpcType a => a -> XmlRpcValue a

instance XmlRpcType (XmlRpcValue a) where
 toValue (XmlRpc x) = toValue x
 getType (XmlRpc x) = getType x
 fromValue = ... -- needs constraint, as it **produces**





Instances in general must specify toJSON and should (but don't need to) specify toEncoding.



@~ ExceptT String m a@

data Parser a


instance MonadFail Parser




@~ Either String a@

data Result a



.


type Encoding = Encoding' Value 

-- | An encoding of a JSON value.
--
-- @tag@ represents which kind of JSON the Encoding is encoding to,
-- we reuse 'Text' and 'Value' as tags here.


newtype Encoding' tag = Encoding {
      fromEncoding :: Builder
}



Builder ~ (Text -> Text)




Data.Text.concat is an O(n+m) operation where n and m are the lengths of the strings you want to concat. This is because a new memory buffer of size n + m must be allocated to store the result of the concatenation.

Builder is specifically optimized for the mappend operation. It's a cheap O(1) operation (function composition, which is also excellently optimized by GHC). With Builder you are essentially building up the instructions for how to produce the final string result, but delaying the actual creation until you do some Builder -> Text transformation.

^ https://stackoverflow.com/questions/29102772/haskell-should-i-use-data-text-lazy-builder-to-construct-my-text-values


-}
--------------------------------------------------
--------------------------------------------------