{-# LANGUAGE ConstraintKinds #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Network.XmlRpc.Classes where

--------------------------------------------------
--------------------------------------------------

import Network.XmlRpc.Types

--------------------------------------------------

import           "time" Data.Time.Calendar
import           "time" Data.Time.LocalTime

-------------------------------------------------

import qualified "bytestring" Data.ByteString.Char8      as BS (ByteString)
import qualified "bytestring" Data.ByteString.Lazy.Char8 as BL (ByteString)

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

  -- | 

  toValue :: a -> Value

  -- default toValue
  --   :: (Generic a, GToJSON Value 0 (Rep a)) 
  --   => a -> Value

  -- | 

  getType :: a -> ValueType

  -- default getType
  --   :: (Generic a, GToJSON Value 0 (Rep a)) 
  --   => a -> Type

--------------------------------------------------

{- | Servers(/Handlers) must deserialize the request they've received from XML-RPC.

Unless the method being handled is @void@ (i.e. returns @()@), they can then serialize the response via 'ToXmlRpc'.

-}

class FromXmlRpc a where

  -- | 

  fromValue :: Value -> Parser a

  -- default fromValue 
  --   :: (Generic a, GFromJSON 0 (Rep a))
  --   => Value -> Parser a

  -- | for @instance FromXmlRpc 'Char'@, to avoid @OverlappingInstances@ between @instance FromXmlRpc ['Char']@ and @instance FromXmlRpc [a]@.

  fromValueList :: [Value] -> Parser [a]
  fromValueList = traverse fromValue

--------------------------------------------------
--------------------------------------------------

-- | unary 'XmlRpc'.

type XmlRpc1 f = (ToXmlRpc1 f, FromXmlRpc1 f)

--------------------------------------------------

class ToXmlRpc1 f where

--------------------------------------------------

class FromXmlRpc1 f where

--------------------------------------------------
--------------------------------------------------

-- | binary 'XmlRpc'.

type XmlRpc2 p = (ToXmlRpc2 p, FromXmlRpc2 p)

--------------------------------------------------

class ToXmlRpc2 p where

--------------------------------------------------

class FromXmlRpc2 p where

--------------------------------------------------
--------------------------------------------------

-- | higher-order 'XmlRpc'.

type XmlRpcF r = (ToXmlRpcF r, FromXmlRpcF r)

--------------------------------------------------

class ToXmlRpcF r where

--------------------------------------------------

class FromXmlRpcF r where

--------------------------------------------------
--------------------------------------------------