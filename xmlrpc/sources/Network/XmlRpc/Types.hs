{-# LANGUAGE ConstraintKinds #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Network.XmlRpc.Types where

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

  -- | for @instance FromXmlRpc 'Char'@, to avoid @OverlappingInstances@ between @instance FromXmlRpc ['Char']@ and @instance FromXmlRpc [a]@.
  fromValueList :: [Value] -> Parser [a]
  fromValueList = traverse fromValue

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

An XML-RPC method call. Consists of a method name and a list of parameters.

-}

data MethodCall = MethodCall String [Value]

--------------------------------------------------

{-| 

An XML-RPC response.

-}

data MethodResponse = Return Value -- ^ A method response returning a value
                    | Fault Int String -- ^ A fault response

--------------------------------------------------

{-| An XML-RPC value (similar to @JSON@).

primitive types:

* numbers (int, double)
* strings (strings, bytestrings)
* booleans
* timestamps

composite types:

* array (a list of Values)
* struct (a map from Strings to Values)

-}

data Value
    = ValueInt Int -- ^ int, i4, or i8
    | ValueBool Bool -- ^ bool
    | ValueString String -- ^ string
    | ValueUnwrapped String -- ^ no inner element
    | ValueDouble Double -- ^ double
    | ValueDateTime LocalTime -- ^ dateTime.iso8601
    | ValueBase64 BS.ByteString -- ^ base 64.  NOTE that you should provide the raw data; the haxr library takes care of doing the base-64 encoding.
    | ValueStruct [(String,Value)] -- ^ struct
    | ValueArray [Value]  -- ^ array

--------------------------------------------------

{-| 

An XML-RPC value. Use for error messages and introspection.

-}

data Type
          = TInt
          | TBool
          | TString
          | TDouble
          | TDateTime
          | TBase64
          | TStruct
          | TArray
          | TUnknown

--------------------------------------------------

{-| 

@≍ ExceptT String m a@

-}

data Parser a

  = Parser a
  
  deriving stock    (Functor,Foldable,Traversable)
  deriving stock    (Lift,Generic)
 -- deriving anyclass (NFData,Hashable)

instance Applicative Parser where
  pure = Parser
  (Parser f) <*> (Parser x) = Parser (f x)

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