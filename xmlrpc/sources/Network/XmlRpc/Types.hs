--------------------------------------------------
--------------------------------------------------

{-|



-}

module Network.XmlRpc.Types where

--------------------------------------------------
--------------------------------------------------

--import           "time" Data.Time.Calendar
import           "time" Data.Time.LocalTime

-------------------------------------------------

import qualified "bytestring" Data.ByteString.Char8      as BS (ByteString)
--import qualified "bytestring" Data.ByteString.Lazy.Char8 as BL (ByteString)

-------------------------------------------------

import Prelude_xmlrpc

--------------------------------------------------
--------------------------------------------------

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

NOTE:

* 'ValueBase64': provide the raw data via a smart-constructor (like 'base64Value') which automatically encodes it into the @base-64@ format.

-}

data Value

  = ValueBool        Bool             -- ^ @≡ \<bool\>@

  | ValueInt         Int              -- ^ @≡ \<int\>@ (or @\<i4\>@ or @\<i8\>@)
  | ValueDouble      Double           -- ^ @≡ \<double\>@

  | ValueString      String           -- ^ @≡ \<string\>@
  | ValueUnwrapped   String           -- ^ TODO? (like @\<string\>@, but no inner element)
  | ValueBase64      BS.ByteString    -- ^ @≡ \<base64\>@

  | ValueDateTime    LocalTime        -- ^ @≡ \<dateTime.iso8601\>@

  | ValueStruct      StructOfValues   -- ^ @≡ \<struct\>@
  | ValueArray       ArrayOfValues    -- ^ @≡ \<array\>@

  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData)

{-

[Problem]

    * No instance for (Lift BS.ByteString)
    * No instance for (Hashable LocalTime)

-}

--------------------------------------------------

{-|

-}

type StructOfValues = [(String,Value)]  -- TODO Map String Value (keys are unordered and unduplicated)

--------------------------------------------------

{-|

-}

type ArrayOfValues = [Value] -- TODO Array/Seq Value (is finite)

--------------------------------------------------

{-| An XML-RPC value-type.

Used for error messages and introspection.

-}

data ValueType

  = TypeBool

  | TypeInt
  | TypeDouble

  | TypeString
  | TypeBase64

  | TypeDateTime

  | TypeStruct
  | TypeArray

  --TODO | TypeUnknown

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| An XML-RPC method call.

Consists of a method name ('MethodName') and a list of parameters (@['Value']@).

Corresponds to the @<methodCall>@ element in an XML-RPC request.

-}

data MethodCall = MethodCall

 { function  :: String
 , arguments :: [Value]
 }

 deriving stock    (Show,Read,Eq,Ord,Generic)
 deriving anyclass (NFData)

--------------------------------------------------

{-| An XML-RPC response.

'MethodFault' is failure, 'MethodReturn' is success.

-}

type MethodResponse = Either MethodFault MethodReturn

--------------------------------------------------

{-|  A successful XML-RPC response.

i.e. the XML-RPC method returns with a single 'Value'.

-}

newtype MethodReturn = MethodReturn
  { returnValue :: Value
  } 

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData)

--------------------------------------------------

-- | @= 'defaultMethodReturn'@
instance Default MethodReturn where
  def = defaultMethodReturn

--------------------------------------------------

{-| A failed XML-RPC response. 

i.e. the XML-RPC method fails, with an arbitrary error 'message', and a custom error 'code'.

'MethodFault' corresponds to the @<fault>@ element in an XML-RPC response:

* 'code': corresponds to a @<faultCode>@ element.
* 'messsage': corresponds to @<faultString>@ element.

NOTE the XML-RPC Specification doesn't specify any meanings for error 'code's.
Thus, they can be implementation-defined (e.g. mirroring the @sh@ell exit codes); or service-defined, for a particular server-client pair .

-}

data MethodFault = MethodFault

  { code    :: Int
  , message :: String
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultMethodFault'@
instance Default MethodFault where
  def = defaultMethodFault

--------------------------------------------------

{- |

The @<methodName>@ string in an XML-RPC request can have only the following characters:

* alphanumeric (i.e. upper-case @A-Z@, lower-case @a-z@, and numeric characters @0-9@)
* identifier
* underscore, dot, colon, slash

-}

newtype MethodName = MethodName

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

instance IsString MethodName where
  fromString = coerce

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

{-| @= 'stringValue'@

**WARNING** this is a partial function.

-}

instance IsString Value where
  fromString = stringValue

--------------------------------------------------
--------------------------------------------------

{-| the type ('ValueType') of the given value ('Value').

For example,

@
> 'valueType' ('ValueBool' _)
'TypeBool'
@

i.e.

@
>>> import Prelude (undefined)
>>> valueType (ValueBool False)
TypeBool
>>> valueType (ValueBool True)
TypeBool
>>> valueType (ValueBool undefined)
TypeBool
@

and so on.

-}

valueType :: Value -> ValueType
valueType = \case

  ValueBool      {} -> TypeBool
  ValueInt       {} -> TypeInt
  ValueDouble    {} -> TypeDouble
  ValueString    {} -> TypeString
  ValueUnwrapped {} -> TypeString
  ValueBase64    {} -> TypeBase64
  ValueDateTime  {} -> TypeDateTime
  ValueStruct    {} -> TypeStruct
  ValueArray     {} -> TypeArray

--------------------------------------------------

{-|

**WARNING** this is a partial function, because it performs validation.

-}

stringValue :: String -> Value
stringValue
  = validStringValue
  > maybe _stringValue id 

--------------------------------------------------

{-|



-}

validStringValue :: String -> Maybe Value
validStringValue = _validStringValue

--------------------------------------------------

{-| A (successful) method that returns nothing. 

@≡ ""@

The empty string substitutes '()', because the XML-RPC Standard doesn't explicitly support the @null@ value.

-}

emptyMethodReturn :: MethodReturn
emptyMethodReturn = MethodReturn{..}
  where
  returnValue = coerce (stringValue "")

--------------------------------------------------

-- | @= 'emptyMethodReturn'@
defaultMethodReturn :: MethodReturn
defaultMethodReturn = emptyMethodReturn

--------------------------------------------------

{-| A non-zero code and no message:

@
'code'    = 1
'message' = ""
@

-}

defaultMethodFault :: MethodFault
defaultMethodFault = MethodFault{..}
  where
  code    = 1
  message = ""

--------------------------------------------------
--------------------------------------------------
{- NOTES





valueType :: Value -> Maybe ValueType
valueType = \case

  ValueBool      {} -> Just TypeBool
  ValueInt       {} -> Just TypeInt
  ValueDouble    {} -> Just TypeDouble
  ValueString    {} -> Just TypeString
  ValueUnwrapped {} -> Just TypeUnwrapped
  ValueBase64    {} -> Just TypeBase64
  ValueDateTime  {} -> Just TypeDateTime
  ValueStruct    {} -> Just TypeStruct
  ValueArray     {} -> Just TypeArray
  _                 -> Nothing






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






-- newtype MethodCall = MethodCall

--   String

--   deriving stock    (Show,Read,Lift,Generic)
--   deriving newtype  (Eq,Ord,Semigroup,Monoid)
--   deriving newtype  (NFData,Hashable)

-- instance IsString MethodCall where
--   fromString = coerce






-}
--------------------------------------------------
--------------------------------------------------