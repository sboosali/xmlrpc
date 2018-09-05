{-# LANGUAGE ConstraintKinds #-}

--------------------------------------------------
--------------------------------------------------

{-|

Re-Exports the core types and functions (i.e. shared by @.Client@ and @.Server@) in this package (i.e. @xmlrpc-core@ TODO).

Import qualified:

@
import qualified "xmlrpc" XmlRpc
@

-}

module XmlRpc
  
  ( -- * Core represention types. 'Value', 'ValueType', 'MethodCall', 'MethodResponse', 'MethodName', etc.
    module Network.XmlRpc.Types

    -- * Marshalling classes. 'XmlRpc', 'FromXmlRpc', 'ToXmlRpc', 'XmlRpc1', 'XmlRpc1', 'XmlRpcF', etc.
  , module Network.XmlRpc.Classes

  ) where

--------------------------------------------------
--------------------------------------------------

import Network.XmlRpc.Types
import Network.XmlRpc.Classes

--------------------------------------------------
--------------------------------------------------