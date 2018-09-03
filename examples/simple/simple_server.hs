{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- A minimal server

import Network.XmlRpc.Server
import Prelude

add :: Int -> Int -> IO Int
add x y = return (x + y)

main = cgiXmlRpcServer [("examples.add", fun add)]