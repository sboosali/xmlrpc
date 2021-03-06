{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- Reads a method response in XML from standard input and prints its
-- internal representation to standard output.

import Network.XmlRpc.Internals
import Prelude

main = do
       c <- getContents
       r <- handleError fail (parseResponse c)
       putStrLn (show r)
