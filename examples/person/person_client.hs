{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Example client using a heterogeneous struct.

import Network.XmlRpc.Client
import PersonTH
import Prelude

server = "http://localhost/~sboo/cgi-bin/person_server"

listPeople :: IO [Person]
listPeople = remote server "listPeople"

main = do
       people <- listPeople
       mapM_ print people
