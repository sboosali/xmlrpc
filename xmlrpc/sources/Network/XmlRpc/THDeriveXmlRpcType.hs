-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.THDeriveXmlRpcType
-- Copyright   :  (c) Bjorn Bringert 2003-2005
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- Uses Template Haskell to automagically derive instances of 'XmlRpcType'
--
------------------------------------------------------------------------------

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.XmlRpc.THDeriveXmlRpcType (asXmlRpcStruct) where

import           Control.Monad            (liftM, replicateM)
import           Data.List                (genericLength)
import           Data.Maybe               (maybeToList)
import           Language.Haskell.TH
import           Network.XmlRpc.Internals hiding (Type)

-- | Creates an 'XmlRpcType' instance which handles a Haskell record
--   as an XmlRpc struct. Example:
-- @
-- data Person = Person { name :: String, age :: Int }
-- $(asXmlRpcStruct \'\'Person)
-- @
asXmlRpcStruct :: Name -> Q [Dec]
asXmlRpcStruct name =
    do
    info <- reify name
    dec <- case info of
                     TyConI d -> return d
                     _ -> fail $ show name ++ " is not a type constructor"
    mkInstance dec

mkInstance :: Dec -> Q [Dec]
#if MIN_VERSION_template_haskell(2,11,0)
mkInstance  (DataD _ n _ _ [RecC c fs] _) =
#else
mkInstance  (DataD _ n _ [RecC c fs] _) =
#endif
    do
    let ns = (map (\ (f,_,t) -> (unqual f, isMaybe t)) fs)
    tv <- mkToValue ns
    fv <- mkFromValue c ns
    gt <- mkGetType
    liftM (:[]) $ instanceD (cxt []) (appT (conT ''XmlRpcType)
                                    (conT n))
              (map return $ concat [tv, fv, gt])

mkInstance _ = error "Can only derive XML-RPC type for simple record types"


isMaybe :: Type -> Bool
isMaybe (AppT (ConT n) _) | n == ''Maybe = True
isMaybe _ = False


unqual :: Name -> Name
unqual = mkName . reverse . takeWhile (`notElem` [':','.']) . reverse . show

mkToValue :: [(Name,Bool)] -> Q [Dec]
mkToValue fs =
    do
    p <- newName "p"
    simpleFun 'toValue [varP p]
                (appE (varE 'toValue)
                          (appE [| concat |] $ listE $ map (fieldToTuple p) fs))


simpleFun :: Name -> [PatQ] -> ExpQ -> Q [Dec]
simpleFun n ps b = sequence [funD n [clause ps (normalB b) []]]

fieldToTuple :: Name -> (Name,Bool) -> ExpQ
fieldToTuple p (n,False) = listE [tupE [stringE (show n),
                                         appE (varE 'toValue)
                                         (appE (varE n) (varE p))
                                        ]
                                 ]
fieldToTuple p (n,True) =
    [| map (\v -> ($(stringE (show n)), toValue v)) $ maybeToList $(appE (varE n) (varE p)) |]

mkFromValue :: Name -> [(Name,Bool)] -> Q [Dec]
mkFromValue c fs =
    do
    names <- replicateM (length fs) (newName "x")
    v <- newName "v"
    t <- newName "t"
    simpleFun 'fromValue [varP v] $
               doE $ [bindS (varP t) (appE (varE 'fromValue) (varE v))] ++
                      zipWith (mkGetField t) (map varP names) fs ++
                      [noBindS $ appE [| return |] $ appsE (conE c:map varE names)]

mkGetField t p (f,False) = bindS p (appsE [varE 'getField,
                                           stringE (show f), varE t])
mkGetField t p (f,True) = bindS p (appsE [varE 'getFieldMaybe,
                                          stringE (show f), varE t])

mkGetType :: Q [Dec]
mkGetType = simpleFun 'getType [wildP]
             (conE 'TStruct)
