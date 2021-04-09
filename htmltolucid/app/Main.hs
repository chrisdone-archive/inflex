{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.XML.Types
import           System.IO
import           Text.HTML.DOM

main :: IO ()
main =
  runConduit
    (CB.sourceHandle stdin .| eventConduit .| lucidConduit .| CT.encode CT.utf8 .|
     CB.sinkHandle stdout)

lucidConduit :: ConduitT Event Text IO ()
lucidConduit =
  CL.mapMaybe
    (\case
       EventBeginElement name attrs ->
         Just (nameLocalName name <> "_ " <> attrs' <> "(do ")
         where attrs' =
                 if null attrs
                   then mempty
                   else "[" <>
                        T.intercalate
                          ", "
                          (map
                             (\(name, cs) ->
                                nameLocalName name <> "_ " <>
                                T.pack (show (foldMap (fromContent) cs)))
                             attrs) <>
                        "] "
       EventEndElement name -> Just ");"
       EventContent c'
         | ContentText c <- c'
         , not (T.null (T.strip c)) -> Just (T.pack (show c) <> ";")
       _ -> Nothing)
  where
    fromContent =
      \case
        ContentText text -> text
        ContentEntity e -> error "no entities allowed!"
