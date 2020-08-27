{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Inflex.Server.Compute where

import           Data.Foldable
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           Inflex.Display ()
import           Inflex.Document
import           Inflex.Instances ()
import qualified Inflex.Schema as Shared
import           Inflex.Types
import           RIO

loadInputDocument :: Shared.InputDocument -> Shared.OutputDocument
loadInputDocument (Shared.InputDocument {cells}) =
  Shared.OutputDocument
    (V.fromList
       (fmap
          (\Named {uuid = Uuid uuid, name, thing} ->
             Shared.OutputCell
               { code =
                   fromMaybe -- TODO: Thread the code through the document?
                     ""
                     (listToMaybe
                        (mapMaybe
                           (\Shared.InputCell {uuid = uuid', code} -> do
                              guard (Shared.UUID uuid == uuid')
                              pure code)
                           (toList cells)))
               , uuid = Shared.UUID uuid
               , name
               , result =
                   either
                     (Shared.ResultError . T.pack . show)
                     (Shared.ResultOk . textDisplay)
                     thing
               })
          (unToposorted
             (evalDocument (evalEnvironment loaded) (defaultDocument loaded)))))
  where
    loaded =
      loadDocument
        (map
           (\Shared.InputCell {uuid = Shared.UUID uuid, name, code} ->
              Named {uuid = Uuid uuid, name, thing = code})
           (toList cells))
