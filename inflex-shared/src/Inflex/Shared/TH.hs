{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Inflex.Shared.TH where

import           Control.Concurrent.Async
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Yaml
import           GHC.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

data Sig = Sig {input :: Text, output :: Text}
 deriving (Show, Generic)
instance FromJSON Sig

generateSchema :: Q [Dec]
generateSchema = do
  qAddDependentFile "config/schema"
  qAddDependentFile "templates/Schema.hs"
  qAddDependentFile "templates/Schema.purs"
  qAddDependentFile "config/rpc"
  qAddDependentFile "templates/Rpc.hs"
  qAddDependentFile "templates/Rpc.purs"
  runIO
    (do concurrently_
          (do content <- T.readFile "config/schema"
              concurrently_
                (do hs <- T.readFile "templates/Schema.hs"
                    T.writeFile
                      "../inflex-server/src/Inflex/Schema.hs"
                      (T.replace "$types" content hs))
                (do purs <- T.readFile "templates/Schema.purs"
                    T.writeFile
                      "../inflex-client/src/Inflex/Schema.purs"
                      (T.replace "$types" content purs)))
          (do result <- decodeFileEither "config/rpc"
              case result of
                Left e -> error (show e)
                Right (sig :: Map Text Sig) ->
                  concurrently_
                    (do hs <- T.readFile "templates/Rpc.hs"
                        let content = hsDispatch sig
                        T.writeFile
                          "../inflex-server/src/Inflex/Rpc.hs"
                          (T.replace "$calls" content hs))
                    (do purs <- T.readFile "templates/Rpc.purs"
                        let content = T.unlines (map pursCall (M.toList sig))
                        T.writeFile
                          "../inflex-client/src/Inflex/Rpc.purs"
                          (T.replace "$calls" content purs)))
        pure [])

pursCall :: (Text, Sig) -> Text
pursCall (name0, Sig {input, output}) =
  T.unlines
    [ name <> " :: forall m. MonadAff m => " <> input <> " -> m (Either String (View " <>
      output <>
      "))"
    , name <> " = rpcCall \"" <> name0 <> "\""
    ]
  where
    name = "rpc" <> name0

hsDispatch :: Map Text Sig -> Text
hsDispatch sigs =
  T.unlines
    [ "rpcHandler :: Text -> Handler Value"
    , "rpcHandler name ="
    , "  case name of"
    , dispatch
    , "    _ -> Prelude.error \"Invalid RPC function.\""
    ]
  where
    dispatch =
      T.unlines
        (map
           (\(name, Sig {input, output}) ->
              T.unlines
                [ "    \"" <> name <> "\" -> timed (TimedRpcCall \"" <> name <> "\") $ do"
                , "      input <- requireCheckJsonBody"
                , "      output <- rpc" <> name <> " (input :: " <> input <> ")"
                , "      pure (toJSON (output :: " <> output <> "))"
                ])
           (M.toList sigs))
