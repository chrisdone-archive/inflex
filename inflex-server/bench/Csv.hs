{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, OverloadedLists #-}

-- |

import           Criterion.Main
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import           Inflex.NormalFormCheck
import           Inflex.Schema
import           Inflex.Server.Csv
import           Inflex.Types
import           System.Directory

main :: IO ()
main = do
  mbytes <-
    do exists <- doesFileExist monzofp
       if exists
         then fmap (Just . L.fromStrict) (S.readFile monzofp)
         else pure Nothing
  case mbytes of
    Nothing -> Prelude.error "Missing file."
    Just bytes -> do
      case Csv.decodeByName bytes of
        Left err -> Prelude.error ("Bad CSV parse: " ++ err)
        Right (headers, rows0 :: Vector (HashMap Text Text)) -> do
          let rows = fmap (hashMapToOMap (fmap T.decodeUtf8 headers)) rows0
              guessed = guessCsvSchema (File {id = 0, name = ""}) bytes
          case guessed of
            GuessCassavaFailure e -> Prelude.error (show e)
            CsvGuessed schema ->
              case importViaSchema (File {id = 0, name = ""}) schema rows of
                Left e -> Prelude.error (show e)
                Right rows' ->
                  case resolveParsedT (ArrayExpression rows0') of
                    Left err -> Prelude.error (show err)
                    Right !_resolved ->
                      defaultMain
                        ([ bgroup
                             "Monzo export"
                             [ bench
                                 "guessCsvSchema"
                                 (whnf
                                    (guessCsvSchema (File {id = 0, name = ""}))
                                    bytes)
                             , bench
                                 "importViaSchema"
                                 (whnf
                                    (importViaSchema
                                       (File {id = 0, name = ""})
                                       schema)
                                    rows)
                             , bench
                                 "rowsToArray"
                                 (whnf (rowsToArray schema) rows')
                             , bench
                                 "resolveParsedT"
                                 (whnf resolveParsedT (ArrayExpression rows0'))
                             ]
                         ])
                  where !rows0' = rowsToArray schema rows'


monzofp :: FilePath
monzofp =
  "/home/chris/Downloads/Monzo Data Export - CSV (Friday, February 26th, 2021).csv"
