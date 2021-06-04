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
import           Data.Vector (Vector)
import           Inflex.Schema
import           Inflex.Server.Csv
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
        Right (_headers, rows :: Vector (HashMap Text Text)) -> do
          let guessed = guessCsvSchema (File {id = 0, name = ""}) bytes
          case guessed of
            GuessCassavaFailure e -> Prelude.error (show e)
            CsvGuessed schema ->
              case importViaSchema (File {id = 0, name = ""}) schema rows of
                Left e -> Prelude.error (show e)
                Right rows' ->
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
                         , bench "rowsToArray" (whnf (rowsToArray schema) rows')
                         ]
                     ])


monzofp :: FilePath
monzofp =
  "/home/chris/Downloads/Monzo Data Export - CSV (Friday, February 26th, 2021).csv"
