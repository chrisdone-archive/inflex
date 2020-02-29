{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Yesod
import           Control.Monad.Catch (catch, MonadThrow)
import           Control.Monad.Logger
import           Control.Monad.Supply
import           Control.Monad.Writer
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import           Duet.Context
import           Duet.Errors
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Setup
import           Duet.Simple
import           Duet.Stepper
import           Duet.Types
import           System.IO

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
  /appjs AppJsR GET
  /api/refresh RefreshR POST
  / AppR GET
|]

getAppR :: Handler TypedContent
getAppR = sendFile "text/html" "index.html"

getAppJsR :: Handler TypedContent
getAppJsR = sendFile "application/javascript" "../inflex-client/app.js"

data DecOut = DecOut
  { name :: Text
  , rhs :: Text
  , result :: Text
  } deriving (Generic)
instance ToJSON DecOut where
  toJSON DecOut {name, rhs, result} =
    object ["name" .= name, "rhs" .= rhs, "result" .= result]

data DecIn = DecIn
  { name :: Text
  , rhs :: Text
  } deriving (Generic)
instance FromJSON DecIn where
  parseJSON j = do
    o <- parseJSON j
    DecIn <$> o .: "name" <*> o .: "rhs"

postRefreshR :: Handler TypedContent
postRefreshR =
  selectRep
    (provideRep
       (do json :: HashMap Text DecIn <- requireCheckJsonBody
           -- TODO: Evaluate the decls as a Duet module.
           pure Null))

main :: IO ()
main = warpEnv App

--------------------------------------------------------------------------------
-- Duet helpers

data Run = Run
  { runInputFile :: FilePath
  , runMainIs :: String
  , runConcise :: Bool
  , runNumbered :: Bool
  , runSteps :: Maybe Integer
  , runHideSteps :: Bool
  } deriving (Show)

runProgram :: Run -> IO ()
runProgram run@Run {..} = do
  catch
    (catch
       (runNoLoggingT
          (evalSupplyT
             (do decls <- liftIO (parseFile runInputFile)
                 (binds, ctx) <- createContext decls
                 things <-
                   execWriterT
                     (runStepperIO
                        run
                        runSteps
                        ctx
                        (fmap (fmap typeSignatureA) binds)
                        runMainIs)
                 pure things)
             [1 ..]))
       (putStrLn . displayContextException))
    (putStrLn . displayParseException)

-- | Run the substitution model on the code.
runStepperIO ::
     forall m. (MonadSupply Int m, MonadThrow m, MonadIO m)
  => Run
  -> Maybe Integer
  -> Context Type Name Location
  -> [BindGroup Type Name Location]
  -> String
  -> m ()
runStepperIO Run {..} maxSteps ctx bindGroups' i = do
  e0 <- lookupNameByString i bindGroups'
  loop 1 "" e0
  where
    loop :: Integer -> String -> Expression Type Name Location -> m ()
    loop count lastString e = do
      e' <- expandSeq1 ctx bindGroups' e
      let string = printExpression (defaultPrint) e
      when
        (string /= lastString && not runHideSteps)
        (if cleanExpression e || not runConcise
           then liftIO
                  (putStrLn
                     ((if runNumbered
                         then "[" ++ show count ++ "]\n"
                         else "") ++
                      printExpression defaultPrint e))
           else pure ())
      e'' <- pure e'
      if (fmap (const ()) e'' /= fmap (const ()) e) &&
         case maxSteps of
           Just top -> count < top
           Nothing -> True
        then do
          newE <-
            renameExpression
              (contextSpecials ctx)
              (contextScope ctx)
              (contextDataTypes ctx)
              e''
          loop (count + 1) string newE
        else pure ()

-- | Filter out expressions with intermediate case, if and immediately-applied lambdas.
cleanExpression :: Expression Type i l -> Bool
cleanExpression =
  \case
    CaseExpression {} -> False
    IfExpression {} -> False
    e0
      | (LambdaExpression {}, args) <- fargs e0 -> null args
    ApplicationExpression _ f x -> cleanExpression f && cleanExpression x
    _ -> True
