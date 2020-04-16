{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

-- | API for form fields using the Lucid HTML generation library for
-- the view.

module Inflex.Server.Forge
  ( Error(..)
  , Field(..)
  , Required(..)
  , Autocomplete(..)
  , PasswordConfig(..)
  , wrap
  , labelled
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Fixed
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Forge.Internal.Types as Forge
import           Inflex.Server.Types
import qualified Lucid
import qualified Lucid.Base
import           Text.Email.Validate as Email
import           Text.Read (readMaybe)

-- | Wrap the view.
wrap ::
     (t -> t)
  -> Forge.Form index parse t field error a
  -> Forge.Form index parse t field error a
wrap f = Forge.CeilingForm (\es html -> (f html,es))

labelled ::
     (Monad m)
  => Text
  -> Forge.Form index parse (Lucid.HtmlT m ()) field Error a2
  -> Forge.Form index parse (Lucid.HtmlT m ()) field Error a2
labelled label =
  wrap Lucid.p_ .
  wrapErrorsAllowBubble label .
  wrap (\html -> (Lucid.label_ (Lucid.toHtml label *> ": " *> html)))

-- | Wrap a formlet with its errors. Allow errors to bubble up.
wrapErrorsAllowBubble ::
     Monad m
  => Text
  -> Forge.Form index parse (Lucid.HtmlT m ()) field Error a
  -> Forge.Form index parse (Lucid.HtmlT m ()) field Error a
wrapErrorsAllowBubble label =
  Forge.CeilingForm
    (\errors html' ->
       ( do html'
            Lucid.ul_
              [Lucid.class_ "inline-errors", vdomkey_ key]
              (mapM_ (Lucid.li_ . showError) errors)
       , map (ContextedError label) errors))
  where key = "inline-errors:" <> label -- Ensures convenient uniqueness for the DOM differ.

-- | Convert an error to HTML.
showError :: Monad m => Error -> Lucid.HtmlT m ()
showError =
  \case
    ContextedError label e -> do
      Lucid.toHtml label
      ": "
      showError e
    MissingInput {} -> "Missing input: please fill this one in."
    InvalidInputFormat {} -> "Invalid input format."

--------------------------------------------------------------------------------
-- Data types for this interface

-- | The errors possible with a lucid form.
data Error
  = MissingInput Forge.Key
  | InvalidInputFormat Forge.Key (NonEmpty Forge.Input)
  | ContextedError Text Error
  deriving (Show, Eq)
data RequiredT = RequiredT | OptionalT

data Required a where
  Required :: Required 'RequiredT
  Optional :: Required 'OptionalT
deriving instance Show (Required a)

type family Result r a where
  Result 'RequiredT a = a
  Result 'OptionalT a = Maybe a

data PasswordConfig r = PasswordConfig
  { required :: Required r
  , def :: Maybe Text
  , autocomplete :: Autocomplete
  } deriving (Show)

data Autocomplete = CompleteNewPassword | CompleteOff | CompleteEmail
 deriving (Show)

-- | A standard Html5 field.
data Field m a where
  TextField :: Maybe Text -> Field m Text
  PasswordField :: PasswordConfig r -> Field m (Result r Password)
  DateField :: Maybe Day -> Field m Day
  TextareaField :: Maybe Text -> Field m Text
  IntegerField :: Maybe Integer -> Field m Integer
  CheckboxField :: Maybe Bool -> Field m Bool
  MultiselectField :: Eq a => Maybe [a] -> NonEmpty (a, Text) -> Field m [a]
  DropdownField :: Eq a => Maybe a -> NonEmpty (a, Text) -> Field m a
  RadioGroupField
    :: (Eq a)
    => Maybe a
    -> NonEmpty (a, Text)
    -> (Integer -> Bool -> Lucid.HtmlT m () -> Lucid.HtmlT m () -> Lucid.HtmlT m ())
    -> Field m a
  EmailField :: Maybe Email -> Field m Email
  UsernameField :: Maybe Username -> Field m Username
  FixedField :: HasResolution a => Maybe (Fixed a) -> Field m (Fixed a)
  PhoneField :: Maybe Text -> Field m Text
  SliderField :: Eq a => SliderConfig a -> Field m a

-- | Slider configuration.
data SliderConfig a = SliderConfig
  { choices :: NonEmpty (a, Text)
  , def :: Maybe a
  } deriving (Show)

--------------------------------------------------------------------------------
-- Instantiation of classes

-- | Limited to the sum type 'Error' defined in this module.
instance Forge.FormError Error where
  missingInputError = MissingInput
  invalidInputFormat = InvalidInputFormat

-- | Instantiation of the standard Html5 fields.
instance (Forge.FormError error, Monad m) =>
         Forge.FormField (Lucid.HtmlT m ()) (Field m) error where
  parseFieldInput key field input = parseFieldInput' key field input
  viewField key minput = viewField' key minput

-- | Used to be replaced with @key@ in the virtual-dom library.
vdomkey_ :: Text -> Lucid.Attribute
vdomkey_ = Lucid.Base.makeAttribute vdomkeyAttributeName

-- | Used to be replaced with @key@ in the virtual-dom library.
vdomkeyAttributeName :: Text
vdomkeyAttributeName = "data-key"

parseFieldInput' ::
     Forge.FormError e
  => Forge.Key
  -> Field m a
  -> NonEmpty Forge.Input
  -> Either e a
parseFieldInput' key field input =
  case field of
    SliderField SliderConfig {choices} -> do
      keys <-
        mapM
          (\case
             (Forge.TextInput text) ->
               case readMaybe (T.unpack text) of
                 Nothing -> Left (Forge.invalidInputFormat key input)
                 Just i -> pure i
             _ -> Left (Forge.invalidInputFormat key input))
          input
      values <-
        mapM
          (\k ->
             case lookup k keyedChoices of
               Nothing -> Left (Forge.invalidInputFormat key input)
               Just ok -> pure ok)
          keys
      case listToMaybe (toList values) of
        Nothing -> Left (Forge.missingInputError key)
        Just a -> pure a
      where keyedChoices =
              map
                (\(i, (value, _title)) -> (i, value))
                (zip [0 :: Integer ..] (toList choices))
    CheckboxField _ ->
      pure
        (any
           (\case
              Forge.TextInput "true" -> True
              _ -> False)
           (toList input))
    TextField _ ->
      case input of
        Forge.TextInput text :| [] -> pure text
        _ -> Left (Forge.invalidInputFormat key input)
    PasswordField PasswordConfig {required} ->
      case required of
        Required ->
          case input of
            Forge.TextInput text :| []
              -- TODO: Set minimum length
              | not (T.null text) -> pure (Password text)
              | otherwise -> Left (Forge.missingInputError key)
            _ -> Left (Forge.invalidInputFormat key input)
        Optional ->
          case input of
            Forge.TextInput text :| []
              | not (T.null text) -> pure (Just (Password text))
              | otherwise -> pure Nothing
            _ -> Left (Forge.invalidInputFormat key input)
    DateField _ ->
      case input of
        Forge.TextInput text :| [] ->
          case parseDate text of
            Just date -> pure date
            Nothing -> Left (Forge.invalidInputFormat key input)
        _ -> Left (Forge.invalidInputFormat key input)
    TextareaField _ ->
      case input of
        Forge.TextInput textarea :| [] -> pure textarea
        _ -> Left (Forge.invalidInputFormat key input)
    PhoneField _ ->
      case input of
        Forge.TextInput phone :| [] -> pure phone
        _ -> Left (Forge.invalidInputFormat key input)
    IntegerField _ ->
      case input of
        Forge.TextInput text :| [] ->
          case readMaybe (T.unpack text) of
            Just i -> pure i
            Nothing -> Left (Forge.invalidInputFormat key input)
        _ -> Left (Forge.invalidInputFormat key input)
    FixedField _ ->
      case input of
        Forge.TextInput text :| [] ->
          case readFixed (T.unpack text) of
            Just i -> pure i
            Nothing -> Left (Forge.invalidInputFormat key input)
        _ -> Left (Forge.invalidInputFormat key input)
    EmailField _ ->
      case input of
        Forge.TextInput (T.strip -> text) :| []
          | T.null text -> Left (Forge.missingInputError key)
          | otherwise ->
            case Email.validate (T.encodeUtf8 text) of
              Right _i -> pure (Email text)
              Left {} -> Left (Forge.invalidInputFormat key input)
        _ -> Left (Forge.invalidInputFormat key input)
    UsernameField _ ->
      case input of
        Forge.TextInput (T.strip -> text) :| []
          | T.null text -> Left (Forge.missingInputError key)
          | otherwise ->
            case parseUsername text of
              Just i -> pure i
              Nothing -> Left (Forge.invalidInputFormat key input)
        _ -> Left (Forge.invalidInputFormat key input)
    MultiselectField _ choices -> do
      keys <-
        mapM
          (\case
             (Forge.TextInput text) -> Right text
             _ -> Left (Forge.invalidInputFormat key input))
          input
      values <-
        mapM
          (\k ->
             case lookup k keyedChoices of
               Nothing -> Left (Forge.invalidInputFormat key input)
               Just ok -> pure ok)
          keys
      pure (toList values)
      where keyedChoices =
              map
                (\(i, (value, title)) -> (uniqueKey i title, value))
                (zip [0 :: Integer ..] (toList choices))
    DropdownField _ choices -> do
      keys <-
        mapM
          (\case
             (Forge.TextInput text) -> Right text
             _ -> Left (Forge.invalidInputFormat key input))
          input
      values <-
        mapM
          (\k ->
             case lookup k keyedChoices of
               Nothing -> Left (Forge.invalidInputFormat key input)
               Just ok -> pure ok)
          keys
      case listToMaybe (toList values) of
        Nothing -> Left (Forge.missingInputError key)
        Just a -> pure a
      where keyedChoices =
              map
                (\(i, (value, title)) -> (uniqueKey i title, value))
                (zip [0 :: Integer ..] (toList choices))
    RadioGroupField _ choices _render -> do
      keys <-
        mapM
          (\case
             (Forge.TextInput text) -> Right text
             _ -> Left (Forge.invalidInputFormat key input))
          input
      values <-
        mapM
          (\k ->
             case lookup k keyedChoices of
               Nothing -> Left (Forge.invalidInputFormat key input)
               Just ok -> pure ok)
          keys
      case listToMaybe (toList values) of
        Nothing -> Left (Forge.missingInputError key)
        Just a -> pure a
      where keyedChoices =
              map
                (\(i, (value, title)) -> (uniqueKey i title, value))
                (zip [0 :: Integer ..] (toList choices))

viewField' ::
     Monad m => Forge.Key -> Maybe (NonEmpty Forge.Input) -> Field m a -> Lucid.HtmlT m ()
viewField' key minput =
  \case
    DateField mdef ->
      Lucid.input_
        [ Lucid.type_ "date"
        , Lucid.name_ (Forge.unKey key)
        , vdomkey_ (Forge.unKey key)
        , Lucid.value_
            (let mdate =
                   case minput of
                     Just (Forge.TextInput text :| []) ->
                       case parseDate text of
                         Just date -> Just date
                         Nothing -> Nothing
                     Just _ -> Nothing
                     Nothing -> mdef
              in maybe "" showDate mdate)
        ]
    CheckboxField mdef -> do
      Lucid.input_
        [ Lucid.type_ "hidden"
        , Lucid.name_ (Forge.unKey key)
        , vdomkey_ (Forge.unKey key <> "/hidden")
        , Lucid.value_ "false"
        ]
      Lucid.input_
        ([ Lucid.type_ "checkbox"
         , Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key <> "/checkbox")
         , Lucid.value_ "true"
         ] <>
         (case minput of
            Nothing -> [Lucid.checked_ | mdef == Just True]
            Just input ->
              [ Lucid.checked_
              | any
                  (\case
                     Forge.TextInput "true" -> True
                     _ -> False)
                  (toList input)
              ]))
    TextField mdef ->
      Lucid.input_
        ([Lucid.name_ (Forge.unKey key), vdomkey_ (Forge.unKey key)] <>
         [ Lucid.value_ value
         | Just (Forge.TextInput value :| []) <-
             [minput <|> fmap (pure . Forge.TextInput) mdef]
         ])
    PasswordField PasswordConfig {autocomplete, required, def = mdef} ->
      Lucid.input_
        ([ Lucid.type_ "password"
         , Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key)
         , Lucid.autocomplete_
             (case autocomplete of
                CompleteNewPassword -> "new-password"
                CompleteOff -> "off"
                CompleteEmail -> "email")
         ] <>
         (case required of
            Required -> [Lucid.required_ "required"]
            _ -> []) <>
         [ Lucid.value_ value
         | Just (Forge.TextInput value :| []) <-
             [minput <|> fmap (pure . Forge.TextInput) mdef]
         ])
    TextareaField mdef ->
      Lucid.textarea_
        ([Lucid.name_ (Forge.unKey key), vdomkey_ (Forge.unKey key)])
        (maybe
           mempty
           (\case
              Forge.TextInput value :| [] -> Lucid.toHtml value
              _ -> mempty)
           (minput <|> fmap (pure . Forge.TextInput) mdef))
    PhoneField mdef ->
      Lucid.input_
        ([ Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key)
         , Lucid.type_ "tel"
         ] <>
         [ Lucid.value_ value
         | Just (Forge.TextInput value :| []) <-
             [minput <|> fmap (pure . Forge.TextInput) mdef]
         ])
    EmailField mdef ->
      Lucid.input_
        ([ Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key)
         , Lucid.type_ "email"
         ] <>
         [ Lucid.value_ value
         | Just (Forge.TextInput value :| []) <-
             [ minput <|>
               fmap
                 (pure . Forge.TextInput)
                 (fmap (\(Email t) -> t) mdef)
             ]
         ])
    UsernameField mdef ->
      Lucid.input_
        ([ Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key)
         , Lucid.type_ "text"
         ] <>
         [ Lucid.value_ value
         | Just (Forge.TextInput value :| []) <-
             [ minput <|>
               fmap
                 (pure . Forge.TextInput)
                 (fmap (\(Username t) -> t) mdef)
             ]
         ])
    IntegerField mdef ->
      Lucid.input_
        ([ Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key)
         , Lucid.type_ "text"
         , Lucid.pattern_ "-?[0-9]*"
         ] <>
         [ Lucid.value_ value
         | Just (Forge.TextInput value :| []) <-
             [minput <|> fmap (pure . Forge.TextInput . T.pack . show) mdef]
         ])
    FixedField mdef ->
      Lucid.input_
        ([ Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key)
         , Lucid.type_ "text"
         , Lucid.pattern_ "-?[0-9.]*"
         ] <>
         [ Lucid.value_ value
         | Just (Forge.TextInput value :| []) <-
             [minput <|> fmap (pure . Forge.TextInput . T.pack . show) mdef]
         ])
    MultiselectField mdef choices ->
      Lucid.select_
        ([ Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key)
         , Lucid.multiple_ "multiple"
         ])
        (mapM_
           (\(i, (a, label)) ->
              Lucid.option_
                ([ Lucid.value_ (uniqueKey i label)
                 , vdomkey_ (Forge.unKey key <> "/" <> uniqueKey i label)
                 ] <>
                 case minput of
                   Just inputs
                     | elem
                        (uniqueKey i label)
                        (mapMaybe
                           (\case
                              Forge.TextInput s -> pure s
                              _ -> Nothing)
                           (toList inputs)) -> [Lucid.selected_ "selected"]
                   _ ->
                     case mdef of
                       Just defaults
                         | elem a defaults -> [Lucid.selected_ "selected"]
                       _ -> [])
                (Lucid.toHtml label))
           (zip [0 :: Integer ..] (toList choices)))
    DropdownField mdef choices ->
      Lucid.select_
        [Lucid.name_ (Forge.unKey key), vdomkey_ (Forge.unKey key)]
        (mapM_
           (\(i, (a, label)) ->
              Lucid.option_
                ([ Lucid.value_ (uniqueKey i label)
                 , vdomkey_ (Forge.unKey key <> "/" <> uniqueKey i label)
                 ] <>
                 case minput of
                   Just inputs
                     | elem
                        (uniqueKey i label)
                        (mapMaybe
                           (\case
                              Forge.TextInput s -> pure s
                              _ -> Nothing)
                           (toList inputs)) -> [Lucid.selected_ "selected"]
                   _ ->
                     case mdef of
                       Just default'
                         | a == default' -> [Lucid.selected_ "selected"]
                       _ -> [])
                (Lucid.toHtml label))
           (zip [0 :: Integer ..] (toList choices)))
    SliderField SliderConfig {def = mdef, choices} ->
      Lucid.input_
        ([ Lucid.name_ (Forge.unKey key)
         , vdomkey_ (Forge.unKey key)
         , Lucid.type_ "text"
         , Lucid.class_ "forge-slider-field"
         , Lucid.data_
             "forge-slider-choices"
             (T.decodeUtf8 (L.toStrict (encode (map snd (toList choices)))))
         ] <>
         case do let given :: Maybe Int
                     given = do
                       formInput <- fmap NE.head minput
                       idx <-
                         case formInput of
                           Forge.TextInput t -> readMaybe (T.unpack t)
                           _ -> Nothing
                       pure idx
                     defaulted = do
                       def <- mdef
                       lookup
                         def
                         (zipWith (\i (a, _) -> (a, i)) [0 ..] (toList choices))
                 given <|> defaulted of
           Nothing -> []
           Just label -> [Lucid.value_ (fromString (show label))])
    RadioGroupField mdef choices render ->
      mapM_
        (\(i, (a, label)) ->
           let checked =
                 case minput of
                   Just inputs
                     | elem
                        (uniqueKey i label)
                        (mapMaybe
                           (\case
                              Forge.TextInput s -> pure s
                              _ -> Nothing)
                           (toList inputs)) -> True
                   _ ->
                     case mdef of
                       Just default'
                         | a == default' -> True
                       _ -> False
            in render
                 i
                 checked
                 (Lucid.input_
                    ([ Lucid.type_ "radio"
                     , Lucid.value_ (uniqueKey i label)
                     , Lucid.name_ (Forge.unKey key)
                     , vdomkey_ (Forge.unKey key <> "/" <> uniqueKey i label)
                     ] <>
                     if checked
                       then [Lucid.checked_]
                       else []))
                 (Lucid.toHtml label))
        (zip [0 :: Integer ..] (toList choices))

-- | A key which is unique with respect to a list index and its display.
uniqueKey :: Integer -> Text -> Text
uniqueKey i title = fromString (show i) <> ":" <> title

-- | Read a fixed precision.
readFixed :: HasResolution p => String -> Maybe (Fixed p)
readFixed s = do
  fixed <- readMaybe s
  let (_, rhs) = span (/= '.') s
  guard
    (null rhs ||
     (all isDigit (drop 1 rhs) && (10 ^ length (drop 1 rhs)) <= resolution fixed))
  pure fixed

-- | Parse a valid date.
parseDate :: Text -> Maybe Day
parseDate t = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)

showDate :: Day -> Text
showDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"
