{-# OPTIONS_GHC -F -pgmF inflex-pgmf #-}

newtype DocumentId = DocumentId Int

data Command
  = LoadDocument DocumentId
  | RefreshDocument DocumentId Document

data Document = Document
  { cells :: Vector Cell
  }

data Cell = Cell
  { uuid :: UUID
  }
