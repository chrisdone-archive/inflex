-- | RELEASE

module Log (prettyWrite) where
{-# INLINE prettyWrite #-}
prettyWrite :: a -> IO ()
prettyWrite _ = pure ()
