{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

import           Control.DeepSeq
import           Control.Monad
import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Gauge.Main
import           Inflex.Generator
import           Inflex.Instances ()
import           Inflex.NormalFormCheck as NF
import           Inflex.Parser
import           Inflex.Resolver
import           Inflex.Solver
import qualified RIO
import           RIO (newSomeRef, RIO)

main :: IO ()
main = do
  let !array1000 = T.concat ["[", T.intercalate "," (replicate 1000 "1"), "]"]
      !array2000 = T.concat ["[", T.intercalate "," (replicate 2000 "1"), "]"]
      !array4000 = T.concat ["[", T.intercalate "," (replicate 4000 "1"), "]"]
  when
    True
    (defaultMain
       [ bgroup
           "encodeUtf8"
           [ env
             (pure (T.replicate i sampleUnicode))
             (\t ->
                bench
                  ("T.encodeUtf8: " ++ show (i * T.length sampleUnicode))
                  (whnf T.encodeUtf8 t))
           | i <- [1, 10, 100]
           ]
       , bgroup
           "parseText"
           [ bench "array[1000]" (nf parseTextUpToErrorSuccess array1000)
           , bench "array[2000]" (nf parseTextUpToErrorSuccess array2000)
           , bench "array[4000]" (nf parseTextUpToErrorSuccess array4000)
           ]
       , bgroup
           "generateText"
           [ bench "array[1000]" (nf generateTextUpToErrorSuccess array1000)
           , bench "array[2000]" (nf generateTextUpToErrorSuccess array2000)
           , bench "array[4000]" (nf generateTextUpToErrorSuccess array4000)
           ]
       , bgroup
           "solveText"
           [ bench
             ("array[" <> show n <> "] SIG")
             (nfIO
                (do ref <- newSomeRef 0
                    binds <- newSomeRef mempty
                    RIO.runRIO
                      (SolveReader {glogfunc = mempty, counter = ref, binds})
                      (solveTextUpToErrorSuccess array)))
           | (n, array) <- [(1000 :: Int, array1000), (1000 :: Int, array4000)]
           ]
       , bgroup
           "normalFormCheck"
           [ env
               (case parseText "" array1000 of
                  Left {} -> error "parse failed"
                  Right ast -> pure $! (EqNF ast))
               (bench "array[1000]" . nf NF.expressionGenerate . unNF)
           , env
               (case parseText "" array2000 of
                  Left {} -> error "parse failed"
                  Right ast -> pure $! (EqNF ast))
               (bench "array[2000]" . nf NF.expressionGenerate . unNF)
           , env
               (case parseText "" array4000 of
                  Left {} -> error "parse failed"
                  Right ast -> pure $! (EqNF ast))
               (bench "array[4000]" . nf NF.expressionGenerate . unNF)
           ]
       , bgroup
           "resolveText"
           [ bench
             ("array[" <> show n <> "] SIG")
             (nfIO
                (do RIO.runRIO ResolveReader (resolveTextUpToErrorSuccess array)))
           | (n, array) <- [(1000 :: Int, array1000), (4000 :: Int, array4000)]
           ]
       ])

newtype EqNF a = EqNF { unNF :: a }
instance Eq a => NFData (EqNF a) where
  rnf (EqNF a) =
    let !_ = a == a
     in ()

parseTextUpToErrorSuccess :: Text -> Either () ()
parseTextUpToErrorSuccess = first (const ()) . second (const ()) . parseText ""

solveTextUpToErrorSuccess :: Text -> RIO SolveReader (Either () ())
solveTextUpToErrorSuccess = fmap (bimap (const ()) (const ())) . solveText mempty ""


resolveTextUpToErrorSuccess ::
     Text
  -> RIO ResolveReader (Either () ())
resolveTextUpToErrorSuccess =
  fmap (bimap (const ()) (const ())) . resolveText mempty ""

generateTextUpToErrorSuccess :: Text -> Either () ()
generateTextUpToErrorSuccess = bimap (const ()) (const ())  . generateText mempty ""

sampleUnicode :: Text
sampleUnicode = "! \" # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \\ ] ^ _ ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~ ¡ ¢ £ ¤ ¥ ¦ § ¨ © ª « ¬ ® ¯ ° ± ² ³ ´ µ ¶ · ¸ ¹ º » ¼ ½ ¾ ¿ À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß à á â ã ä å æ ç è é ê ë ì í î ï ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ Ā ā Ă ă Ą ą Ć ć Ĉ ĉ Ċ ċ Č č Ď ď Đ đ Ē ē Ĕ ĕ Ė ė Ę ę Ě ě Ĝ ĝ Ğ ğ Ġ ġ Ģ ģ Ĥ ĥ Ħ ħ Ĩ ĩ Ī ī Ĭ ĭ Į į İ ı Ĳ ĳ Ĵ ĵ Ķ ķ ĸ Ĺ ĺ Ļ ļ Ľ ľ Ŀ ŀ Ł ł Ń ń Ņ ņ Ň ň ŉ Ŋ ŋ Ō ō Ŏ ŏ Ő ő Œ œ Ŕ ŕ Ŗ ŗ Ř ř Ś ś Ŝ ŝ Ş ş Š š Ţ ţ Ť ť Ŧ ŧ Ũ ũ Ū ū Ŭ ŭ Ů ů Ű ű Ų ų Ŵ ŵ Ŷ ŷ Ÿ Ź ź Ż ż Ž ž ſ ƀ Ɓ Ƃ ƃ Ƅ ƅ Ɔ Ƈ ƈ Ɖ Ɗ Ƌ ƌ ƍ Ǝ Ə Ɛ Ƒ ƒ Ɠ Ɣ ƕ Ɩ Ɨ Ƙ ƙ ƚ ƛ Ɯ Ɲ ƞ Ɵ Ơ ơ Ƣ ƣ Ƥ ƥ Ʀ Ƨ ƨ Ʃ ƪ ƫ Ƭ ƭ Ʈ Ư ư Ʊ Ʋ Ƴ ƴ Ƶ ƶ Ʒ Ƹ ƹ ƺ ƻ Ƽ ƽ ƾ ƿ ǀ ǁ ǂ ǃ Ǆ ǅ ǆ Ǉ ǈ ǉ Ǌ ǋ ǌ Ǎ ǎ Ǐ ǐ Ǒ ǒ Ǔ ǔ Ǖ ǖ Ǘ ǘ Ǚ ǚ Ǜ ǜ ǝ Ǟ ǟ Ǡ ǡ Ǣ ǣ Ǥ ǥ Ǧ ǧ Ǩ ǩ Ǫ ǫ Ǭ ǭ Ǯ ǯ ǰ Ǳ ǲ ǳ Ǵ ǵ Ǻ ǻ Ǽ ǽ Ǿ ǿ Ȁ ȁ Ȃ ȃ ɐ ɑ ɒ ɓ ɔ ɕ ɖ ɗ ɘ ə ɚ ɛ ɜ ɝ ɞ ɟ ɠ ɡ ɢ ɣ ɤ ɥ ɦ ɧ ɨ ɩ ɪ ɫ ɬ ɭ ɮ ɯ ɰ ɱ ɲ ɳ ɴ ɵ ɶ ɷ ɸ ɹ ɺ ɻ ɼ ɽ ɾ ɿ ʀ ʁ ʂ ʃ ʄ ʅ ʆ ʇ ʈ ʉ ʊ ʋ ʌ ʍ ʎ ʏ ʐ ʑ ʒ ʓ ʔ ʕ ʖ ʗ ʘ ʙ ʚ ʛ ʜ ʝ ʞ ʟ ʠ ʡ ʢ ʣ ʤ ʥ ʦ ʧ ʨ"
