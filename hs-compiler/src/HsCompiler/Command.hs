{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module HsCompiler.Command (runCommand) where

import HsCompiler.Parser qualified
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    help,
    info,
    long,
    metavar,
    strOption,
  )

data Stage
  = Lex FilePath
  | Parse FilePath
  | CodeGen FilePath
  deriving stock (Eq, Show)

instance ToText Stage where
  toText stage =
    case stage of
      Lex _ -> "lex"
      Parse _ -> "parse"
      CodeGen _ -> "code-gen"

runCommand :: IO ()
runCommand = do
  stages <- execParser (info stages fullDesc)
  case stages of
    Lex filepath -> do
      fileContent <- readFileBS filepath
      print (HsCompiler.Parser.runParser (decodeUtf8 fileContent))
      pure ()
    Parse _filepath -> pure ()
    CodeGen filepath -> print $ "this is the filepath: " <> filepath

lexInput :: Parser Stage
lexInput =
  Lex <$> strOption (long "lex" <> metavar "FILEPATH" <> help "File path")

parseInput :: Parser Stage
parseInput =
  Parse <$> strOption (long "parse" <> metavar "FILEPATH" <> help "File path")

codeGenInput :: Parser Stage
codeGenInput =
  CodeGen <$> strOption (long "code-gen" <> metavar "FILEPATH" <> help "File path")

stages :: Parser Stage
stages =
  lexInput
    <|> parseInput
    <|> codeGenInput
