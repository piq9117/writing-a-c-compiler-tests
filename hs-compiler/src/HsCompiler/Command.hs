{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module HsCompiler.Command (runCommand) where

import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    help,
    info,
    long,
    strOption,
  )

data Stages
  = Lex
  | Parse
  | CodeGen
  deriving stock (Eq, Show)

instance ToText Stages where
  toText stage =
    case stage of
      Lex -> "lex"
      Parse -> "parse"
      CodeGen -> "code-gen"

data Command = Command
  { stage :: Stages
  }
  deriving stock (Show)

runCommand :: IO ()
runCommand = do
  command <- execParser (info command fullDesc)
  case command of
    Nothing -> fail "Invalid stage"
    Just command -> print command

command :: Parser (Maybe Command)
command = do
  stages <&> \stg ->
    case stg of
      "lex" -> Just (Command {stage = Lex})
      "parse" -> Just (Command {stage = Parse})
      "code-gen" -> Just (Command {stage = CodeGen})
      _ -> fail "invalid stage"

stages :: Parser Text
stages = strOption (long "stage" <> help "Compilation stage")
