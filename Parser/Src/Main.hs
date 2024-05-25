module Main where

import qualified Src.Parserator as P
import qualified Src.Evaluator as E
import qualified Src.Visualisator as V
import qualified Data.Map as M
import qualified Control.Monad.State as S
import Options.Applicative

newtype Options = Options
    { optFilePath :: FilePath }

maini :: FilePath -> IO ()
maini = processFile

main :: IO ()
main = do
    opts <- execParser optsInfo
    let filepath = optFilePath opts
    processFile filepath

optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Process a .kuifje file"
 <> header "kuifje parser - a simple .kuifje file parser" )

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        ( long "file"
       <> short 'f'
       <> metavar "FILE"
       <> help ".kuifje file to process" )

processFile :: FilePath -> IO ()
processFile filepath = do
    let initialEnv = M.empty
    program <- P.parser filepath
    r <- S.runStateT (E.evaluate program) initialEnv
    V.visualiseEnv (snd r)
    V.visualiseDist "result" (fst r)
