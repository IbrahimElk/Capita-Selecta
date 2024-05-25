module Main (main) where

import qualified Parser as P
import qualified Evaluator as E
import qualified Visualisator as V
import qualified Data.Map as M
import qualified Control.Monad.State as S
import Options.Applicative

data Options = Options
    { optFilePath :: FilePath }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        ( long "file"
       <> short 'f'
       <> metavar "FILE"
       <> help ".kuifje file to process" )

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

processFile :: FilePath -> IO ()
processFile filepath = do
    let initialEnv = M.empty
    program <- P.parser filepath
    r <- S.runStateT (E.evaluate program) initialEnv
    V.visualiseEnv (snd r)
    V.visualiseDist "result" (fst r)
