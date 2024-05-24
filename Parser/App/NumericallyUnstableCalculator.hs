module Parser.App.Main (main) where

import qualified Parser.Src.Parser as P
import qualified Parser.Src.Evaluator as E
import qualified Parser.Src.Visualisator as V
import qualified Data.Map as M
import qualified Control.Monad.State as S

main :: IO ()
main = do
    let filename = "Parser/App/program.kuifje" 
    -- let filename = "Parser/App/program2.kuifje" 
    let initialEnv = M.empty
    program <- P.parser filename
    r <- S.runStateT (E.evaluate program) initialEnv
    V.visualiseEnv (snd r)
    V.visualiseDist "result" (fst r) 