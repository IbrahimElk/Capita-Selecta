module Parser.Src.Visualisator (visualiseDist, visualiseEnv) where

-- https://hackage.haskell.org/package/gnuplot-0.1/docs/Graphics-GNUPlot-Simple.html

-- for each variable do the following in a .log file
-- some information about the variable. 
-- for example, which values this variable can take.

import Parser.Src.Representation (Prob, Dist(..), Env)
import qualified Graphics.Gnuplot.Simple as G
import qualified Data.Map as M
import Data.Ratio ((%))


plotHistogram :: String -> Dist Int -> IO ()
plotHistogram varName (D outcomes) = do
    let values = map fst (M.toList outcomes)
    let probabilities = map snd (M.toList outcomes)
    let dataPoints = zip values probabilities

    let title = "Distribution of Outcomes for variable " ++ varName
    let xlabel = "Values"
    let ylabel = "Probabilities"
    let plotStyle = G.defaultStyle

    G.plotListStyle [G.Title title, G.XLabel xlabel, G.YLabel ylabel] plotStyle dataPoints
    -- G.plotList [] dataPoints

generateHistogram :: Dist Int -> String
generateHistogram (D outcomes) =
    "Calculator Result Histogram:\n" ++
    concatMap (\(x, p) -> show x ++ ": " ++ replicate (round (fromRational p * 100)) '*' ++ "\n") (M.toList outcomes)

expectedValue :: Dist Int -> Double
expectedValue (D outcomes) =
    fromRational $ sum [fromIntegral x * p | (x, p) <- M.toList outcomes]

variance :: Dist Int -> Double
variance dist =
    let mean = expectedValue dist
    in sum [fromRational p * ((fromIntegral x - mean) ^ 2) | (x, p) <- M.toList (runD dist)]

standardDeviation :: Dist Int -> Double
standardDeviation dist = sqrt (variance dist)

possibleValues :: Dist Int -> [Int]
possibleValues (D outcomes) = [x | (x, _) <- M.toList outcomes]

sumProbs :: Dist Int -> Double
sumProbs (D dist) = fromRational $ sum (M.elems dist)

logVariableInfo :: String -> Dist Int -> IO ()
logVariableInfo varName dist = do
    let hist = generateHistogram dist
    let totalProb = sumProbs dist 
    let mean = expectedValue dist
    let pv = possibleValues dist
    let var = variance dist
    let stdDev = standardDeviation dist
    let logContent = unlines [
            "Variable: " ++ varName,
            "Total Prob: " ++ show totalProb, 
            "Possible values with probabilities:",
            show pv,
            hist,
            "Metrics:",
            "Expected value (mean): " ++ show mean,
            "Variance: " ++ show var,
            "Standard deviation: " ++ show stdDev,
            "\n"
            ]
    plotHistogram varName dist
    appendFile (varName ++ ".log") logContent

visualiseEnv :: Env -> IO ()
visualiseEnv env = mapM_ (uncurry logVariableInfo) (M.toList env)

type FileName = String 
visualiseDist :: FileName -> Dist Int -> IO ()
visualiseDist = logVariableInfo


-- tests: 
-- exampleDist :: Dist Int
-- exampleDist = D $ M.fromList [(1, 1 % 4), (2, 1 % 2), (3, 1 % 4)]

-- exampleEnv :: Env
-- exampleEnv = M.fromList [("exampleVar", exampleDist)]
