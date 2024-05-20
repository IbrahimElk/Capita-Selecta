module Visualisator (main) where

-- https://hackage.haskell.org/package/gnuplot-0.1/docs/Graphics-GNUPlot-Simple.html
import qualified Graphics.Gnuplot.Simple as G
import qualified Data.Map as M
import Data.Ratio ((%))

type Prob = Rational
newtype Dist a = D { runD :: M.Map a Prob }
  deriving (Show)
type Env = M.Map String (Dist Int)


-- for each variable do the following in a .log file
-- some information about the variable. 
-- for example, which values this variable can take.
plotHistogram :: Dist Int -> IO ()
plotHistogram (D outcomes) = do
    let values = map fst (M.toList outcomes)
    let probabilities = map snd (M.toList outcomes)
    let dataPoints = zip values probabilities
    G.plotList [] dataPoints

generateHistogram :: Dist Int -> String
generateHistogram (D outcomes) =
    "Calculator Result Histogram:\n" ++
    concatMap (\(x, p) -> show x ++ ": " ++ replicate (round (fromRational p * 100)) '*' ++ "\n") (M.toList outcomes)

expectedValue :: Dist Int -> Double
expectedValue (D outcomes) =
    fromRational $ sum [fromIntegral x * p | (x, p) <- M.toList outcomes]

-- variance
variance :: Dist Int -> Double
variance dist =
    let mean = expectedValue dist
    in sum [fromRational p * ((fromIntegral x - mean) ^ 2) | (x, p) <- M.toList (runD dist)]

-- standard deviation
standardDeviation :: Dist Int -> Double
standardDeviation dist = sqrt (variance dist)

possibleValues :: Dist Int -> [Int]
possibleValues (D outcomes) = [x | (x, _) <- M.toList outcomes]

-- Function to log information about the variable
logVariableInfo :: String -> Dist Int -> IO ()
logVariableInfo varName dist = do
    let hist = generateHistogram dist
    let mean = expectedValue dist
    let pv = possibleValues dist
    let var = variance dist
    let stdDev = standardDeviation dist
    let logContent = unlines [
            "Variable: " ++ varName,
            "Possible values with probabilities:",
            show pv,
            hist,
            "Metrics:",
            "Expected value (mean): " ++ show mean,
            "Variance: " ++ show var,
            "Standard deviation: " ++ show stdDev,
            "\n"
            ]
    plotHistogram dist
    appendFile (varName ++ ".log") logContent

exampleDist :: Dist Int
exampleDist = D $ M.fromList [(1, 1 % 4), (2, 1 % 2), (3, 1 % 4)]

exampleEnv :: Env
exampleEnv = M.fromList [("exampleVar", exampleDist)]

main :: IO ()
main = mapM_ (uncurry logVariableInfo) (M.toList exampleEnv)