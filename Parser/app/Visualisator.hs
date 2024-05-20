-- responsible for outputting to terminal, graphs, histograms, error codes....

-- TODO: tijd om te visualiseren 
-- TODO: quickeck functies voor alles. 






















-- generateHistogramTerminal :: Dist Int -> IO ()
-- generateHistogramTerminal (D outcomes) =
--     putStrLn "Calculator Result Histogram" >>
--     mapM_ (\(x, p) -> putStrLn (show x ++ ": " ++ replicate (round $ p * 100) '*')) (M.toList outcomes)

-- exampleDist1 :: Dist Int
-- exampleDist1 = D $ M.fromList [(1, 0.2), (2, 0.5), (3, 0.3)]

-- exampleDist2 :: Dist Int
-- exampleDist2 = D $ M.fromList [(0, 0.1), (1, 0.2), (2, 0.3), (3, 0.4)]

-- main2 :: IO ()
-- main2 = do
--     putStrLn "Example 1:"
--     generateHistogramTerminal exampleDist1
--     -- putStrLn "\nExample 2:"
--     -- generateHistogramFigure exampleDist2





-- -- IDEA: 

-- -- Choose a Plotting Library: Select a Haskell plotting library such as haskell-plot or Chart. 
-- -- These libraries provide functions and types for creating a wide range of plots, 
-- -- including histograms, line charts, scatter plots, and heatmaps.

-- -- extra ideen : 
-- -- Extend the Evaluator: Modify the evaluator function to compute additional statistical metrics or 
-- -- data that can be used for plotting. For example, you may calculate the mean, median, variance, or 
-- -- other summary statistics of the stochastic processes or probability distributions.

-- -- PROBLEEM2:
-- -- Hoe samplen van een Dist int type. 
-- -- blijkbaar kun je dat doen met de Dist monad.
-- -- sample :: Dist Int -> Int
-- -- of 
-- -- sample :: Dist Bool -> Bool
-- -- Mailtje sturen naar prof voor deze functie. 

-- -- quickCheck : 

