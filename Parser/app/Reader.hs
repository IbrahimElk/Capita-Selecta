
module Reader where 
import qualified System.IO as IO

-- deze programma is verantwoordelijk voor het lezen van een text file 
-- die in string formaat een kuifje programma bevat. 
-- eenmaal gelezen, stuur resultaat naar parser. 
-- indien parser resultaat geeft, print op terminal. 
-- indien niet, error code in terminal. 

-- dit programma heeft dan ook de verantwoordelijkheid van een linker, 
-- de kuifje programmer gaat niet bij elke operatie een distributie bijschrijven
-- definieer een aparte file, die distributies erkent aan de definties/function geschreven
-- in de kuifje taal
-- en de linker zal die twee files samenbrenge, en vervolgens stringifyen voor de parser .

-- mss inderdaad, deze file is liik de gcc, 
-- lees de rauwe data, use the parser 
-- ,and write it into a new file.  

-- then evaluaotrs read this new file with the AST init, 
-- then do computation, 
-- and then write out result in new new file

-- then visualisor reads new file, 
-- and prints visualisations.
-- (maybe saves this visualisation into a png image or something.)



main :: IO ()
main = do  
        contents <- IO.readFile "test.txt"
        print contents
