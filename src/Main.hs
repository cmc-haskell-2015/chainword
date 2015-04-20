import Data.Map
import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game



answ= ["kot","tort","tok"]


main = do 
    print "type number"
    n <- getLine
    print "type answer"
    an <- getLine
    print ((answ !! read n)== an)

chEq::[Char]->[Char]->Bool
chEq [] [] = True
chEq (x:xs) (y:ys)| x==y = chEq xs ys
			      | otherwise = False



