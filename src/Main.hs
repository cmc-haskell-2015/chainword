--import Data.Map
--import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO
import System.IO.Unsafe





answPath= "answers.txt"
questPath= "questions.txt"

fileToStr:: String -> String
fileToStr s  = unsafePerformIO (hGetContents (unsafePerformIO (openFile s ReadMode)))

find:: (Eq a) => [a] -> a -> Int
find xs e 
    | elem e xs = ffind xs e 
    | otherwise = length xs 
    where 
        ffind (x:xs) e
                | x == e    = 0
                | otherwise     = 1 + ffind xs e


parseInput:: String -> [String]
parseInput s
    | null s = [] 
    | otherwise= (take (find s '\n') s): (parseInput (drop ((find s '\n')+1) s))

answList= (parseInput (fileToStr answPath))
questList= (parseInput (fileToStr questPath))

main = do 
    --print (parseInput (fileToStr answPath))
    print "type number->"
    n <- getLine
    print (questList !! read n)
    print (show (length (answList !! read n))++ "  letters")
    print "type answer->"
    an <- getLine
    print ((answList !! read n)== an)







chEq::[Char]->[Char]->Bool
chEq [] [] = True
chEq (x:xs) (y:ys)| x==y = chEq xs ys
			      | otherwise = False



