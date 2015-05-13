import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Applicative

import Graphics.Gloss
--import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO
import Data.Monoid

check:: Int-> [String]-> [Maybe Char]-> Bool
check n as tp= (ans==typ)
                    where
                        s= as!!n
                        ans= map (\x-> Just x) s
                        l| n==0 = 0
                         | n==1 = length $ head as 
                         |otherwise= length $ mconcat [as!!k | k<- [0,1..n-1]]
                        typ= take (length s) (drop (l-n) tp)



chCols:: [String]-> [Maybe Char]-> Int-> [Color]
chCols as tp n 
    |n== (length as) = []
    |check n as tp = (replicate ((length $ as!!n)-n ) green) ++ (chCols  as tp (n+1) )
    |otherwise= (replicate ((length $ as!!n)-n) black) ++ (chCols as tp (n+1))



    --check n as tp = (replicate ((length $ as!!n)-k ) green) ++ (chCols  as tp (n+1) )
--otherwise= (replicate ((length $ as!!n)-k) black) ++ (chCols as tp (n+1))