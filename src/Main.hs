import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Applicative

import Graphics.Gloss
--import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO
import Data.Monoid



-- константы
answPath= "examples/Ответы.txt"
questPath= "examples/Вопросы.txt"
fieldDim= 10:: Int
winsize= 500:: Int
cellsize= div winsize fieldDim
maxTime= 1.0*60

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

allprint:: [String]-> Int-> IO()
allprint [] _ = return ()
allprint xs n= do
    putStrLn (show(n)++ "." ++ (head xs))
    allprint (tail xs) (n+1)

--строим gui


--состояние поля- это массив из введенных букв
data World= World {
    xs::[Maybe Char], --список введенных букв
    cur_field::Maybe Int, -- номер поля если произошло нажатие
    colours::[Color],   -- цвета букв
    answers::[String],   -- ответы на вопросы
    time::Float        --время отгадывания
}    
    

--Начальное состояние поля
initial :: [String] -> World
initial as = World (replicate (fieldDim^2) Nothing ) Nothing (replicate (fieldDim^2) black) as 0.0

--функция отрисовки, преобразует world в Picture- встроенный тип gloss, который им отрисовывается
render:: World-> Picture
render (World xs _ colours answList tm)
    |tm > maxTime = scale 0.15 0.15 $ color red $ Text "YOUR TIME IS UP!!!"
    |otherwise = grid<>numbers<>words
    where 
        grid= makeLines fieldDim<> makePath
        numbers= mconcat [translate (fst $ cellPos n) (snd $ cellPos n + 2) $ scale 0.1 0.1 $ Text $ show k | n<- [0..length indexes -1], Just k<- [indexes!!n] ]
        words= mconcat [color col $ translate (fst $ cellCenter n) (snd $ cellCenter n) $ scale 0.2 0.2 $ Text $ [ch] | n<- [0..length xs -1], Just ch<- [xs!!n], col<- [colours!!n]]
        indexes= getIndex answList 1

-- индексы в каких клетках должны стоять номера ответов
getIndex:: [String]-> Int-> [Maybe Int]
getIndex [] _ = []
getIndex xs n = [Just n] <> (replicate (length (head xs) - 2) Nothing ) <> getIndex (tail xs)  (n+1)

--добавляем к рисунку разлиновку на клетки
makeLines:: Int-> Picture
makeLines n 
        |n<0 = Blank
        |otherwise= (color (makeColor 0.5 0.5 0.5 0.3) (line [ (linePos 0, linePos n), (linePos fieldDim,linePos n)]))<>
                    (color (makeColor 0.5 0.5 0.5 0.3) (line [ (linePos n,linePos 0), (linePos n,linePos fieldDim)]))<> (makeLines (n-1))




-- добавляем к картинке рисунок змейки чайнворда
makePath:: Picture
makePath= mconcat [color black (line [(linePos 0, linePos n), (linePos (fieldDim-1), linePos n)] ) | n<- [1,3..fieldDim-1] ]<>
            mconcat [color black (line [(linePos 1, linePos n), (linePos fieldDim, linePos n)]) | n<- [2,4..fieldDim-2]]<>
            mconcat [color black (line [(linePos 0, linePos n), (linePos fieldDim, linePos n)] ) | n<- [0,fieldDim]]<>
            mconcat [color black (line [(linePos n,linePos 0), (linePos n,linePos fieldDim)]) | n<- [0,fieldDim]]



-- Функции высчитывания кординат
--клетки нумеруются от левого нижнего угла по змейке, линии слева направо, снизу вверх
--координаты линии по номеру
linePos:: Int-> Float
linePos n = fromIntegral(- div winsize 2 + n* cellsize)

--координаты левого нижнего угла клетки
cellPos:: Int-> (Float,Float)
cellPos n  |even y =   ( linePos x, linePos y)
              |otherwise = (linePos $ fieldDim-x-1, linePos y)
                where
                    y= div n fieldDim
                    x= mod n fieldDim

-- координаты середины клетки
cellCenter:: Int-> (Float,Float)
cellCenter n= (x+ l,y+l) 
    where
        x= fst $ cellPos n
        y= snd $ cellPos n
        l= fromIntegral $ div cellsize 2



--обработчик события 
--дописать
--по сути нужно просто изменять массив в world , занести туда букву в позицию соответствующую номеру нажатой клетки
handler:: Event->World->World
handler (EventKey (MouseButton LeftButton) Up _ (x, y)) (World xs _ col as tm) = World  xs (findCell (x, y) 0) col as tm
handler (EventKey (Char key) Up _ _) (World xs (Just n) col as tm)
                                                        |n> fieldDim^2-1 = World newXs (Just n) (chCols as newXs 0 blacks) as tm
                                                        |otherwise = World newXs (Just $ n+1) (chCols as newXs 0 blacks) as tm
                                                        where
                                                            blacks= replicate (fieldDim^2) black
                                                            newXs| n> fieldDim^2 - 1 = ins xs  (Just ch) $ Just (fieldDim^2 -1)
                                                                 |otherwise = (ins xs  (Just ch) $ Just n)
                                                            ch|(key== '0') && (n< length ansLine) = ansLine!!n
                                                              |(key=='0') && (n>= length ansLine)= ' '
                                                              |otherwise = key
                                                            ansLine= as!!0 <> mconcat [  tail w |w<- tail as]
handler _ w = w





--вставка элемента в список в нужную позицию
ins::[a]->a-> Maybe Int->[a]
ins xs _ Nothing = xs
ins (x:xs)  c  (Just 0) = (c:xs)
ins (x:xs)  c  (Just n) = (x:(ins xs c $ Just (n-1)))

--вставка в список одинаковых элементов от n до m позиции
insMany:: [a]->a->Int->Int->[a]
insMany xs x n m
    |n > m = xs
    |otherwise= insMany (ins xs x $ Just n) x (n+1) m

--поиск номера клетки по координатам
findCell:: (Float, Float)->Int-> Maybe Int
findCell (x, y) n 
				|n> fieldDim^2-1 = Nothing  
				|otherwise = 
    				if (x <= a) && (y <= b) && (x >= c) && (y >= d)
        				then Just n
        			else findCell (x, y) (n + 1)
    			where
        			a= (fst $ cellPos n) + (fromIntegral cellsize)
        			b= (snd $ cellPos n) + (fromIntegral cellsize)
        			c= fst $ cellPos n
        			d= snd $ cellPos n

--функция вызываемая 30 раз в секунду, в нашем случае тождественная
updater:: Float-> World->World
updater _ (World tp cur cl as tm) = (World tp cur cl as (tm + (1/30)))


--пробегает список ответов, сравнивает с введенным и заполняет массив цветов зеленым в местах правильного ответа
chCols:: [String]-> [Maybe Char]-> Int-> [Color] -> [Color]
chCols as tp n cl 
    |n== (length as) = cl
    |check n as tp = chCols as tp (n+1) $ insMany cl green n1 n2 
    |otherwise= chCols as tp (n+1) cl 
    where
        s= as!!n
        l| n==0 = 0
         | n==1 = length $ head as 
         |otherwise= length $ mconcat [as!!k | k<- [0,1..n-1]]
        n1= l-n
        n2= l-n+ (length s) -1


--функция проверяющая наличие строки из ansqlist в текущей введенной строке world
check:: Int-> [String]-> [Maybe Char]-> Bool
check n as tp= (ans==typ)
                    where
                        s= as!!n
                        ans= map (\x-> Just x) s
                        l| n==0 = 0
                         | n==1 = length $ head as 
                         |otherwise= length $ mconcat [as!!k | k<- [0,1..n-1]]
                        typ= take (length s) (drop (l-n) tp)



--вызов главной функции 
startplay answList=  
    play (InWindow "chainword" (winsize,winsize) (500, 500)) white 30 (initial answList) render handler updater

main = do
   -- putStrLn "Список вопросов"
   questList <- parseInput <$> readFile questPath
   answList  <- parseInput <$> readFile answPath
   allprint questList 1
   startplay answList
	



