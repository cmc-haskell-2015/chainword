import qualified Data.Map as Map
import qualified Data.Set as Set
import Graphics.Gloss
--import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO
import System.IO.Unsafe
import Data.Monoid



-- константы
answPath= "Ответы.txt"
questPath= "Вопросы.txt"
fieldDim= 10:: Int
winsize= 500:: Int
cellsize= div winsize fieldDim

--загружаем вопросы и ответы в списки строк
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

allprint:: [String]-> Int-> IO()
allprint [] _ = return ()
allprint xs n= do
    putStrLn (show(n)++ "." ++ (head xs))
    allprint (tail xs) (n+1)

--строим gui

--состояние поля- это массив из введенных букв
data World= World {
    xs::[Maybe Char], --список введенных букв
    flag::Bool -- флаг нажатия мыши, если он тру то ожидаем символ
}
    

--Начальное состояние поля
initial:: World
initial= World (  [Just 'a'] ++ [Just 'b'] ++ replicate (fieldDim^2-2) Nothing ) False

-- индексы в квких клетках должны стоять номера ответов
getIndex:: [String]-> Int-> [Maybe Int]
getIndex [] _ = []
getIndex xs n = [Just n] <> (replicate (length (head xs) - 2) Nothing ) <> getIndex (tail xs)  (n+1)

indexes= getIndex answList 1


--функция отрисовки, преобразует world в Picture- встроенный тип gloss, который им отрисовывается
render:: World-> Picture
render (World xs _)= grid<>numbers<>words
    where 
        grid= makeLines fieldDim<> makePath
        numbers= mconcat [translate (fst $ cellPos n) (snd $ cellPos n) $ scale 0.1 0.1 $ Text $ show k | n<- [0..length indexes -1], Just k<- [indexes!!n] ]
        words= mconcat [ translate (fst $ cellCenter n) (snd $ cellCenter n) $ scale 0.2 0.2 $ Text $ [ch] | n<- [0..length xs -1], Just ch<- [xs!!n]]


--добавляем к рисунку разлиновку на клетки
makeLines:: Int-> Picture
makeLines n 
        |n<0 = Blank
        |otherwise= (color black (line [ (linePos 0, linePos n), (linePos fieldDim,linePos n)]))<>
                    (color black (line [ (linePos n,linePos 0), (linePos n,linePos fieldDim)]))<> (makeLines (n-1))




-- добавляем к картинке рисунок змейки чайнворда
makePath:: Picture
makePath= mconcat [ color red (line [(linePos 0, linePos n), (linePos (fieldDim-1), linePos n)] ) | n<- [1,3..fieldDim-1] ]<>
            mconcat [color red (line [(linePos 1, linePos n), (linePos fieldDim, linePos n)]) | n<- [2,4..fieldDim-2]]<>
            mconcat [color red (line [(linePos 0, linePos n), (linePos fieldDim, linePos n)] ) | n<- [0,fieldDim]]<>
            mconcat [color red (line [(linePos n,linePos 0), (linePos n,linePos fieldDim)]) | n<- [0,fieldDim]]



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
handler:: Event-> World-> World
--handler (EventKey (MouseButton LeftButton) Up _ (x, y))
handler _= id



--функция вызываемая 30 раз в секунду, в нашем случае тождественная
updater:: Float-> World->World
updater _= id



--вызов главной функции 
startplay=  
    play (InWindow "chainword" (winsize,winsize) (500, 500)) white 30 initial render handler updater

main = do
   -- putStrLn "Список вопросов"
   allprint questList 1
   startplay
	



