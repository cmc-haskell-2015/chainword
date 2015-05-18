module Generat where

import System.Random
import System.Random.Shuffle (shuffle')

--генерирует нужный кортеж из 2 списков- с вопросами и с ответами 
--берем полный список вопросов и ответов, преобразуем его в список кортежей (вопрос,ответ)
--перемешиваем и составляем цепочку
generate:: RandomGen g => ([String],[String]) -> g -> Int -> ([String],[String])
generate tp g n = convBack $ makeList (tail ql) [head ql] n
    where
        ql= shuffle (convert tp) g


-- строит цепочку по списку пар (вопрос,ответ)
--выбираем каждый следующий элеммент - первый подходящий по первой букве ответа из перемешанного массива
--продолжаем пока не наберем нужную длинну, не более чем n
makeList:: [(String,String)]-> [(String,String)] -> Int -> [(String,String)]
makeList al rs n | new == ([],[]) = rs
                 |otherwise = makeList (delElem al new) (rs ++ [new]) n
                where
                    new = getNext al c l -- добавляемый элемент
                    c= last (snd $ last rs) -- последняя буква последнего элемента текущей цепи
                    l= n - (length (concat $ map snd rs) - (length rs) +1 ) +1  --макс длинна нового слова


-- удаляем из списка заданный элемент
delElem:: Eq a => [a] -> a -> [a]
delElem [] _ = []
delElem xs a | head xs == a = tail xs
             |otherwise = [head xs] ++ (delElem (tail xs) a)

--берет первый элемент подходящий по первой букве ответа, длинны не более n 
getNext:: [(String,String)] -> Char -> Int -> (String,String)
getNext xs c n | null xs = ([],[])
               | (ans!!0 == c) && (length ans < n) = head xs
               |otherwise = getNext (tail xs) c  n
                where
                    ans= snd $ head xs


-- конвертирует кортеж из 2 списков в список бинарных кортежей. 
convert:: ([a],[a]) -> [(a,a)]
convert ([],[])  = []
convert (xs,ys) = [(head xs,head ys)]++ convert (tail xs,tail ys)

--конвертируем обратно
convBack:: [(a,a)] -> ([a],[a])
convBack xs = (map fst xs,map snd xs)

-- перемешивает список 
shuffle:: RandomGen g => [a]-> g -> [a]
shuffle xs g =  shuffle' xs (length xs) g




