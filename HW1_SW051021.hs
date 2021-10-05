{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

-- Определите функцию вычисляющую двойной факториал

doubleFact :: Integer -> Integer
doubleFact n = if n > 2
               then n * (doubleFact (n - 2))
               else if n > 0
               then n
               else 1





-- Реализуйте функцию, находящую элементы следующей рекуррентной последовательности
-- b_0 = 1,
-- b_1 = 2,
-- b_2 = 3,
-- b_{k+3} = b_{k+2} - 2 b_{k+1} + 3 b_{k}

func 0 t c p = p
func 1 t c p = c
func 2 t c p = t
func n t c p = func (n-1) (t - 2 * c + 3 * p) t c
seqB :: Integer -> Integer
seqB n = func n 3 2 1




-- Понятие чисел Фибоначчи можно расширить, потребовав, чтобы рекуррентное 
-- соотношение выполнялось для произвольных целых значений аргумента, в том числе 
-- и отрицательных. Реализуйте функцию, вычисляющую числа Фибоначчи так, чтобы
-- она удовлетворяла этому требованию

func 0 c p = p
func 1 c p = c
func n c p = if n > 0
             then func (n - 1) (p + c) c
             else func (n + 1) (p - c) c
fibonacci :: Integer -> Integer
fibonacci n = func n 1 0





-- Реализуйте функцию, находящую сумму и количество цифр заданного целого числа

func 0 c p = (c, p)
func n c p = if n < 10
             then func 0 (n + c) p
             else func (div n 10) (c + (mod n 10)) (p + 1)

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = func (abs x) 0 1





-- Реализуйте функцию, находящую значение определённого интеграла от заданной
-- функции на заданном интервале методом трапеций

func 0 f a b = 0
func n f a b = (func (n - 1) f (a + ((b - a) / n)) b) + ((b - a) / n) * ((f a) + (f (a + ((b - a) / n)))) / 2


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = func 1000 f a b





-- Найдите количество четных элементов в заданном списке целых чисел

func n l = if l == []
           then n
           else 
               if ((mod (head l) 2) == 0)
                   then func (n + 1) (tail l)
                   else func n (tail l)
countEven l = func 0 l

countEven1 [] = 0
countEven1 (x : xs) = if ((mod x 2) == 0)
                      then ((countEven1 xs) + 1)
                      else (countEven1 xs)





-- Сформируйте новый список целых чисел, содержащий только нечетные элементы исходного

oddElemsFrom [] = []
oddElemsFrom (x : xs) | odd x = x : rest
                      | otherwise = rest
    where rest = oddElemsFrom xs





-- Даны два списка целых чисел. Сформируйте список, каждый элемент
-- которого равен разности соответствующих элементов исходных списков. 
-- Если из-за разницы в длинах соответствующий элемент отсутствует, то предполагается,
-- что он равен нулю

subtractLists (x : xs) (y : ys) = (x - y) : subtractLists xs ys
subtractLists xs [] = xs
subtractLists [] (y : ys) = (-y) :  subtractLists [] ys





-- Для данного список построить список пар: элемент, его порядковый
номер

enumerateElems xs = zip xs [0..]





-- Сформируйте список, содержащий подсписок длины n исходного списка, начиная 
-- с k-го элемента исходного. Функция должна быть тотальной, возвращающей
-- последовательные элементы из пересечения диапазонов индексов [k, k + n)
-- и [0, m), где m — длина исходного списка

sublistOfLenFrom' n k = take (min n (n + k)0 . drop k