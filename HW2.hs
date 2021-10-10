-- Реализуйте функцию cmp, сравнивающую элементы типа LogLevel так, чтобы имел место
-- порядок Error > Warning > Info

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp _ Error = LT
cmp Info _ = LT
cmp _ Info = GT





-- Реализуйте функцию abbrFirstName, которая сокращает имя до первой буквы с точкой,
-- то есть если имя было "John", то после применения этой функции, оно превратится в "J.".
-- Однако если имя было короче двух символов, то оно не меняется.

data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName (Person [] ln a) = Person [] ln a
abbrFirstName (Person (x : []) ln a) = Person [x] ln a
abbrFirstName (Person (x : y : []) ln a) = Person ([x, y]) ln a
abbrFirstName (Person (x : y : xs) ln a) = Person ([x, '.']) ln a





-- Напишите функции, которые вычисляют сумму элементов дерева и максимальную высоту дерева:

data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node left a right) = a + (treeSum left) + (treeSum right)

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node left _ right) = 1 + (max (treeHeight left) (treeHeight right))





-- Составить список сумм соответствующих элементов трех заданных списков.
-- Длина результирующего списка должна быть равна длине самого длинного из
-- заданных списков, при этом "закончившиеся" списки не должны давать вклада в суммы.

sum3 :: Num a => [a] -> [a] -> [a] -> [a]

sum3 [] [] [] = []

sum3 (x : xs) [] [] = x : (sum3 xs [] [])
sum3 [] (x : xs) [] = x : (sum3 [] xs [])
sum3 [] [] (x : xs) = x : (sum3 [] [] xs)

sum3 (x : xs) (y : ys) [] = (x + y) : (sum3 xs ys [])
sum3 [] (x : xs) (y : ys) = (x + y) : (sum3 [] xs ys)
sum3 (y : ys) [] (x : xs) = (x + y) : (sum3 ys [] xs)

sum3 (x : xs) (y : ys) (z : zs) = (x + y + z) : (sum3 xs ys zs)





-- Сформируйте список цифр заданного целого числа.

digits :: Integer -> [Integer]

digits x = if ((abs x) < 10)
           then [abs x]
           else (digits (div (abs x) 10)) ++ [mod (abs x) 10]





-- Определите, содержит ли заданное целое число все цифры от 1 до 9.

digits :: Integer -> [Integer]

digits x = if ((abs x) < 10)
           then [abs x]
           else (digits (div (abs x) 10)) ++ [mod (abs x) 10]

is a [] = False
is a (x : xs) = if (x == a)
                then True
                else (is a xs)

isList x = and [is 1 x, is 2 x, is 3 x, is 4 x, is 5 x, is 6 x, is 7 x, is 8 x, is 9 x]
containsAllDigits :: Integer -> Bool
containsAllDigits x = isList (digits x)





-- Определите, содержит ли заданное целое число все цифры от 1 до 9 в точности
-- по одному разу.

digits :: Integer -> [Integer]

digits x = if ((abs x) < 10)
           then [abs x]
           else (digits (div (abs x) 10)) ++ [mod (abs x) 10]

is a [] b = b
is a (x : xs) False = if (x == a)
                      then (is a xs True)
                      else (is a xs False)
is a (x : xs) True = if (x == a)
                      then False
                      else (is a xs True)

isList x = and [is 1 x False, is 2 x False, is 3 x False, is 4 x False, is 5 x False, is 6 x False, is 7 x False, is 8 x False, is 9 x False]
containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes x = isList (digits x)





-- Из заданного списка выделите подсписок, состоящий из элементов с n-го номера
-- по k-ый, считая от нуля. При этом n-ый элемент должен входить в результат, а
-- k-ый - нет.

func i n k [] = []

func i n k (x : xs) = if (i >= k)
                      then []
                      else
                          if (i < n)
                          then func (i + 1) n k xs
                          else
                              [x] ++ func (i + 1) n k xs

sublist :: Int -> Int -> [a] -> [a]
sublist n k x = func 0 n k x





-- Повторите каждый элемент списка заданное число раз.

func 0 _ = []
func k x = [x] ++ (func (k - 1) x)

repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem k [] = []
repeatEveryElem k (x : xs) = (func k x) ++ (repeatEveryElem k xs)





-- Дан список (возможно бесконечный) [a_1, a_2, ...] и положительное целое число n.
-- Создайте спиcок "скользящих" подсписков длины n, то есть список списков следующего
-- вида: [[a_1, ... , a_{n}], [a_2, ... , a_{n+1}], [a_3, ... , a_{n+2}], ... ]


func i n k [] = []

func i n k (x : xs) = if (i >= k)
                      then []
                      else
                          if (i < n)
                          then func (i + 1) n k xs
                          else
                              [x] ++ func (i + 1) n k xs

sublist :: Int -> Int -> [a] -> [a]
sublist n k x = func 0 n k x

func1:: Int -> Int -> [a] -> [[a]]
func1 n k x = if ((n + k) > (length x))
              then [[]]
              else [(sublist n (n + k) x)] ++ (func1 (n + 1) k x)

movingLists1 :: Int -> [a] -> [[a]]
movingLists1 k x = init (func1 0 k x)

-- работает быстрее:

movingLists :: Int -> [a] -> [[a]]
movingListsW :: Int -> [a] -> [[a]]
movingListsW _ [] = [[]]
movingListsW n (x : xs) = if (length (take n (x : xs)) < n)
                          then [[]]
                          else [take n (x : xs)] ++ (movingListsW n xs)
movingLists _ [] = [[]]
movingLists n x = init (movingListsW n x)