import Data.List


data List a = Cons a (List a)
            | Nil
              deriving (Show)


fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil



toList (Cons x y) = x : (toList y)
toList Nil        = []


data Tree a = Node a (Tree a) (Tree a)
             | Empty
              deriving (Show)

height Empty               = 0
height (Node x left right) = 1 + max (height left) (height right)

-- data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving Show

quux a = let a = "foo"
         in a ++ "eek!"

len :: [a] -> Int
len (x:xs) = len xs + 1
len []     = 0

mean x = mean_helper x 0 0
         where mean_helper [] _ 0            = Nothing
               mean_helper [] sum count      = Just (sum / count)
               mean_helper (x:xs) sum count  = mean_helper xs (sum + x) 
                                                              (count + 1)

mean2 [] = Nothing
mean2 x  = Just (uncurry (/) (foldl helper (0,0) x))
           where helper (sum, count) elt = (sum + elt, count + 1)

pal :: [a] -> [a]

pal [] = []
pal x = helper [] [] x
    where helper ys [] [] = ys
          helper ys stack (t:ts) = helper (t:ys) (t:stack) ts
          helper ys (s:ss) [] = helper (s:ys) ss []


ispal x = x == reverse x


sortByLen = sortBy (\x y -> compare (length x) (length y))


interspers :: a -> [[a]] -> [a]

interspers _ []       = []
interspers _ (x:[])   = x
interspers sep (x:xs) = x ++ (sep : interspers sep xs)

