{---Don't we need to sort the sparse vector around dimension before doing anything-}


data Dimension = Dim { dimension :: Integer
                     , value :: Double
                     } deriving (Show)

data Vector = Vec [Dimension] deriving (Show)

sqr a = a * a

distance (Vec x) (Vec y) = sqrt (distance' (quicksort (Vec x)) (quicksort (Vec y)))

distance':: Vector -> Vector -> Double
distance' (Vec[]) (Vec []) = 0
distance' (Vec([])) (Vec(x:xs)) =  sqr (value x) + distance' (Vec []) (Vec xs) 
distance' (Vec(y:ys)) (Vec([])) =  sqr (value y) + distance' (Vec ys) (Vec []) 
distance' (Vec(x:xs)) (Vec(y:ys)) 
		| dimension x == dimension y =  sqr (value x - value y) + distance' (Vec xs) (Vec ys)
        | dimension x > dimension y  =  sqr (value y) + distance' (Vec (x:xs)) (Vec ys)
        | otherwise                    =  sqr (value x) + distance' (Vec xs) (Vec (y:ys))


dot :: Vector -> Vector -> Double

dot x y = dot' (quicksort x) (quicksort y)
dot':: Vector -> Vector -> Double
dot' (Vec []) (Vec []) = 0
dot' (Vec _ ) (Vec []) = 0
dot' (Vec []) (Vec _) = 0
dot' (Vec(x:xs)) (Vec(y:ys)) 
		| dimension x == dimension y = (value x * value y) + dot' (Vec xs) (Vec ys)
        | dimension x > dimension y  = dot' (Vec (x:xs)) (Vec ys)
        | otherwise                  = dot' (Vec xs) (Vec (y:ys))

add:: Vector -> Vector -> Vector
add x y = Vec $ add' (quicksort x) (quicksort y)

add':: Vector -> Vector -> [Dimension]
add' (Vec []) (Vec [])= []
add' (Vec([]))  (Vec(x:xs)) = (x:xs)
add' (Vec(y:ys)) (Vec [])  =(y:ys)
add' (Vec(x:xs)) (Vec(y:ys)) 
		| dimension x == dimension y = [Dim (dimension x) (value x + value y)] ++  add' (Vec xs) (Vec ys)
        | dimension x > dimension y = [Dim (dimension y) (value y)] ++  add' (Vec (x:xs)) (Vec ys)
        | otherwise                 = [Dim (dimension x) (value x)]  ++  add' (Vec xs) (Vec (y:ys))


quicksort :: Vector -> Vector
quicksort a = Vec $ quicksort' a

quicksort' :: Vector -> [Dimension]
quicksort' (Vec [])     =  []
quicksort' (Vec (p:xs)) = (quicksort' (Vec lesser)) ++ [p] ++ (quicksort' (Vec greater))
    where
        lesser  = filter les  xs 
        greater = filter gre  xs
        les x = (dimension p) > (dimension x)
        gre x = (dimension p) < (dimension x)

magnitude :: Vector -> Double
magnitude (Vec []) = 0
magnitude (Vec (x:xs)) = sqrt (sqr (value x) + sqr ( magnitude (Vec xs)))

isUnitVec :: Vector -> Bool
isUnitVec v = (magnitude v ) == 1

constMul::Vector -> Double -> Vector
constMul (Vec a) mul = 	Vec $ map (dimmul mul) a

dimmul:: Double -> Dimension->Dimension
dimmul mul (Dim x y) = Dim x (mul*y)


convertUnit::Vector->Vector
convertUnit v1 = let mag = magnitude v1
			in constMul v1 (1/mag)
			
cross:: Vector -> Vector -> Vector
cross (Vec []) (Vec []) = (Vec [])
cross (Vec _ ) (Vec []) = (Vec [])
cross (Vec []) (Vec _) = (Vec [])
cross (Vec (x:xs)) (Vec (y:ys))
				| dimension x ==1 = add (constMul (crossWith_i (Vec (y:ys))) (value x) ) (cross (Vec xs) (Vec (y:ys)))
				| dimension x ==2 = add (constMul (crossWith_j (Vec (y:ys))) (value x) ) (cross (Vec xs) (Vec (y:ys)))
				| dimension x ==3 = add (constMul (crossWith_k (Vec (y:ys))) (value x) ) (cross (Vec xs) (Vec (y:ys)))
				| otherwise	= cross (Vec xs) (Vec (y:ys))
				
crossWith_i ::  Vector -> Vector 
crossWith_i (Vec []) = (Vec [])
crossWith_i (Vec (x:xs))
	| dimension x ==1 = crossWith_i(Vec xs)
	| dimension x ==2 = add (Vec [Dim (3) (value x)]) (crossWith_i (Vec xs))
	| dimension x ==3 = add (Vec [Dim (2) ((value x)*(-1))]) (crossWith_i (Vec xs))
	| otherwise		= crossWith_i (Vec xs)

crossWith_j ::  Vector -> Vector 
crossWith_j (Vec []) = (Vec [])
crossWith_j (Vec (x:xs))
	| dimension x ==2 = crossWith_j(Vec xs)
	| dimension x ==3 = add (Vec [Dim (1) (value x)]) (crossWith_j (Vec xs))
	| dimension x ==1 = add (Vec [Dim (3) ((value x)*(-1))]) (crossWith_j (Vec xs))
	| otherwise		= crossWith_j (Vec xs)

crossWith_k ::  Vector -> Vector 
crossWith_k (Vec []) = (Vec [])
crossWith_k (Vec (x:xs))
	| dimension x ==3 = crossWith_k(Vec xs)
	| dimension x ==1 = add (Vec [Dim (2) (value x)]) (crossWith_k (Vec xs))
	| dimension x ==2 = add (Vec [Dim (1) ((value x)*(-1))]) (crossWith_k (Vec xs))
	| otherwise		= crossWith_k (Vec xs)
				

a = take 100000 ([1..])
b = take 100000 ([(1.0),(1.1)..])

c = take 100000 ([(2.0),(2.4)..])

x = temp a b
y = temp a c

a' = take 10000 ([1..])
b' = take 10000 ([(1.0),(1.1)..])

c' = take 10000 ([(2.0),(2.4)..])

x' = temp a' b'
y' = temp a' c'

p = take 1000 ([1..])
q = take 1000 ([(1.0),(1.1)..])

r = take 1000 ([(2.0),(2.4)..])

s = temp p q
t = temp p r


temp' [] [] = []
temp' (x:xs) (y:ys) = ([(Dim x y)] ) ++ (temp' xs ys)

temp x y = Vec (temp' x y)

