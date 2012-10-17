module HFKR

where 
import List

prime n = 
  n > 1 && all (\ x -> rem n x /= 0) [2..n-1]

somePrimes    = filter prime [1..1000]

primesUntil n = filter prime [1..n]

allPrimes     = filter prime [1..]

divides :: Integer -> Integer -> Bool
divides m n = rem n m == 0

sqr :: Int -> Int 
sqr = \ x -> x * x 

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

containedIn :: Eq a => [a] -> [a] -> Bool
containedIn xs ys = all (\ x -> elem x ys) xs

type Rel a = [(a,a)]

sameR :: Ord a => Rel a -> Rel a -> Bool
sameR r s = sort (nub r) == sort (nub s)

cnv :: Rel a -> Rel a
cnv r = [ (y,x) | (x,y) <- r ]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

euclR :: Eq a => Rel a -> Bool
euclR r = (cnv r @@ r) `containedIn` r

serialR :: Eq a => Rel a -> Bool
serialR r = 
  all (not.null) 
    (map (\ (x,y) -> [ v | (u,v) <- r, y == u]) r)

data Agent = A | B | C | D | E deriving (Eq,Ord,Enum)

a,alice, b,bob, c,carol, d,dave, e,ernie  :: Agent
a = A; alice = A
b = B; bob   = B
c = C; carol = C
d = D; dave  = D
e = E; ernie = E

instance Show Agent where
  show A = "a"; show B = "b"; show C = "c"; 
  show D = "d" ; show E = "e"

data Prop = P Int | Q Int | R Int deriving (Eq,Ord)

instance Show Prop where 
  show (P 0) = "p"; show (P i) = "p" ++ show i 
  show (Q 0) = "q"; show (Q i) = "q" ++ show i 
  show (R 0) = "r"; show (R i) = "r" ++ show i

data EpistM state = Mo
             [state]
             [Agent]
             [(state,[Prop])]
             [(Agent,state,state)]
             [state]  deriving (Eq,Show)

