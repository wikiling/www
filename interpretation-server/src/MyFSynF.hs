module MyFSynF
  ( Formula(..),
  )
where

-- syntax of predicate logic

type Name     = String
type Index    = [Int]
data Variable = Variable Name Index deriving (Eq, Ord)

data Term = Var Variable | Struct String [Term]
            deriving (Eq, Ord)

instance Show Term where
  show (Var v)       = show v
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

instance Show Variable where
  show (Variable name [])  = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is ) = name ++ showInts is
     where showInts []     = ""
           showInts [i]    = show i
           showInts (i:is) = show i ++ "_" ++ showInts is

data Formula a = Atom String [a]
               | Eq a a
               | Neg  (Formula a)
               | Impl (Formula a) (Formula a)
               | Equi (Formula a) (Formula a)
               | Conj [Formula a]
               | Disj [Formula a]
               | Forall Variable (Formula a)
               | Exists Variable (Formula a)
               deriving Eq

instance Show a => Show (Formula a) where
    show (Atom s [])  = s
    show (Atom s xs)  = s ++ show xs
    show (Eq t1 t2)   = show t1 ++ "==" ++ show t2
    show (Neg form)   = '~' : (show form)
    show (Impl f1 f2)  = "(" ++ show f1 ++ " -> "
                             ++ show f2 ++ ")"
    show (Equi f1 f2)  = "(" ++ show f1 ++ " <-> "
                             ++ show f2 ++ ")"
    show (Conj [])    = "true"
    show (Conj fs)    = "&" ++ show fs
    show (Disj [])    = "false"
    show (Disj fs)    = "V" ++ show fs
    show(Forall v f)  = "A" ++ show v ++ (' ' : show f)
    show(Exists v f)  = "E" ++ show v ++ (' ' : show f)

--------------------

tx, ty, tz :: Term
tx = Var x
ty = Var y
tz = Var z

x, y, z :: Variable
x = Variable "x" []
y = Variable "y" []
z = Variable "z" []

formula0 = Atom "R" [x,y]
formula1 = Forall x (Atom "R" [x,x])
formula2 = Forall x
            (Forall y
              (Impl (Atom "R" [x,y]) (Atom "R" [y,x])))

formula3 = Conj [Atom "p" [], Neg (Atom "p" [])]
formula4 = Disj [Atom "p1" [], Atom "p2" [], Atom "p3" [], Atom "p4" []]

-- semantics of propositional logic

{-
propNames :: Formula Term -> [String]
propNames (Atom name) = [name]
propNames (Neg f)   = propNames f
propNames (Conj fs) = (sort.nub.concat) (map propNames fs)
propNames (Disj fs) = (sort.nub.concat) (map propNames fs)

genVals :: [String] -> [[(String, Bool)]]
genVals [] = [[]]
genVals (name:names) = map ((name, True) :) (genVals names)
                    ++ map ((name, False):) (genVals names)
            
allVals :: Formula Term -> [[(String, Bool)]]
allVals = genVals . propNames

eval :: [(String,Bool)] -> Formula Term -> Bool
eval [] (Atom c)    = error ("no info about " ++ show c)
eval ((i,b):xs) (Atom c)
     | c == i    = b
     | otherwise = eval xs (Atom c)
eval xs (Neg f)   = not (eval xs f)
eval xs (Conj fs) = all (eval xs) fs
eval xs (Disj fs) = any (eval xs) fs

tautology :: Formula Term -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

satisfiable :: Formula Term -> Bool
satisfiable f = any (\v -> eval v f) (allVals f)

contradiction :: Formula Term -> Bool
contradiction = not . satisfiable

implies :: Formula Term -> Formula Term -> Bool
implies f1 f2 = contradiction (Conj [f1,Neg f2])

update :: [[(String,Bool)]] -> Formula Term -> [[(String,Bool)]]
update vals f = [ v | v <- vals, eval v f ]
-}