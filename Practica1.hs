import SintaxisSemantica
import Data.List
import Test.HUnit
import Verificacion

--Variables (Ejercicio1)---------------------------------------------------------
--Un tipo de datos para variables:
type Variable= String
--OTRAS opciones para definir Variable:
type Variable2= Int
data Variable3= X | V Variable3 deriving (Eq,Show)
data Variable4= X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 deriving (Eq,Show) --Diez variables


-------------------------------------------------------------------------------------------------------
--Valuaciones (truth assignments)Ejercicio2 -----------------------------------
type Valuacion= Variable -> Int
--Otras opciones para definir Valuacion:
type Valuacion2= Variable -> Bool
type Valuacion3= [(Variable,Bool)]
type Valuacion4= [Variable]     -- Los elementos de sigma son las variables verdaderas
type Valuacion5= S.Set Variable -- Los elementos de sigma son las variables verdaderas
type Valuacion6 a= a -> Int
--
-- Ejemplos de valuaciones:
sigma1 :: Valuacion
sigma1 x= case x of
            "x0"    -> 1
            "x1"    -> 0
            _       -> error $ "sigma1 no esta definida para la variable "++(show x)
--
sigma2 :: Valuacion
sigma2 _ = 0
--
sigma3 :: Valuacion
sigma3 x= case x of
            "x0"    -> 0
            _       -> 1

---------------------------------------------------------------------------------------------------------------
Definir la función
-- subconjuntos :: [a] -> [[a]]
-- tal que (subconjuntos x) es la lista de los subconjuntos de x. Por
-- ejmplo,
-- subconjuntos "abc" ==> ["abc","ab","ac","a","bc","b","c",""] Ejercicio3
-- ------------------------------------------------------------------------------------------------------------
subconjuntos :: [a] -> [[a]]
subconjuntos [] = [[]]
subconjuntos (x:xs) = [x:ys | ys <- xss] ++ xss
where xss = subconjuntos xs
---------------------------------------------------------------------------------------------------------------
-- Ejercicio 4: Definir la función
-- modelosFórmula :: Prop -> [Interpretación]
-- tal que (modelosFórmula f) es la lista de todas las interpretaciones
-- de f que son modelo de F. Por ejemplo,
type SímboloProposicional = String

data Prop = Atom SímboloProposicional
| Neg Prop
| Conj Prop Prop
| Disj Prop Prop
| Impl Prop Prop
| Equi Prop Prop
deriving (Eq,Ord)
instance Show Prop where
show (Atom p) = p
show (Neg p) = "no " ++ show p
show (Conj p q) = "(" ++ show p ++ " /\\ " ++ show q ++ ")"
show (Disj p q) = "(" ++ show p ++ " \\/ " ++ show q ++ ")"
show (Impl p q) = "(" ++ show p ++ " --> " ++ show q ++ ")"
show (Equi p q) = "(" ++ show p ++ " <--> " ++ show q ++ ")"
----------------------------------------------------------------------------------------------------------------
Definir la función(Ejercicio5)
-- eliminaImplicaciones :: Prop -> Prop
-- tal que (eliminaImplicaciones f) es una fórmula equivalente a f sin
-- signos de implicación. Por ejemplo,
-- eliminaImplicaciones (p --> q)
-- ==> (no p \/ q)
-- eliminaImplicaciones (eliminaEquivalencias (p <--> q))
-- ==> ((no p \/ q) /\ (no q \/ p))
-- Nota: Se supone que f no tiene signos de equivalencia.
-- ---------------------------------------------------------------------
eliminaImplicaciones :: Prop -> Prop
eliminaImplicaciones (Atom f) = (Atom f)
eliminaImplicaciones (Neg f) = Neg (eliminaImplicaciones f)
eliminaImplicaciones (Conj f g) = Conj (eliminaImplicaciones f) (eliminaImplicaciones g)
eliminaImplicaciones (Disj f g) = Disj (eliminaImplicaciones f) (eliminaImplicaciones g)
eliminaImplicaciones (Impl f g) = Disj (Neg (eliminaImplicaciones f)) (eliminaImplicaciones g)
-----------------------------------------------------------------------------------------------------------------
--Definir la función Nand(Ejercicio6)
-- interiorizaNegación :: Prop -> Prop
-- tal que (interiorizaNegación f) es una fórmula equivalente a f donde
-- las negaciones se aplican sólo a fórmulas atómicas. Por ejemplo,
-- interiorizaNegación (no (p /\ q)) ==> (no p \/ no q)

interiorizaNegación :: Prop -> Prop
interiorizaNegación (Atom f) = (Atom f)
interiorizaNegación (Conj f g) = Conj (interiorizaNegación f) (interiorizaNegación g)
-----------------------------------------------------------------------------------------------------------------

Definir la función(Ejercicio7)
-- esModeloConjunto :: Interpretación -> [Prop] -> Bool
-- tal que (esModeloConjunto i s) se verifica si i es modelo de s. Por
-- ejemplo,
-- esModeloConjunto [p,r] [(p \/ q) /\ ((no q) \/ r), q --> r]
-- ==> True
-- esModeloConjunto [p,r] [(p \/ q) /\ ((no q) \/ r), r --> q]
-- ==> False
-- ---------------------------------------------------------------------
esModeloConjunto :: Interpretación -> [Prop] -> Bool
esModeloConjunto i s = and [esModeloFórmula i f | f <- s]



            
     
