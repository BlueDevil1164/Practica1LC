--
--Valuaciones (truth assignments) -----------------------------------
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
