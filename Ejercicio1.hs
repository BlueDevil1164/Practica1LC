--Variables ---------------------------------------------------------
--Un tipo de datos para variables:
type Variable= String
--OTRAS opciones para definir Variable:
type Variable2= Int
data Variable3= X | V Variable3 deriving (Eq,Show)
data Variable4= X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 deriving (Eq,Show) --Diez variables
