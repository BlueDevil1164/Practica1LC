Definir la función
-- eliminaEquivalencias :: Prop -> Prop
-- tal que (eliminaEquivalencias f) es una fórmula equivalente a f sin
-- signos de equivalencia. Por ejemplo,
-- eliminaEquivalencias (p <--> q)
-- ==> ((p --> q) /\ (q --> p))
-- eliminaEquivalencias ((p <--> q) /\ (q <--> r))
-- ==> (((p --> q) /\ (q --> p)) /\ ((q --> r) /\ (r --> q)))
-- ---------------------------------------------------------------------
eliminaEquivalencias :: Prop -> Prop
eliminaEquivalencias (Atom f) =
(Atom f)
eliminaEquivalencias (Neg f) =
Neg (eliminaEquivalencias f)
eliminaEquivalencias (Conj f g) =
Conj (eliminaEquivalencias f) (eliminaEquivalencias g)
eliminaEquivalencias (Disj f g) =
Disj (eliminaEquivalencias f) (eliminaEquivalencias g)
eliminaEquivalencias (Impl f g) =
Impl (eliminaEquivalencias f) (eliminaEquivalencias g)
eliminaEquivalencias (Equi f g) =
Conj (Impl (eliminaEquivalencias f) (eliminaEquivalencias g))
(Impl (eliminaEquivalencias g) (eliminaEquivalencias f))
