-- 1. Aufgabe
data Length = Foot Double | Centimeter Double 
            | Yard Double | Kilometer Double 
            | Inch Double | Meter Double
            | Mile Double
            deriving Show

foot2cm :: Length -> Length
foot2cm (Foot ft) = Centimeter (ft * 30.48)

inch2cm :: Length -> Length
inch2cm (Inch ic) = Centimeter (ic * 2.54)

yard2m :: Length -> Length
yard2m (Yard yd) = Meter (yd / 1.0936)

mile2km :: Length -> Length
mile2km (Mile mi) = Kilometer (mi / 0.62137)
