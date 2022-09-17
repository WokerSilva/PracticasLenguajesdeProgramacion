module Interp where

import Grammars

interp :: ASA -> Int
interp (Num n) = n
interp (Suma x y) = interp x + interp y
interp (Resta x y) = interp x - interp y
interp (Mult x y) = interp x * interp y
interp (Div x y) = let xv = (interp x)
                       yv = (interp y) in 
                       if yv /= then (div xv yx)
                        else error "División no posible"