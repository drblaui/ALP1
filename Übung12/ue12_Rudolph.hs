{-- Functional Pogramming 
    SKI parser und eval Functions
    Original version: WS-08-09 Prof. Dr. Raul Rojas
    modified WS-09-10 by: Prof. Dr. Margarita Esponda
    modified WS-17-18 by: Prof. Dr. Margarita Esponda
--}

module SKII (Expr, Show, parse, transform, eval) where 

-- extended algebraic type for the transform function

data Expr = App Expr Expr | S | K | I | Var String | Lam String Expr | Nil
              deriving Eq

instance Show Expr where show exp = show_expr exp True

show_expr S _ = "S"
show_expr K _ = "K"
show_expr I _ = "I"

show_expr (App x y) True  = "("++(show_expr x False)++(show_expr y True)++")"
show_expr (App x y) False = (show_expr x False)++(show_expr y True)

show_expr (Var x) _ = x

-- returns a list of free variables in an expression

freie::Expr->[String]->[String]

freie (Var x)   bound | element x bound = []
                      | otherwise       = [x]
freie (Lam x y) bound = freie y (x:bound)
freie (App x y) bound = (freie x bound)++(freie y bound)
freie S bound = []
freie K bound = []
freie I bound = []

element x [] = False
element x (y:r) 
           | x==y =  True
           |otherwise = element x r
           
----------------------- Interpreter ----------------------------------

eval::Expr->Bool->Expr

eval S _ = S 
eval K _ = K 
eval I _ = I 

eval (App I x) b = eval x b

eval (App (App K x) y) b = eval x b

eval (App (App (App S f) g) x) b = eval (App (App f x) (App g x)) b

eval (App x y) b
               | (evalx == x) && b = (App x y)
               | (evalx == x) && (not b)  = (App evalx (eval y False))
               | otherwise   = eval (App evalx y) b
                 where evalx = eval x True
eval (Var x) _ = (Var x)
eval x _ = error (show_expr x False)

--------------------------------------------------------------------

transform (Lam x y) = (eliminate x y)
transform (Var x) = Var x
transform (App x y) = App (transform x) (transform y)
transform S = S
transform K = K
transform I = I

eliminate x S = App K S
eliminate x K = App K K
eliminate x I = App K I
eliminate x y | not(element x (freie y [])) = (App K (transform y))
eliminate x (Var y) | x==y      = I
                    | otherwise = (App K (Var y))

eliminate x (Lam y z) = eliminate x (eliminate y z)
eliminate x (App exp (Var y)) | not (elem x (freie exp [])) && x==y  = transform exp
eliminate x (App y z) = (App (App S (eliminate x y)) (eliminate x z))

---------------------------- parser ------------------------------------
{--
Everything I added or changed is to be found in this section.
Also: Please note, that miss Esponda created a show instance
that removes "Var" and "App" from our expressions so to test everything
you have to remove that completley
--}
ski_parser :: String -> Expr
ski_parser str = parse Nil str


parse :: Expr -> String -> Expr
parse Nil [] = emptyExpr
parse expr [] = expr

parse Nil ('(':rest) = parse (parse Nil inside) out
                            where (inside, out) = extract [] rest 0

parse expr (')':rest) = parse expr rest

parse Nil (a:rest) | letter a = parse (Var [a]) rest
                   | ((length rest) == 0) = (char2Exp a)


parse Nil (a:b:rest) | ((expression a) && (expression b)) = parse (App (char2Exp a) (char2Exp b)) rest
                     | ((expression a) && (letter b)) = parse (App (char2Exp a) (Var [b])) rest
                     | ((letter a) && (expression b)) = parse (App (Var [a]) (char2Exp b)) rest
                     | ((letter a) && (letter b)) = parse (App (Var [a]) (Var [b])) rest
                     | otherwise = parse (Var [a]) (b:rest)

parse expr ('(':rest) = parse (App expr (parse Nil inside)) out
                            where (inside, out) = extract [] rest 0

parse expr (a:rest) | (expression a) = parse (App expr (char2Exp a)) rest

parse expr rest = illegalExpr rest

char2Exp :: Char -> Expr
char2Exp 'S' = S
char2Exp 'K' = K
char2Exp 'I' = I

expression :: Char -> Bool
expression x = (x == 'S') || (x == 'I') || (x == 'K')

emptyExpr = error "the empty expression is not a valid SKI-Expression"
notAnumber = error "an empty string is not a number"
illegalExpr str = error ("there is a syntax error in the expression  " ++ str)

------------------------- Auxiliary Functions --------------------------

letter x = element x ['a'..'z']

extract a   []     _  = error "unbalanced parentheses"
extract a (')':b)  0  = (a,b)
extract a (')':b)  n  = extract  (a++")")  b (n-1)
extract a ('(':b)  n  = extract  (a++"(")  b (n+1)
extract a (b:c)    n  = extract  (a++[b])  c  n

--------------- some SKI expresions for testing ------------------------

expand "succ" = "(S(S(KS)K))"
expand "T" =  "I"
expand "F" = "(KI)"
expand "pred" = "(S(S(SI(K(S(S(KS)(S(K(SI))(S(KK)(S(K(S(S(KS)K)))(SI(KK))))))(S(KK)(SI(KK))))))(K(S(SI(K(KI)))(K(KI)))))(K(KI)))"
expand "or" = "(SI(KK))"
expand "and" = "(SS(K(K(KI))))"
expand "*" = "(S(KS)K)"
-- usw.

--------------------------------------------------------------------------
-----------       SKI Parser and Interpreter       -----------------------

skii exp = show_expr (eval (ski_parser exp) False) False

--------------------------------------------------------------------------
--------------------------------------------------------------------------


------------------------- for testing the transform function ----------------------

zzero = transform (Lam "s" (Lam "z" (Var "z")))
succesor = transform (Lam "w" (Lam "x" (Lam "y" (App (Var "x") (App (App (Var "w") (Var "x")) (Var "y"))) )))
mult = transform (Lam "x" (Lam "y" (Lam "z" (App (Var "x") (App (Var "y") (Var "z"))) )))

one = transform (Lam "x" (Lam "z" (App (Var "x") (Var "z"))))
two = transform (Lam "x" (Lam "z" (App (Var "x") (App (Var "x") (Var "z")))))
three = transform (App succesor two)
four = transform (App succesor three)
five = transform (App succesor four)

exp1 = transform          (Lam "x"      (App (Var "y") (App (Var "x") (Var "y"))) ) -- (/x.y(xy))
exp2 = transform (Lam "z" (Lam "x" (App (App (Var "y") (App (Var "x") (Var "y"))) (Var "z")) )) --(/zx.y(xy)z)

true  = transform (Lam "a" (Lam "b" (Var "a")))
false = transform (Lam "a" (Lam "b" (Var "b")))
and2  = transform (Lam "x" (Lam "y" (App (App (Var "x") (Var "y")) false) ))
or2   = transform (Lam "x" (Lam "y" (App (App (Var "x")    true) (Var "y")) ))
not2  = transform (Lam "x" (App (App (Var "x") false) true) )

test_zero = transform (Lam "x" (App(App(App (Var "x") false) not2) false))

par_00 = transform (Lam "z" (App(App (Var "z") zzero) zzero))
phi = transform (Lam "p" (Lam "z" (App ( App (Var "z") (App succesor (App (Var "p") true))) (App (Var "p") true)  )))
predecesor = transform (Lam "n" (App (App (App (Var "n") phi) par_00) false))

greater_eq = transform (Lam "x" (Lam "y" (App test_zero (App (App (Var "x") predecesor) (Var "y"))) ))

rec = transform (Lam "y" (App (Lam "x" (App (Var "y") (App (Var "x") (Var "x"))))
                    (Lam "x" (App (Var "y") (App (Var "x") (Var "x"))))
                ))
                
summe = transform (Lam "r" (Lam "n" 
                  (App (App (App test_zero (Var "n")) zzero)
                  (App (App (Var "n") succesor) (App (Var "r") (App predecesor (Var "n"))))
                    )))
      
factorial = transform (Lam "r" (Lam "n"
                      (App (App (App test_zero (Var "n")) one)
                      (App (App mult (Var "n")) (App (Var "r") (App predecesor (Var "n"))))
                       )))

xc = transform (Lam "f" (App (App (App (Var "f") K) S) K))

-------------------------------------------------------------------
