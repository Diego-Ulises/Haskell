potencia b e = if(e==0) then 1 else b*(potencia b (e-1))

fact n = if(n==0) then 1 else n * fact (n-1) 

gauss n = if(n==1) then 1 else n + gauss (n-1)  

divi num d = if(num >= d) then 1 + divi (num-d) d else 0

multiplicar a b = if(a==0) then 0 else  b + multiplicar (a-1) b

largo [] = 0
largo (x:xs) = 1 + largo xs

sustituye [] = []
sustituye (x:xs)=if (x==6) then 7:(sustituye xs) else x:(sustituye xs)

aprobados [] = 0
aprobados (x:xs)=if (x>=70) then 1 + (aprobados xs) else (aprobados xs)

raprobados [] = []
raprobados (x:xs)=if (x>=70) then x:(raprobados xs) else (raprobados xs)

del_iesimo [] c = []
del_iesimo (x:xs) c = if (c==0) then xs else x:(del_iesimo xs (c-1))

mayor [] v = v
mayor (x:xs) v = if(x>v) then mayor xs x else mayor xs v
    
elmayor [] = -1
elmayor (x:xs) = mayor xs x

prom l = let 
    s=suma l
    c=cuenta l
    in 
        s/c

suma [] = 0
suma (x:xs) = x + suma xs

cuenta [] = 0
cuenta (x:xs) = 1 + cuenta xs

burbuja [] = []
burbuja (x:xs) = let
    m = menor (x:xs)
    l2 = quitar (x:xs) m
    in
        m:(burbuja l2)

menor (x:xs) = less xs x
less [] v = v
less (x:xs) v = if (x<v) then less xs x else less xs v

quitar [] v = []
quitar (x:xs) v = if (x==v) then xs else x:(quitar xs v)

