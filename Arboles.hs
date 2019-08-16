data Tree = Branch Int Tree Tree | Leaf Int

arbol1 = Branch 11 (Branch 7 (Leaf 5) (Leaf 9)) (Leaf 6)
arbol2 = Branch 1 (Branch 8 (Leaf 7) (Leaf 9)) (Branch 4 (Leaf 5) (Branch 4 (Leaf 5) (Branch 4 (Leaf 5) (Leaf 2))))
arbol3 = Branch 1 (Branch 8 (Leaf 6) (Branch 4 (Leaf 2) (Leaf 9))) (Branch 4 (Leaf 5) (Leaf 2))

suma (Leaf x) = x
suma (Branch x izq der) = x + (suma izq) + (suma der)

contar (Leaf x) = 1
contar (Branch x izq der) = 1 + (contar izq) + (contar der)

construirLista (Leaf x) = [x]
construirLista (Branch x izq der) = (construirLista izq)++[x]++(construirLista der)

profundidad (Leaf x) = 1
profundidad (Branch x izq der) = let 
    profundidadizq=profundidad izq
    profundidadder=profundidad der
    in  if (profundidadizq>profundidadder) then 1 + profundidadizq else 1 + profundidadder

--1. Indicar la suma de la rama mayor.
sumaProf (Leaf x) = 1
sumaProf (Branch x izq der) = let 
    sumaProfIzq=sumaProf izq
    sumaProfDer=sumaProf der
    in  if (sumaProfIzq>sumaProfDer) then suma izq else suma der

--2. Dada una lista de de arboles sumar los valores de todos los nodos de todos los arboles.
concatenarListas [] = []
concatenarListas (x:xs) = x ++ concatenarListas xs

sumaTodo [] = 0
sumaTodo (x:xs) = x + sumaTodo xs
    --sumaTodo (concatenarListas (map construirLista [arbol1, arbol2, arbol3]))
  
--3. Dada una lista de arboles, indicar el número mayor de nodos de ellos 
mayor [] v = v
mayor (x:xs) v = if(x>v) then mayor xs x else mayor xs v

elmayor [] = -1
elmayor (x:xs) = mayor xs x
    --elmayor (map length (map construirLista [arbol1, arbol2, arbol3]))