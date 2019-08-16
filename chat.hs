frases = ["Para el hambre feroz coma tortas", 
          "La barbacoa es especial",
          "El arroz es muy nutritivo",
          "La verdura es saludable"]
stopwords = ["para", "el", "la", "es", "muy", "lo"]

digo texto = calcula texto frases

calcula texto frases = let
                    d = diccionario texto frases
                    vq = vector d texto
                    vectores = map (vector d) frases
                    similitudes = map (coseno vq) vectores
                    n = posmayor similitudes (head similitudes) 0 0
                in
                    frases !! n

posmayor [] v pos cont = pos
posmayor (x:xs) v pos cont = if (x>v) then posmayor xs cont (cont+?...)

sumarEspacio frase = frase ++ " "

diccionario texto frases = words (concat ([sumarEspacio texto] ++ map sumarEspacio frases))

coseno l1 l2 = (producto_punto l1 l2) / ((euclidiana l1) * (euclidiana l2))

producto_punto l1 l2 = suma (mult l1 l2)

mult [] [] = []
mult (x:xs)(y:ys) = (x*y) (mult xs ys)

euclidiana l = sqrt (suma (map cuadrado l))

cuadrado x = x*x

suma [] = 0
suma (x:xs) = x + suma xs