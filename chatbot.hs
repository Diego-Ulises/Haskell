texto = "verdura hambre hola"
frases = ["Para el hambre feroz coma tortas", 
          "La barbacoa es especial",
          "El arroz es muy nutritivo",
          "La verdura es saludable"]

simbolos = [',', '.', '!', '?', '¿']

stopwords = ["para", "el", "la", "es", "muy", "lo", "tiene", "se", "tengo"]

digo texto = calcula texto

sumarEspacio frase = frase ++ " "

diccionario texto frases = words (concat ([sumarEspacio texto] ++ map sumarEspacio frases))

sustituye lista1 lista2 =   let  
                                cabeza = head lista1
                                lista2Nueva = reemplazarValor cabeza lista2
                                lista1Nueva = tail lista1
                            in
                                if(length lista1 == 1) then lista2Nueva else sustituye lista1Nueva lista2Nueva

reemplazarValor valor []=[]
reemplazarValor valor (x:xs) =if (x==valor) then "1":(reemplazarValor valor xs) else x:(reemplazarValor valor xs)

reemplazarPalabras [] = []
reemplazarPalabras (x:xs)=if (x/="1") then "0":(reemplazarPalabras xs) else x:(reemplazarPalabras xs)

vector texto =  let
                    listaDicc = diccionario texto frases
                    listaTexto = words texto
                    v = sustituye listaTexto listaDicc
                    listaCeros = reemplazarPalabras v
                in
                    listaCeros

calcula string = let
        -- Limpio el texto de simbolos tipograficos = "Hola! para todos" -> "Hola para todos"
        newString = [ caracter | caracter <- string, not(caracter `elem` simbolos)] 
        
        -- Elimino los stopwords de mi cadena ingresada = "Hola para todos" -> ["Hola", "todos"]
        lStr = deleteStopWords newString

        -- Elimino los stopwords de cada una de mis frases (se crea una lista de listas)
        -- ejemplo: [ ["hambre", "feroz"], ["coma", "tortas"] ... ]
        lFrs = map deleteStopWords frases

        -- Eliminamos las palabras que se repiten en cada frase que estan contenidas en mi cadena ingresada
        newFrs = map (clean lStr) lFrs

        -- Eliminamos las palabras que se repiten entre frases


        -- Concateno la lista de mi cadena junto a la lista de listas
        -- Creo una sola lista con la funcion "unir". (Quito la lista de listas)
        -- ejemplo: ["Hola", "Todos", "hambre", "feroz", "coma", ...]
        -- Asi ya tenemos una lista que sera nuestro diccionario
        lDicc = unir ([lStr]++newFrs)

        -- Creamos una lista con los vectores 
        vString = vectorR lDicc lStr
        vFrases = map (vectorR lDicc) lFrs

        -- Creamos el vector con la funcion coseno
        similitudes = map (coseno vString) vFrases
    in
        similitudes

clean [] [] = []
clean xs ys = [ y | y <- ys, not(y `elem` xs) ]

reemplazarVector [] = []
reemplazarVector listaDiccionario listaFrases = [ if x `elem` listaFrases then 1 else 0 | x <- listaDiccionario]

vectorR lDicc listaFrases = [ if x `elem` listaFrases then 1 else 0 | x <- lDicc]

unir [] = []
unir (x:xs) = x ++ unir xs
    
deleteStopWords string =let
                            -- Creo una lista separado por palabras = "Hola para todos" -> ["Hola", "para", "todos"]
                            listWords = words string
                            -- Elimino los stopwords de mi lista = ["Hola", "para", "todos"] -> ["Hola", "todos"]
                            newList = [ word | word <- listWords, not(word `elem` stopwords)] 
                        in
                            newList

--posmayor [] v pos cont = pos
--posmayor (x:xs) v pos cont = if (x>v) then posmayor xs cont (cont+?...)

coseno l1 l2 = (producto_punto l1 l2) / ((euclidiana l1) * (euclidiana l2))

producto_punto l1 l2 = suma (mult l1 l2)

mult [] [] = []
mult (x:xs)(y:ys) = (x*y) (mult xs ys)

euclidiana l = sqrt (suma (map cuadrado l))

cuadrado x = x*x

suma [] = 0
suma (x:xs) = x + suma xs