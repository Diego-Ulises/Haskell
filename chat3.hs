import Data.List
import Data.Ord


frases = ["los tacos estan muy ricos", "ya no hay al pastor", "aun hay de bistec", "hoy hay tacos de todo", "cuestan baratos los tacos", "la salsa es la mas rica de la region", "aqui esta tu cuenta", "gracias a ti"]

simbols = [',', '.', '!', '?', '¿', '¡', '#', '$', '%']

stopwords = ["para", "el", "la", "los", "las", "un", "una", "unos","unas","es", "muy", "tiene", "se", "tengo", "a", "ante", "bajo", "cabe", "con", "contra", "de", "desde", "durante", "en", "entre", "hacia", "hasta", "mediante", "para", "por", "según", "sin", "so", "sobre", "tras", "versus", "vía"]

calcula string = let
        -- Limpio el texto de simbolos tipograficos = "Hola! para todos" -> "Hola para todos"
        newString = [ caracter | caracter <- string, not(caracter `elem` simbols)] 
        
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
        vString = vector lDicc lStr
        vFrases = map (vector lDicc) lFrs

        -- Creamos el vector con la funcion coseno
        similitudes = map (coseno vString) vFrases
        
        n = posMayor similitudes
    in
        frases !! n

-- ["hambre", "alimento"] , ["hambre", "quita", "comiendo"]
clean [] [] = []
clean xs ys = [ y | y <- ys, not(y `elem` xs) ]

mayor [] v = v
mayor (x:xs) v = if(x>v) then mayor xs x else mayor xs v
    
elmayor [] = -1
elmayor (x:xs) = mayor xs x

vector lDicc listaFrases = [ if x `elem` listaFrases then 1 else 0 | x <- lDicc]

unir [] = []
unir (x:xs) = x ++ unir xs
    
deleteStopWords string = let
        -- Creo una lista separado por palabras = "Hola para todos" -> ["Hola", "para", "todos"]
        listWords = words string
        -- Elimino los stopwords de mi lista = ["Hola", "para", "todos"] -> ["Hola", "todos"]
        newList = [ word | word <- listWords, not(word `elem` stopwords)] 
    in
        newList

posMayor :: Ord a => [a] -> Int
posMayor = fst . maximumBy (comparing snd) . zip [0..]

coseno l1 l2 = (producto_punto l1 l2) / ((euclidiana l1) * (euclidiana l2))

producto_punto l1 l2 = suma (mult l1 l2)

mult [] [] = [] 
mult (x:xs) (y:ys) = (x*y):(mult xs ys)

euclidiana l = sqrt (suma (map cuadrado l))

cuadrado x = x*x

suma [] = 0
suma (x:xs) = x + suma xs
