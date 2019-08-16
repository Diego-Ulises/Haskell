frases = ["el arroz tiene vitaminas", 
          "el hambre, se quita comiendo"]

simbols = [',', '.', '!', '?', '¿']

stopwords = ["para", "el", "la", "es", "muy", "lo", "tiene", "se", "tengo"]

calcula string = let
        -- Limpio el texto de simbolos tipograficos = "Hola! para todos" -> "Hola para todos"
        newString = [ caracter | caracter <- string, not(caracter `elem` simbols)] 
        
        -- Elimino los stopwords de mi cadena ingresada = "Hola para todos" -> ["Hola", "todos"]
        lStr = deleteStopWords newString

        -- Elimino los stopwords de cada una de mis frases (se crea una lista de listas)
        -- ejemplo: [ ["hambre", "feroz"], ["coma", "tortas"] ... ]
        lFrs = map deleteStopWords frases
    
        -- Concateno la lista de mi cadena junto a la lista de listas
        -- Creo una sola lista con la funcion "unir". (Quito la lista de listas)
        -- ejemplo: ["Hola", "Todos", "hambre", "feroz", "coma", ...]
        -- Asi ya tenemos una lista que sera nuestro diccionario
        lDicc = unir ([lStr]++lFrs)

        -- Creamos una lista con los vectores 
        v = map (vector lDicc) lFrs
    in
        v


vector [] _ = []
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

