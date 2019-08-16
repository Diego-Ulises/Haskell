import Data.List
import Data.Ord

frases = ["las carnitas tienen buen sabor y son nutritivas","los tacos estan muy ricos", "ya no hay al pastor", "aun hay de bistec", "hoy hay tacos de todo", "cuestan baratos los tacos", "la salsa es la mas rica de la region", "aqui esta tu cuenta", "gracias a ti"]

simbolos = [',', '.', '!', '?', '¿', '¡', '#', '$', '%']

stopwords = ["para", "que", "me", "mi", "al", "su", "a", "el", "la", "los", "las", "un", "una", "unos","unas","es", "muy", "tiene", "se", "tengo", "a", "ante", "bajo", "cabe", "con", "contra", "de", "desde", "durante", "en", "entre", "hacia", "hasta", "mediante", "para", "por", "según", "sin", "sobre", "tras", "versus", "vía"]

consultas=["de que hay tacos hoy?","hay tacos al pastor?","tiene de bistec?","que tan baratos estan los tacos?","muchas gracias!!!","me das mi cuenta $$$"]

controlador = map calcula consultas

calcula texto = let
                    nuevoTexto = [ caracter | caracter <- texto, not(caracter `elem` simbolos)] 
                    listaTexto = borrarStopWords nuevoTexto
                    listaFrases = map borrarStopWords frases
                    nuevaListaFrases = map (eliminarRepeticiones listaTexto) listaFrases
                    diccionario = unir ([listaTexto]++nuevaListaFrases)
                    vectorCadenas = vector diccionario listaTexto
                    vectorFrases = map (vector diccionario) listaFrases
                    similitudes = map (coseno vectorCadenas) vectorFrases
                    n = posMayor similitudes
                in
                    frases !! n

posMayor :: Ord a => [a] -> Int
posMayor = fst . maximumBy (comparing snd) . zip [0..]

vector diccionario listaFrases = [ if x `elem` listaFrases then 1 else 0 | x <- diccionario]

unir [] = []
unir (x:xs) = x ++ unir xs

eliminarRepeticiones [] [] = []
eliminarRepeticiones xs ys = [ y | y <- ys, not(y `elem` xs) ]
    
borrarStopWords string = let
        listWords = words string
        newList = [ word | word <- listWords, not(word `elem` stopwords)] 
    in
        newList
                        
coseno l1 l2 = (producto_punto l1 l2) / ((euclidiana l1) * (euclidiana l2))

producto_punto l1 l2 = suma (mult l1 l2)

mult [] [] = [] 
mult (x:xs) (y:ys) = (x*y):(mult xs ys)

euclidiana l = sqrt (suma (map cuadrado l))

cuadrado x = x*x

suma [] = 0
suma (x:xs) = x + suma xs