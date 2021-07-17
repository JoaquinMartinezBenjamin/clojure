(ns clj12160001.vigenere
    (:require [clj12160001.fos :as fos] :reload)
  )


 (defn convertirMayusculas [x] (fos/smap fos/en-Minuscula x))

(defn cifrado [x y] ( 
                      
                      if (and (fos/cadenaValida (convertirMayusculas y))   (fos/claveValida (convertirMayusculas x)))
                       
                        (fos/en-cadena (fos/vmap (fn [z] (fos/en-letra z))(fos/criptogramaNumerico  (convertirMayusculas y) (convertirMayusculas x) )))
                         "" 
                         ;(fos/en-cadena (fos/vmap (fn [z] (fos/en-letra z))(fos/criptogramaNumerico x y)))
                         ))

(defn descifrado [x y] ( fos/descifrando y x))


(defn clave-válida [ x y z ] (if (== (compare (cifrado x y ) z) 0)  true false )  )
