(ns clj11161009.vigenere
    (:require [clj11161009.fda :as fda] :reload)
  )


(defn cifrar [x y] ( 
                     
                      if (and (fda/cadenaValida x) (fda/claveValida y))
                       
                        (fda/en-cadena (fda/vmap (fn [z] (fda/en-letra z))(fda/criptogramaNumerico x y)))
                        ""
                        ))

(defn descifrar [x y] ( fda/descifrando x y))

(defn cifrar-archivo [x y]  (cifrar (slurp x) y))
(defn descifrar-archivo [x y]  (descifrar (slurp x) y))
(defn guardarArchivoCifrado [x y z]  ( spit z (cifrar-archivo x y)))
(defn guardarArchivoDescifrado [x y z]  ( spit z (descifrar-archivo x y)))

