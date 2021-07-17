(ns clj11160973.desplazamiento
    (:require [clj11160973.fos :as fos] :reload)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn archivo-cifrado [x y] (if (fos/valida? x y)
                             (fos/cifrando (fos/validaMayusculas  y) x)
                               ""
                              ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defn archivo-descifrado [x y]  (if (fos/valida? x y) 
                             (fos/descifrar (fos/validaMayusculas  y) x)
                               ""
                              ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
