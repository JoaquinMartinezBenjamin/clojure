(ns proyecto03.fos)


(defn  vmap [f xs]
(  letfn [ (recursividad [g g2 zs]
                        (if (empty? g2)
                          zs
                          (recursividad g
                                        (rest g2)
                                        (conj zs ( g (first g2))))
                          )
                        )] (recursividad f xs [] )))

(defn composicion-de [f g]
  (fn [x]
    (g (f x))))

