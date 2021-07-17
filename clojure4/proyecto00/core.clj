(ns proyecto00.core)

(defn  vmap [f xs]
(  letfn [ (recursividad [g ys zs]
                        (if (empty? ys)
                          zs
                          (recursividad g
                                        (rest ys)
                                        (conj zs ( g (first ys))))
                          )
                        )] (recursividad f xs [] )))

 (defn vreduce  [f vi xs]
   (letfn [(recursividad [g vf ys]
             
             (if (empty? ys)
               vf
               (recursividad g
                             (g vf (first ys))
                             (rest ys))))]
     (recursividad f vi xs)))
 
  (defn sreduce  [f vi xs]
   (letfn [(recursividad [g vf ys]
             
             (if (empty? ys)
               vf
               (recursividad g
                             (g vf (first ys))
                             (rest ys))))]
     (recursividad f vi xs)))



(defn en-Numero [c]
  (let [mayusculas {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 } ]
    (if (nil? (mayusculas c))
      c
      (mayusculas c))))





(defn en-Vector [n] (sreduce (fn [x y]
                      (conj x y))
                    []
                    (str n)))
(defn en-String [x] (str x))

(defn en-VectorNumero [i]
     (vmap (fn [x] (en-Numero x)) (en-Vector i))
     )

(defn vector12 [v] 
      (letfn [(g [ys c v z ]
               (if (>= c z )
                 ys
                 (g (if (even? c) (conj ys, (* (first v) 1)) (conj ys, (* (first v) 2)))
                    (+ 1 c) 
                    (rest v)
                    z)
                 )
               
               
               )] (g [] 0 v (count v))))
  

(defn aUnDigito [n]((fn [v](let [x v] (vmap ( fn[x] (if (> x 9)
                            (+ (first (en-VectorNumero x) )(last (en-VectorNumero x)))
                            x)
               )x )) ) n))


        
 
 (defn sumaElementosVector [x] (vreduce (fn [x y] (+ x y))0 x))