(ns clj11161009.fda)

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
  
  (let [letras {\A 0 \B 1 \C 2 \D 3 \E 4 \F 5 \G 6 \H 7 \I 8 \J 9 \K 10 \L 11 \M 12 \N 13 \Ñ 14 \O 15 \P 16 \Q 17 \R 18 \S 19 \T 20 \U 21 \V 22 \W 23 \X 24 \Y 25 \Z 26 \  -27} ]
    (if (nil? (letras c))
      c
      (letras c)))) 




(defn en-Vector [n] (sreduce (fn [x y]
                      (conj x y))
                    []
                    (str n)))
(defn en-String [x] (str x))

(defn en-VectorNumero [i]
     (vmap (fn [x] (en-Numero x)) (en-Vector i))
     )

(defn sumarVectores[v v2] 
      (letfn [(g [ys c v v2 v3 z ]
               (if   (>= c z )
                 ys
                 
                 (if (empty? v2 )
                   
                  
                   
                   (  g 
                    ys
                    c 
                    v
                    v3
                    v3
                    z)
                   
                   (if (neg? (first v))
                     
                     (  g 
                       (conj ys, (first [70])) 
                    (+ 1 c) 
                    (rest v)
                    v2 
                    v3
                    z)
                     
                      (  g 
                    (conj ys, (+ (first v) (first v2))) 
                    (+ 1 c) 
                    (rest v)
                    (rest v2)
                    v3
                    z)
                     
                     ) 
                   
                    
                   
                   )
                 
                
                 
                  
                 )
               
               
               )] (g [] 0 v v2 v2(count v))))


(defn sumarMclaClave [x y] (sumarVectores ( en-VectorNumero x) ( en-VectorNumero y )))


(defn restarMayor27[n]((fn [v](let [x v] (vmap ( fn[x] (if  (and (not (== x 70)) (>= x 27)) 
                            (- x 27)
                            x)
               )x )) ) n))

(defn criptogramaNumerico [x y]    (restarMayor27 (sumarMclaClave x y))  )



(defn en-letra [c] 
  
  (if (neg? c)
     (en-letra  70)
   
     (let [letras {0 \A 1 \B 2 \C 3 \D 4 \E 5 \F 6 \G 7 \H 8 \I 9 \J 10 \k 11 \L 12 \M 13\N 14 \Ñ 15 \O 16 \P 17 \Q 18 \R 19 \S 20 \T 21 \U 22 \V 23 \W 24 \X 25 \Y 26 \Z 70 \ } ]
    (if (nil? (letras c))
      c
      (letras c)))
  
    ) 
 
  
  )




(defn  en-cadena [xs]
(  letfn [ (recursividad [ ys zs]
                        (if (empty? ys)
                          zs
                          (recursividad 
                                        (rest ys)
                                        (str zs ( first ys)))
                          )
                        )] (recursividad xs "" )))

(defn caracterValida [x] (if (or (= x \A)
                                 (= x \B)
                                 (= x \C)
                                 (= x \D)
                                 (= x \E)
                                 (= x \F)
                                 (= x \G)
                                 (= x \H)
                                 (= x \I)
                                 (= x \J)
                                 (= x \K)
                                 (= x \L)
                                 (= x \M)
                                 (= x \N)
                                 (= x \Ñ)
                                 (= x \O)
                                 (= x \P)
                                 (= x \Q)
                                 (= x \R)
                                 (= x \S)
                                 (= x \T)
                                 (= x \U)
                                 (= x \V)
                                 (= x \W)
                                 (= x \X)
                                 (= x \Y)
                                 (= x \Z)
                                 (= x \ )
                                 
                                 ) true false  ))

(defn caracterValidaClave [x] (if (or (= x \A)
                                 (= x \B)
                                 (= x \C)
                                 (= x \D)
                                 (= x \E)
                                 (= x \F)
                                 (= x \G)
                                 (= x \H)
                                 (= x \I)
                                 (= x \J)
                                 (= x \K)
                                 (= x \L)
                                 (= x \M)
                                 (= x \N)
                                 (= x \Ñ)
                                 (= x \O)
                                 (= x \P)
                                 (= x \Q)
                                 (= x \R)
                                 (= x \S)
                                 (= x \T)
                                 (= x \U)
                                 (= x \V)
                                 (= x \W)
                                 (= x \X)
                                 (= x \Y)
                                 (= x \Z)
                             
                                 
                                 ) true false  ))

(defn vfilter [f xs]
  
  (letfn [(recursividad [g ys zs]
            (if (empty? ys)
              zs
              (recursividad g 
                             (rest ys)
                             (if (g (first ys))
                               (conj zs (first ys)),zs))))]
    (recursividad f xs []))) 
 (defn soloValidos [x] (vfilter (fn [x] (caracterValida x)) x) )
 (defn soloValidosC [x] (vfilter (fn [x] (caracterValidaClave x)) x) )
  
  (defn cadenaValida [x] (if (or (> (count (str x)) (count (soloValidos (en-Vector x)))) (< (count (str x)) (count (soloValidos (en-Vector x))))) 
                           
                           false
                           true
                           ))
  
   (defn claveValida [x] (if (or (> (count (str x)) (count (soloValidosC (en-Vector x)))) (< (count (str x)) (count (soloValidosC (en-Vector x))))) 
                           
                           false
                           true
                           ))
  
   
   
   (defn restarVectores[v v2] 
      (letfn [(g [ys c v v2 v3 z ]
               (if   (>= c z )
                 ys
                 
                 (if (empty? v2 )
                   
                  
                   
                   (  g 
                    ys
                    c 
                    v
                    v3
                    v3
                    z)
                   
                   (if (== -27 (first v))
                     
                     (  g 
                       (conj ys, (first [70])) 
                    (+ 1 c) 
                    (rest v)
                    v2 
                    v3
                    z)
                     
                      (  if (< (first v) (first v2))
                         (g 
                    (conj ys, (- (+ 27 (first v)) (first v2))) 
                    (+ 1 c) 
                    (rest v)
                    (rest v2)
                    v3
                    z)  
                        
                        (g 
                    (conj ys, (- (first v) (first v2))) 
                    (+ 1 c) 
                    (rest v)
                    (rest v2)
                    v3
                    z)
                     
                        
                        
                        )
                     
                     ) 
                   
                    
                   
                   )
                 
                
                 
                  
                 )
               
               
               )] (g [] 0 v v2 v2(count v))))
   
   (defn restarMclaClave [x y] (restarVectores ( en-VectorNumero x) ( en-VectorNumero y )))
   
   (defn descriptogramaNumerico [x y]    (restarMclaClave x y))
   
   (defn descifrando [x y] (en-cadena (vmap (fn [z] (en-letra z))(descriptogramaNumerico x y))))
   
   
