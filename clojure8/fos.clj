(ns clj11160973.fos)


(defn  vmap [f xs]
(  letfn [ (recursividad [g ys zs]
(if (empty? ys)
zs
(recursividad g
(rest ys)
(conj zs ( g (first ys))))
)
)] (recursividad f xs [] )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vreduce  [f vi xs]
(letfn [(recursividad [g vf ys]

(if (empty? ys)
vf
(recursividad g
(g vf (first ys))
(rest ys))))]
(recursividad f vi xs)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sreduce  [f vi xs]
(letfn [(recursividad [g vf ys]

(if (empty? ys)
vf
(recursividad g
(g vf (first ys))
(rest ys))))]
(recursividad f vi xs)))

(defn en-Numero [c]

(let [letras {\a 0 \á 1 \b 2 \c 3 \d 4 \e 5 \é 6 \f 7 \g 8 \h 9 \i 10 \í 11 \j 12 \k 13 \l 14 \m 15 \n 16 \ñ 17 \o 18 \ó 19 \p 20 \q 21 \r 22 \s 23 \t 24 \u 25 \ú 26 \ü 27 \v 28 \w 29 \x 30 \y 31 \z 32 \  -33 \return -33 \newline -33} ]
(if (nil? (letras c))
c
(letras c))))

(defn en-NumeroClave [c x]
(let [letras {\a x \á x \b x \c x \d x \e x \é x \f x \g x \h x \i x \í x \j x \k x \l x \m x \n x \ñ x \o x \ó x \p x \q x \r x \s x \t x \u x \ú x \ü x \v x \w x \x x \y x \z x \  -33 \return -33 \newline -33} ]
(if (nil? (letras c))
x
(letras c x))))


(defn en-Minuscula [c]

(let [letras {\A \a \Á \á \B \b \C \c \D \d \E \e \É \é \F \f \G \g \H \h \I \i \Í \í \J \j \K \k \L \l \M \m \N \n \Ñ \ñ \O \o \Ó \ó \P \p \Q \q \R \r \S \s \T \t \U \u \Ú \ú \Ü \ü \V \v \W \w \X \x \Y \y \Z \z } ]
(if (nil? (letras c))
c
(letras c))))

(defn en-mayusculas [c]
(let [mayusculas {\A \a \Á \á \B \b \C \c \D \d \E \e \É \é \F \f \G \g \H \h \I \i \Í \í \J \j \K \k \L \l \M \m \N \n \Ñ \ñ \O \o \Ó \ó \P \p \Q \q \R \r \S \s \T \t \U \u \V \v \W \w \X \x \Y \y \Z \z \  \  } ]
(if (nil? (mayusculas c))
c
(mayusculas c))))

(defn en-mayusculas2 [c]
(let [mayusculas2 {\a \A \e \E \i \I \o \O \u \U} ]
(if (nil? (mayusculas2 c))
c
(mayusculas2 c))))

(defn en-Vector [n] (sreduce (fn [x y]
(conj x y))
[]
(str n)))
(defn en-String [x] (str x))

(defn en-VectorNumero [i]
(vmap (fn [x] (en-Numero x)) (en-Vector i))
)

(defn en-VectorNumeroClave [i j]
(vmap (fn [x] (en-NumeroClave x j)) (en-Vector i))
)

  (defn leer [x] (slurp x))
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
(rest v2) ;;;;;;;
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
                   
                   (if (== -33 (first v))
                     
                     (  g 
                       (conj ys, (first [70])) 
                    (+ 1 c) 
                    (rest v)
                   (rest v2);; 
                    v3
                    z)
                     
                      (  if (< (first v) (first v2))
                         (g 
                    (conj ys, (- (+ 33 (first v)) (first v2))) 
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

(defn sumarMclaClave [x y] (sumarVectores ( en-VectorNumero x) ( en-VectorNumero y )))
(defn sumarClave[x y] (sumarVectores ( en-VectorNumero x) ( en-VectorNumeroClave x y )))


(defn restarClave[x y] (restarVectores ( en-VectorNumero x) ( en-VectorNumeroClave x y )))


(defn restarMayor33[n]((fn [v](let [x v] (vmap ( fn[x] (if  (and (not (== x 70)) (>= x 33)) 
(- x 33)
x)
)x )) ) n))

(defn criptogramaNumerico [x y]    (restarMayor33 (sumarMclaClave x y))  )


(defn en-letra [c] 

(if (neg? c)
(en-letra  70)

(let [letras {0 \a 1 \á 2 \b 3 \c 4 \d 5 \e 6 \é 7 \f 8 \g 9 \h 10 \i 11 \í 12 \j 13 \k 14 \l 15 \m 16 \n 17 \ñ 18 \o 19 \ó 20 \p 21 \q 22 \r 23 \s 24 \t 25 \u 26 \ú 27 \ü 28 \v 29 \w 30 \x 31 \y 32 \z 70 \  } ]
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

(defn caracterValida [x] (if (or ( = x \a)
( = x \b)
( = x \c)
( = x \d)
( = x \e)
( = x \f)
( = x \g)
( = x \h)
( = x \i)
( = x \j)
( = x \k)
( = x \l)
( = x \m)
( = x \n)
( = x \ñ)
( = x \o)
( = x \p)
( = x \q)
( = x \r)
( = x \s)
( = x \t)
( = x \u)
( = x \v)
( = x \w)
( = x \x)
( = x \y)
( = x \z)
( = x \á)
( = x \é)
( = x \í)
( = x \ó)
( = x \ú)
( = x \ü)
( = x \ )

) true false  ))

(defn caracterValidaClave [x] (if (or ( = x \a)
( = x \b)
( = x \c)
( = x \d)
( = x \e)
( = x \f)
( = x \g)
( = x \h)
( = x \i)
( = x \j)
( = x \k)
( = x \l)
( = x \m)
( = x \n)
( = x \ñ)
( = x \o)
( = x \p)
( = x \q)
( = x \r)
( = x \s)
( = x \t)
( = x \u)
( = x \v)
( = x \w)
( = x \x)
( = x \y)
( = x \z)
( = x \á)
( = x \é)
( = x \í)
( = x \ó)
( = x \ú)
( = x \ü)
( = x \ )

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

(if (== -33 (first v))

(  g 
(conj ys, (first [70])) 
(+ 1 c) 
(rest v)
(rest v2);; 
v3
z)

(  if (< (first v) (first v2))
(g 
(conj ys, (- (+ 33 (first v)) (first v2))) 
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
   
   
   (defn descriptograma[x y]    (restarClave x y))
   
   (defn descifrando [x y] (en-cadena (vmap (fn [z] (en-letra z))(descriptogramaNumerico x y))))
   
   (defn smap [f xs]
  (letfn [(recursividad [g ys zs]
            (if (empty? ys)
              zs
              (recursividad g
                            (rest ys)
                            (str zs (g (first ys)))
                            )))]
    
    (recursividad f xs "")))
  

   
  (defn cifrando [x y](en-cadena (vmap (fn [z] (en-letra z))  (restarMayor33 (sumarClave (leer x) y)))))
  
  (defn descifrar[x y] (en-cadena (vmap (fn [z] (en-letra z))(descriptograma (leer x) y))))
    
  
  (defn validaMayusculas [x] (smap en-Minuscula x))

(defn vacia [x]  (if (== (compare x "") 0)  false true ) )

(defn negativo? [x] (if   (neg? x) false true ) )
  
  (defn valida?[x y] (if (and (vacia y) (negativo? x) (< x 33)) 
                            true
                              false
                              ))

