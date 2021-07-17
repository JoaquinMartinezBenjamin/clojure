(ns proyecto03.core
  (:require [proyecto03.fos :as fos] :reload) )
 

(defn duplicados [xs]
  (fos/vmap (fn [x] (* 2 x))xs))