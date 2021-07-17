(ns proyecto03.core2
   (:require [proyecto03.fos :as fos] :reload)
  (:require [proyecto03.string :as str] :reload))

(defn duplicados [xs]
  (fos/vmap (fn [x] (* 2 x))xs))