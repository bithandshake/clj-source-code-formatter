
(ns source-code-formatter.ns.prototypes
    (:require [source-code-formatter.ns.utils :as ns.utils]
              [vector.api                     :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn source-code-map-prototype
  ; @ignore
  ;
  ; @param (map) source-code-map
  ;
  ; @return (map)
  [source-code-map]
  (-> source-code-map (update-in [:ns 0 :ns/import  :deps] ns.utils/sort-libspecs)
                      (update-in [:ns 0 :ns/import  :deps] vector/update-items-by :prefixed update :prefixed ns.utils/sort-libspecs)
                      (update-in [:ns 0 :ns/require :deps] ns.utils/sort-libspecs)
                      (update-in [:ns 0 :ns/require :deps] vector/update-items-by :prefixed update :prefixed ns.utils/sort-libspecs)
                      (update-in [:ns 0 :ns/use     :deps] ns.utils/sort-libspecs)
                      (update-in [:ns 0 :ns/use     :deps] vector/update-items-by :prefixed update :prefixed ns.utils/sort-libspecs)))
