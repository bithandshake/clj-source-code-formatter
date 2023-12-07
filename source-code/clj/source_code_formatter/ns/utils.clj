
(ns source-code-formatter.ns.utils
    (:require [fruits.map.api    :as map]
              [fruits.string.api :as string]
              [fruits.vector.api :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn compare-libspecs
  ; @ignore
  ;
  ; @param (map) a
  ; @param (map) b
  ;
  ; @return (boolean)
  [a b]
  ; Libspec types (increasing order):
  ; 1. raw:      (:require my-namespace)
  ; 2. basic:    (:require [my-namespace])
  ; 3. detailed: (:require [my-namespace :as ...])
  ;              (:require [my-namespace :refer [...]])
  ;              (:require [my-namespace :rename {...}])
  ; 4. prefixed: (:require [my-prefix namespace])
  ;              (:require [my-prefix [namespace :as ...]])
  (letfn [(f0 [{:keys [raw?]}]                    (-> raw?))                    ; Returns TRUE if the given libspec is raw
          (f1 [{:keys [alias only refer rename]}] (or alias only refer rename)) ; Returns TRUE if the given libspec is detailed
          (f2 [{:keys [prefixed]}]                (-> prefixed))                ; Returns TRUE if the given libspec is prefixed
          (f3 [%] (not (or (f0 %) (f1 %) (f2 %))))]                             ; Returns TRUE if the given libspec is basic
         (cond (and (f0 a) (f0 b)) (string/abc? (:name a) (:name b)) ; a is raw,               b is raw
               (and (f3 a) (f3 b)) (string/abc? (:name a) (:name b)) ; a is basic,             b is basic
               (and (f1 a) (f1 b)) (string/abc? (:name a) (:name b)) ; a is detailed,          b is detailed
               (and (f2 a) (f2 b)) (string/abc? (:name a) (:name b)) ; a is prefixed,          b is prefixed
               (and (f0 a))        (-> true)                         ; a is raw,               b is not raw
               (and        (f0 b)) (-> false)                        ; a is not raw,           b is raw,
               (and (f3 a))        (-> true)                         ; a is basic,             b is not raw nor basic
               (and        (f3 b)) (-> false)                        ; a is not raw nor basic, b is basic
               (and (f1 a))        (-> true)                         ; a is detailed,          b is prefixed
               (and        (f1 b)) (-> false))))                     ; a is prefixed,          b is detailed

(defn sort-libspecs
  ; @ignore
  ;
  ; @param (maps in vector) libspecs
  ;
  ; @return (maps in vector)
  [libspecs]
  (vector/sort-items libspecs compare-libspecs))

(defn longest-libspec-name-length
  ; @ignore
  ;
  ; @param (maps in vector) libspecs
  ;
  ; @return (string)
  [libspecs]
  (letfn [(f0 [result {:keys [alias name refer] :as libspec}]
              (if (or alias refer)
                  (max result (count name))
                  (->  result)))]
         (reduce f0 0 libspecs)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn prepare-ns-declaration-map
  ; @ignore
  ;
  ; @param (map) ns-declaration-map
  ;
  ; @return (map)
  [ns-declaration-map]
  (-> ns-declaration-map (update-in [:import  :deps] sort-libspecs)
                         (update-in [:import  :deps] vector/update-items-by :prefixed update :prefixed sort-libspecs)
                         (update-in [:require :deps] sort-libspecs)
                         (update-in [:require :deps] vector/update-items-by :prefixed update :prefixed sort-libspecs)
                         (update-in [:use     :deps] sort-libspecs)
                         (update-in [:use     :deps] vector/update-items-by :prefixed update :prefixed sort-libspecs)))
