
(ns source-code-formatter.ns.utils
    (:require [string.api :as string]
              [vector.api :as vector]))

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
  (letfn [(f0 [{:keys [alias only refer rename]}] (or alias only refer rename))
          (f1 [{:keys [prefixed]}]                (-> prefixed))]
         (cond (and (f0 a) (f0 b)) (string/abc? (:name a) (:name b))
               (and (f1 a) (f1 b)) (string/abc? (:name a) (:name b))
               (and (f0 a) (f1 b)) (-> true)
               (and (f1 a) (f0 b)) (-> false)
               (and (f0 a))        (-> false)
               (and (f1 a))        (-> false)
               (and        (f0 b)) (-> true)
               (and        (f1 b)) (-> true)
               :both-basic         (string/abc? (:name a) (:name b)))))

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
  (letfn [(f [result {:keys [alias name refer] :as libspec}]
             (if (or alias refer)
                 (max result (count name))
                 (->  result)))]
         (reduce f 0 libspecs)))
