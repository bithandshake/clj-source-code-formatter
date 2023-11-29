
(ns source-code-formatter.libspecs.format
    (:require [source-code-formatter.libspecs.assemble :as libspecs.assemble]
              [source-code-formatter.libspecs.utils    :as libspecs.utils]
              [source-code-map.api                  :as source-code-map]
              [io.api                               :as io]
              [seqable.api                          :as seqable]
              [string.api                           :as string]
              [syntax.api                           :as syntax]
              [syntax-reader.api                    :as syntax-reader]
              [vector.api                           :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn rebuild-ns-directive
  [source-code-map file-content directive]
  (letfn [(f0 [source-code-map]
              (-> source-code-map (update-in [:ns 0 directive :deps] libspecs.utils/sort-libspecs)
                                  (update-in [:ns 0 directive :deps] vector/update-items-by :prefixed update :prefixed libspecs.utils/sort-deps)))]
         (if-let [deps (-> source-code-map :ns first directive)]
                 (-> file-content (string/cut-range   (-> source-code-map :ns first directive :bounds first)
                                                      (-> source-code-map :ns first directive :bounds second))
                                  (string/insert-part (-> source-code-map f0 (libspecs.assemble/assemble-ns-directive file-content directive))
                                                      (-> source-code-map :ns first directive :bounds first)))
                 (-> file-content))))
         ;(-> source-code-map f0 (libspecs.assemble/assemble-ns-directive file-content directive))))

(defn format-ns-deps!
  ; @description
  ; Reads the content of a Clojure source code file found on the given filepath and formats the libspecs within the namespace declaration:
  ; 1. Groups the libspecs by type:
  ;    1. Raw libspecs                               (e.g. 'namespace-a')                       ; <- now it is vectorized also!
  ;    2. Vector libspecs                            (e.g. '[namespace-a]')
  ;    3. Vector libspecs with ':as', ':refer', etc. (e.g. '[namespace-a :as a]')
  ;    4. Prefixed libspecs                          (e.g. '[prefix [namespace-a :as a]]')
  ;    + Commented libspecs are ordered as they were not commented.
  ; 2. Sorts the libspecs alphabetically within the type groups.
  ; 3. Indents the libspecs.
  ; 4. Aligns the vector type libspecs.
  ;
  ; @param (string) filepath
  ;
  ; @usage
  ; (format-ns-deps! "my-namespace.clj")
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (if-let [source-code-map (source-code-map/read-source-file filepath)]
                  (-> source-code-map (rebuild-ns-directive file-content :ns/require)))))



  ;(if-let [source-code (io/read-file filepath {:warn? true})]
  ;        (if-let [ns (libspecs.read/source-code->ns source-code)]
  ;                (if-let [require (libspecs.read/ns->require ns)]
  ;                        (-> require (libspecs.read/require->libspecs))])
                                      ;(prepare-libspecs)
                                      ;(split-lines)
                                      ;(order-lines)
  ;                                    (str)
  ;                                    (println))))))
  ;(println)
  ;(println))

  ;        (if (string/contains-part? source-code "(:require ")
  ;            (io/write-file! filepath (str (libspecs.read/source-code->before-require source-code)
  ;                                          (-> source-code (libspecs.read/source-code->require)
  ;                                                          (libspecs.read/require->libspecs)
  ;                                                          (split-libspecs)
  ;                                                          (order-libspecs)
  ;                                                          (align-vectors)
  ;                                                          (indent-libspecs)
  ;                                                          (join-libspecs)
  ;                                                          (wrap-libspecs)
  ;                                          (libspecs.read/source-code->after-require source-code)])
