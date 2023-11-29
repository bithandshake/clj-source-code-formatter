
(ns source-code-formatter.libspecs.indents
    (:require [source-code-formatter.libspecs.utils :as libspecs.utils]
              [string.api                           :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-indent-length
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ;
  ; @return (integer)
  [source-code-map file-content _]
  (let [ns-start-position (-> source-code-map :ns first :bounds first)]
       (string/inline-position file-content ns-start-position)))

(defn ns-directive-indent-length
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ;
  ; @return (integer)
  [source-code-map file-content directive]
  (+ 4 (ns-indent-length source-code-map file-content directive)))

(defn libspec-indent-length
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ;
  ; @return (integer)
  [source-code-map file-content directive]
  (+ 7 (ns-indent-length source-code-map file-content directive)
       (-> directive name count)))

(defn libspec-details-indent-length
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (integer)
  [source-code-map _ directive libspec]
  (- (-> source-code-map :ns first directive :deps libspecs.utils/longest-libspec-name-length)
     (-> libspec :name count)))

(defn prefixed-namespace-indent-length
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (integer)
  [source-code-map file-content directive libspec]
  (+ 2 (libspec-indent-length source-code-map file-content directive)
       (-> libspec :name count)))

(defn prefixed-namespace-details-indent-length
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (integer)
  [_ _ _ libspec pref-ns]
  (- (-> libspec :prefixed libspecs.utils/longest-libspec-name-length)
     (-> pref-ns :name count)))
