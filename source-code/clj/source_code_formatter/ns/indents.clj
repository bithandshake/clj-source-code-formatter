
(ns source-code-formatter.ns.indents
    (:require [source-code-formatter.ns.utils :as libspecs.utils]
              [string.api                     :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-indent-length
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (integer)
  [file-content source-code-map _]
  (let [ns-start-position (-> source-code-map :ns first :bounds first)]
       (string/inline-position file-content ns-start-position)))

(defn ns-directive-indent-length
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (integer)
  [file-content source-code-map directive]
  (string/inline-position file-content (-> source-code-map :ns first directive :bounds first)))

(defn libspec-indent-length
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (integer)
  [file-content source-code-map directive]
  (+ 3 (ns-directive-indent-length file-content source-code-map directive)
       (-> directive name count)))

(defn libspec-details-indent-length
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (integer)
  [_ source-code-map directive libspec]
  (- (-> source-code-map :ns first directive :deps libspecs.utils/longest-libspec-name-length)
     (-> libspec :name count)))

(defn prefixed-namespace-indent-length
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (integer)
  [file-content source-code-map directive libspec]
  (+ 2 (libspec-indent-length file-content source-code-map directive)
       (-> libspec :name count)))

(defn prefixed-namespace-details-indent-length
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (integer)
  [_ _ _ libspec pref-ns]
  (- (-> libspec :prefixed libspecs.utils/longest-libspec-name-length)
     (-> pref-ns :name count)))
