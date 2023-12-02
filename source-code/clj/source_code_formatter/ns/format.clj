
(ns source-code-formatter.ns.format
    (:require [source-code-formatter.ns.assemble   :as ns.assemble]
              [source-code-formatter.ns.prototypes :as ns.prototypes]
              [source-code-formatter.ns.indents    :as ns.indents]
              [source-code-map.api                 :as source-code-map]
              [io.api                            :as io]
              [string.api                          :as string]

              [time.api :as time]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn rebuild-ns-directive
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (string)
  [file-content source-code-map directive]
  (if-let [_ (-> source-code-map :ns first directive :bounds)]
          (-> file-content (string/cut-range   (-> source-code-map :ns first directive :bounds first)
                                               (-> source-code-map :ns first directive :bounds second))
                           (string/insert-part (-> file-content (ns.assemble/assemble-ns-directive source-code-map directive))
                                               (-> source-code-map :ns first directive :bounds first))
                           (string/insert-part (-> file-content (ns.assemble/assemble-ns-directive-comments source-code-map directive))
                                               (-> source-code-map :ns first directive :bounds first (+ 3 (-> directive name count)))))
          (-> file-content)))

(defn format-ns-deps!
  ; @description
  ; Formats the dependencies within the namespace declaration in the source code file found at the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @usage
  ; (format-ns-deps! "my-namespace.clj")
  [filepath]
  (println "filepath:" filepath)
  (println (time/timestamp-string))
  (println "generating source code map")
  (if-let [source-code-map (source-code-map/read-source-file filepath)]
          (let [_ (println "source code map generated")
                source-code-map (ns.prototypes/source-code-map-prototype source-code-map)]
               (println "source code map updated")
               (io/update-file! filepath (fn [file-content] (-> file-content (rebuild-ns-directive source-code-map :ns/import)
                                                                             (rebuild-ns-directive source-code-map :ns/require)
                                                                             (rebuild-ns-directive source-code-map :ns/use)))))
          (println "unable to generate source code map")))
