
(ns source-code-formatter.ns.format
    (:require [io.api                        :as io]
              [source-code-formatter.ns.read :as ns.read]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn format-ns!
  ; @description
  ; Reads the content of a Clojure source code file found on the given filepath and formats the 'ns' macro.
  ;
  ; @param (string) filepath
  ;
  ; @usage
  ; (format-ns! "my-namespace.clj")
  [filepath]
  (if-let [source-code (io/read-file filepath {:warn? true})]
          (if-let [ns (ns.read/source-code->ns source-code)]
                  (-> ns ))))
