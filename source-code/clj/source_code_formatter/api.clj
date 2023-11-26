
(ns source-code-formatter.api
    (:require [source-code-formatter.libspecs.format :as libspecs.format]
              [source-code-formatter.ns.format :as ns.format]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; source-code-formatter.libspecs.format
(def format-ns-deps! libspecs.format/format-ns-deps!)

; source-code-formatter.ns.format
(def format-ns! ns.format/format-ns!)
