
(ns source-code-formatter.api
    (:require [source-code-formatter.ns.format :as ns.format]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @tutorial How to format dependency libspecs in a Clojure/ClojureScript namespace?
;
; The ['source-code-formatter.api/format-ns-deps!'](#format-ns-deps!) function formats the dependency
; libspecs within the namespace declaration in the source code file at the given filepath.
;
; @usage
; (format-libspecs! "my_namespace.clj")
;
; @code
; (ns my-namespace
;         (:require namespace-a   [namespace-xyz :as xyz]
;            [namespace-d] [namespace-c]
;           (prefix [namespace-h :as h])
;          [namespace-f :as f] [namespace-g :as g] [namespace-e :as e]
;          namespace-b))
; =>
; (ns my-namespace
;     (:require namespace-a
;               namespace-b
;               [namespace-c]
;               [namespace-d]
;               [namespace-e   :as e]
;               [namespace-f   :as f]
;               [namespace-g   :as g]
;               [namespace-xyz :as xyz]
;               (prefix [namespace-h :as h])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @redirect (source-code-formatter.ns.format/*)
(def format-ns-deps! ns.format/format-ns-deps!)
