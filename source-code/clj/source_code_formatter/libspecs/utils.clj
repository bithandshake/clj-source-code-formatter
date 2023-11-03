
(ns source-code-formatter.libspecs.utils)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn libspec->type
  ; @ignore
  ;
  ; @description
  ; Returns the type of the given libspec string.
  ;
  ; @param (string) libspec
  ;
  ; @example
  ; (libspec->type "namespace-a]")
  ; =>
  ; :raw
  ;
  ; @example
  ; (libspec->type "[namespace-a :as a]")
  ; =>
  ; :vector
  ;
  ; @example
  ; (libspec->type "; [namespace-a :as a]")
  ; =>
  ; :comment
  ;
  ; @example
  ; (libspec->type "(prefix [namespace-a :as a]]")
  ; =>
  ; :list
  ;
  ; @return (keyword)
  ; :comment, :empty, :list, :raw, :vector
  [libspec]
  (case (-> libspec first str)
        "[" :vector ";" :comment "(" :list "" :empty :raw))
