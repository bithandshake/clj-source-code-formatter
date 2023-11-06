
(ns source-code-formatter.libspecs.utils)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn line->type
  ; @ignore
  ;
  ; @description
  ; Returns the type of the libspec found in the beginning of the given 'line' string.
  ;
  ; @param (string) line
  ;
  ; @example
  ; (line->type "namespace-a]")
  ; =>
  ; :raw
  ;
  ; @example
  ; (line->type "[namespace-a :as a]")
  ; =>
  ; :vector
  ;
  ; @example
  ; (line->type "; [namespace-a :as a]")
  ; =>
  ; :comment
  ;
  ; @example
  ; (line->type "(prefix [namespace-a :as a]]")
  ; =>
  ; :list
  ;
  ; @return (keyword)
  ; :comment, :empty, :list, :raw, :vector
  [line]
  (case (-> line str first str)
        "[" :vector ";" :comment "(" :list "" :empty :raw))
