
(ns source-code-formatter.libspecs.read
    (:require [string.api :as string]
              [syntax.api :as syntax]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn source-code->before-require
  ; @ignore
  ;
  ; @description
  ; Takes the source-code of a Clojure namespace returns the part before the require section.
  ;
  ; @param (string) source-code
  ;
  ; @return (string)
  [source-code]
  (if-let [require-start-pos (string/first-dex-of source-code "(:require ")]
          (-> source-code (string/part 0 require-start-pos)
                          (string/trim-end)
                          (string/trim-newlines)
                          (string/append "\n"))))

(defn source-code->after-require
  ; @ignore
  ;
  ; @description
  ; Takes the source-code of a Clojure namespace returns the part after the require section.
  ;
  ; @param (string) source-code
  ;
  ; @return (string)
  [source-code]
  (if-let [require-start-pos (string/first-dex-of source-code "(:require ")]
          (if-let [require-end-pos (syntax/close-paren-position source-code require-start-pos)]
                  (-> source-code (string/part (inc require-end-pos))))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn source-code->require
  ; @ignore
  ;
  ; @description
  ; Takes the source-code of a Clojure namespace and ...
  ; ... removes the part before and after the require section,
  ; ... removes the newlines from the output,
  ; ... removes the duplicated whitespaces from the output,
  ; ... removes the leading and trailing whitespaces (trim) from the output.
  ;
  ; @param (string) source-code
  ;
  ; @example
  ; (source-code->require "\n(ns my-namespace\n  (:require namespace-a\n    [namespace-b :as b]))\n...")
  ; =>
  ; "(:require namespace-a [namespace-b :as b])"
  ;
  ; @return (string)
  [source-code]
  (if-let [require-start-pos (string/first-dex-of source-code "(:require ")]
          (if-let [require-end-pos (syntax/close-paren-position source-code require-start-pos)]
                  (-> source-code (string/part require-start-pos (inc require-end-pos))
                                  (string/replace-part #"[\r\n]" " ")
                                  (string/trim-gaps)
                                  (string/trim)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn require->libspecs
  ; @ignore
  ;
  ; @description
  ; Takes the require section of the 'ns' macro read and formatted by the 'source-code->require' function
  ; and removes the "(:require " string from the beginning and its close paren from the end.
  ;
  ; @param (string) require
  ;
  ; @example
  ; (require->libspecs "(:require namespace-a [namespace-b] ; [namespace-c :as c] (prefix [namespace-d] [namespace-e]))")
  ; =>
  ; "namespace-a [namespace-b] ; [namespace-c :as c] (prefix [namespace-d] [namespace-e])"
  ;
  ; @return (string)
  [require]
  (-> require (string/after-first-occurence "(:require " {:return? false})
              (string/before-last-occurence ")"          {:return? false})
              (string/trim)))
