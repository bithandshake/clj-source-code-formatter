
(ns source-code-formatter.libspecs.read
    (:require [regex.api         :as regex]
              [string.api        :as string]
              [syntax-reader.api :as syntax-reader]
              [vector.api        :as vector]))

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
          (if-let [require-end-pos (syntax-reader/paren-closing-position source-code {:offset require-start-pos})]
                  (-> source-code (string/part (inc require-end-pos))))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn source-code->ns
  [source-code]
  ; ns-start: First position of a newline character that is followed by only whitespaces (if any) and the string "(ns ".
  (if-let [ns-start (regex/first-dex-of source-code #"(?<=\n[ ]{0,})\(ns\s")]
          (if-let [ns-end (syntax-reader/paren-closing-position source-code {:offset ns-start})]
                  (-> source-code (string/part ns-start (inc ns-end))
                                  (string/trim-start)))))

(defn ns->require
  [ns]
  ; require-start: First position of a newline character that is followed by only whitespaces (if any) and the string "(:require ".
  (if-let [require-start (regex/first-dex-of ns #"(?<=\n[ ]{0,})\(\:require\s")]
          (if-let [require-end (syntax-reader/paren-closing-position ns {:offset require-start})]
                  (-> ns (string/part require-start (inc require-end))
                         (string/trim-start)))))

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
  ;(if-let [ns-start-pos
  ;         (string/first-dex-of source-code "(ns")]
  (if-let [require-start-pos (string/first-dex-of source-code "(:require ")]
          (if-let [require-end-pos (syntax-reader/paren-closing-position source-code {:offset require-start-pos})]
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
  ; Takes the require section of the 'ns' macro read and formatted by the 'source-code->require'
  ; function and ...
  ; ... removes the "(:require " string from the beginning,
  ; ... removes its close paren from the end.
  ;
  ; @param (string) require
  ;
  ; @example
  ; (require->libspecs "(:require namespace-a [namespace-b] ; [namespace-c :as c]\n (prefix [namespace-d] [namespace-e]))")
  ; =>
  ; "namespace-a [namespace-b] ; [namespace-c :as c]\n (prefix [namespace-d] [namespace-e])"
  ;
  ; @return (string)
  [require]
  (-> require (string/after-first-occurence "(:require " {:return? false})
              (string/before-last-occurence ")"          {:return? false})))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn libspecs->chunks
  [libspecs])
