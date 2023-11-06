
(ns source-code-formatter.ns.read
    (:require [regex.api  :as regex]
              [string.api :as string]
              [syntax.api :as syntax]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn source-code->before-ns
  ; @ignore
  ;
  ; @description
  ; Takes the source-code of a Clojure namespace and returns the part before the (first) namespace declaration.
  ;
  ; @param (string) source-code
  ;
  ; @return (string)
  [source-code]
  (if-let [ns-start-pos (regex/first-dex-of source-code #"(?<=\n[ ]{0,})\(ns\s")]
          (-> source-code (string/part 0 ns-start-pos)
                          (string/trim-end)
                          (string/trim-newlines)
                          (string/append "\n"))))

(defn source-code->after-ns
  ; @ignore
  ;
  ; @description
  ; Takes the source-code of a Clojure namespace and returns the part after the (first) namespace declaration.
  ;
  ; @param (string) source-code
  ;
  ; @return (string)
  [source-code]
  (if-let [ns-start-pos (regex/first-dex-of source-code #"(?<=\n[ ]{0,})\(ns\s")]
          (if-let [ns-end-pos (syntax/close-paren-position source-code {:offset ns-start-pos})]
                  (-> source-code (string/part (inc ns-end-pos))))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn source-code->ns
  ; @ignore
  ;
  ; @description
  ; Takes the source-code of a Clojure namespace and returns the (first) namespace declaration.
  ;
  ; @param (string) source-code
  ;
  ; @return (string)
  [source-code]
  ; ns-start: First position of a newline character that is followed by only whitespaces (if any) and the string "(ns ".
  (if-let [ns-start-pos (regex/first-dex-of source-code #"(?<=\n[ ]{0,})\(ns\s")]
          (if-let [ns-end-pos (syntax/close-paren-position source-code {:offset ns-start-pos})]
                  (-> source-code (string/part ns-start-pos (inc ns-end-pos))
                                  (string/trim-start)))))
