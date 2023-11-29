
(ns source-code-formatter.libspecs.assemble
    (:require [source-code-formatter.libspecs.indents :as libspecs.indents]
              [string.api                             :as string]
              [vector.api                             :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-libspec-newline
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [source-code-map _ directive libspec]
  (if (-> source-code-map :ns first directive :deps first (not= libspec)) "\n"))

(defn assemble-prefixed-namespace-newline
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (string)
  [_ _ _ libspec pref-ns]
  (if (-> libspec :prefixed first (not= pref-ns)) "\n"))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-libspec-opening
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [name]}]
  (str "[" name))

(defn assemble-libspec-closure
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ _]
  (str "]"))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-libspec-indent
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [source-code-map file-content directive libspec]
  (let [libspec-indent-length (libspecs.indents/ns-libspec-indent-length source-code-map file-content directive)]
       (if (-> source-code-map :ns first directive :deps first (not= libspec))
           (string/repeat " " libspec-indent-length)
           (string/repeat " " 1))))

(defn assemble-prefixed-namespace-indent
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (string)
  [source-code-map file-content directive libspec pref-ns]
  (let [pref-ns-indent-length (libspecs.indents/prefixed-namespace-indent-length source-code-map file-content directive libspec)]
       (if (-> libspec :prefixed first (not= pref-ns))
           (string/repeat " " pref-ns-indent-length)
           (string/repeat " " 1))))

(defn assemble-libspec-details-indent
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [source-code-map file-content directive libspec]
  (string/repeat " " (libspecs.indents/libspec-details-indent-length source-code-map file-content directive libspec)))

(defn assemble-prefixed-namespace-details-indent
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (string)
  [source-code-map file-content directive libspec pref-ns]
  (string/repeat " " (libspecs.indents/prefixed-namespace-details-indent-length source-code-map file-content directive libspec pref-ns)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-libspec-alias
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [alias]}]
  (if alias (str " :as " alias)))

(defn assemble-libspec-only
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [only]}]
  (if only (str " :only [" (-> only vector/abc-items (string/join " ")) "]")))

(defn assemble-libspec-refer
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [refer]}]
  (if refer (str " :refer [" (-> refer vector/abc-items (string/join " ")) "]")))

(defn assemble-libspec-rename
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [rename]}]
  (if rename (str " :rename {" (-> rename vector/flat-items (string/join " ")) "}")))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-prefixed-namespace
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (string)
  [source-code-map file-content directive {:keys [prefixed] :as libspec} {:keys [alias only refer rename] :as pref-ns}]
  (str (assemble-prefixed-namespace-newline source-code-map file-content directive libspec pref-ns)
       (assemble-prefixed-namespace-indent  source-code-map file-content directive libspec pref-ns)
       (assemble-libspec-opening            source-code-map file-content directive pref-ns)
       (cond alias  (assemble-prefixed-namespace-details-indent source-code-map file-content directive libspec pref-ns)
             only   (assemble-prefixed-namespace-details-indent source-code-map file-content directive libspec pref-ns)
             refer  (assemble-prefixed-namespace-details-indent source-code-map file-content directive libspec pref-ns)
             rename (assemble-prefixed-namespace-details-indent source-code-map file-content directive libspec pref-ns))
       (assemble-libspec-alias              source-code-map file-content directive pref-ns)
       (assemble-libspec-only               source-code-map file-content directive pref-ns)
       (assemble-libspec-refer              source-code-map file-content directive pref-ns)
       (assemble-libspec-rename             source-code-map file-content directive pref-ns)
       (assemble-libspec-closure            source-code-map file-content directive pref-ns)))

(defn assemble-prefixed-namespaces
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [source-code-map file-content directive {:keys [prefixed] :as libspec}]
  (letfn [(f [result pref-ns] (str result (assemble-prefixed-namespace source-code-map file-content directive libspec pref-ns)))]
         (reduce f nil prefixed)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-basic-libspec
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [source-code-map file-content directive libspec]
  (str (assemble-libspec-newline source-code-map file-content directive libspec)
       (assemble-libspec-indent  source-code-map file-content directive libspec)
       (assemble-libspec-opening source-code-map file-content directive libspec)
       (assemble-libspec-closure source-code-map file-content directive libspec)))

(defn assemble-detailed-libspec
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [source-code-map file-content directive libspec]
  (str (assemble-libspec-newline        source-code-map file-content directive libspec)
       (assemble-libspec-indent         source-code-map file-content directive libspec)
       (assemble-libspec-opening        source-code-map file-content directive libspec)
       (assemble-libspec-details-indent source-code-map file-content directive libspec)
       (assemble-libspec-alias          source-code-map file-content directive libspec)
       (assemble-libspec-only           source-code-map file-content directive libspec)
       (assemble-libspec-refer          source-code-map file-content directive libspec)
       (assemble-libspec-rename         source-code-map file-content directive libspec)
       (assemble-libspec-closure        source-code-map file-content directive libspec)))

(defn assemble-prefixed-libspec
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [source-code-map file-content directive libspec]
  (str (assemble-libspec-newline     source-code-map file-content directive libspec)
       (assemble-libspec-indent      source-code-map file-content directive libspec)
       (assemble-libspec-opening     source-code-map file-content directive libspec)
       (assemble-prefixed-namespaces source-code-map file-content directive libspec)
       (assemble-libspec-closure     source-code-map file-content directive libspec)))

(defn assemble-libspec
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [source-code-map file-content directive {:keys [alias only prefixed refer rename] :as libspec}]
  (cond prefixed (assemble-prefixed-libspec source-code-map file-content directive libspec)
        alias    (assemble-detailed-libspec source-code-map file-content directive libspec)
        only     (assemble-detailed-libspec source-code-map file-content directive libspec)
        refer    (assemble-detailed-libspec source-code-map file-content directive libspec)
        rename   (assemble-detailed-libspec source-code-map file-content directive libspec)
        :basic   (assemble-basic-libspec    source-code-map file-content directive libspec)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-ns-directive-opening
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ;
  ; @return (string)
  [source-code-map file-content directive]
  (let [ns-directive-indent (libspecs.indents/ns-directive-indent-length source-code-map file-content directive)]
       (str "\n" (string/repeat " " ns-directive-indent) "(:" (name directive))))

(defn assemble-ns-directive-closure
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ;
  ; @return (string)
  [_ _ _]
  (str ")"))

(defn assemble-ns-directive-libspecs
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ;
  ; @return (string)
  [source-code-map file-content directive]
  (let [libspecs (-> source-code-map :ns first directive :deps)]
       (letfn [(f [result libspec] (str result (assemble-libspec source-code-map file-content directive libspec)))]
              (reduce f nil libspecs))))

(defn assemble-ns-directive
  ; @ignore
  ;
  ; @param (map) source-code-map
  ; @param (string) file-content
  ; @param (keyword) directive
  ;
  ; @return (string)
  [source-code-map file-content directive]
  (str (assemble-ns-directive-opening  source-code-map file-content directive)
       (assemble-ns-directive-libspecs source-code-map file-content directive)
       (assemble-ns-directive-closure  source-code-map file-content directive)))
