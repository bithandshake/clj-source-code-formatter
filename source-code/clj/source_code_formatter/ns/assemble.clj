
(ns source-code-formatter.ns.assemble
    (:require [source-code-formatter.ns.indents :as ns.indents]
              [string.api                       :as string]
              [syntax-reader.api                :as syntax-reader]
              [vector.api                       :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-libspec-newline
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ source-code-map directive libspec]
  (if (-> source-code-map :ns first directive :deps first (not= libspec)) "\n"))

(defn assemble-prefixed-namespace-newline
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
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
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [name]}]
  (str "[" name))

(defn assemble-libspec-closure
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
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
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [file-content source-code-map directive libspec]
  (let [libspec-indent-length (ns.indents/libspec-indent-length file-content source-code-map directive)]
       (if (-> source-code-map :ns first directive :deps first (not= libspec))
           (string/repeat " " libspec-indent-length)
           (string/repeat " " 1))))

(defn assemble-prefixed-namespace-indent
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (string)
  [file-content source-code-map directive libspec pref-ns]
  (let [pref-ns-indent-length (ns.indents/prefixed-namespace-indent-length file-content source-code-map directive libspec)]
       (if (-> libspec :prefixed first (not= pref-ns))
           (string/repeat " " pref-ns-indent-length)
           (string/repeat " " 1))))

(defn assemble-libspec-details-indent
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [file-content source-code-map directive libspec]
  (string/repeat " " (ns.indents/libspec-details-indent-length file-content source-code-map directive libspec)))

(defn assemble-prefixed-namespace-details-indent
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (string)
  [file-content source-code-map directive libspec pref-ns]
  (string/repeat " " (ns.indents/prefixed-namespace-details-indent-length file-content source-code-map directive libspec pref-ns)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-libspec-alias
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [alias]}]
  (if alias (str " :as " alias)))

(defn assemble-libspec-only
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [only]}]
  (if only (str " :only [" (-> only vector/abc-items (string/join " ")) "]")))

(defn assemble-libspec-refer
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [_ _ _ {:keys [refer]}]
  (if refer (str " :refer [" (-> refer vector/abc-items (string/join " ")) "]")))

(defn assemble-libspec-rename
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
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
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ; @param (map) pref-ns
  ;
  ; @return (string)
  [file-content source-code-map directive {:keys [prefixed] :as libspec} {:keys [alias only refer rename] :as pref-ns}]
  (str (assemble-prefixed-namespace-newline file-content source-code-map directive libspec pref-ns)
       (assemble-prefixed-namespace-indent  file-content source-code-map directive libspec pref-ns)
       (assemble-libspec-opening            file-content source-code-map directive pref-ns)
       (cond alias  (assemble-prefixed-namespace-details-indent file-content source-code-map directive libspec pref-ns)
             only   (assemble-prefixed-namespace-details-indent file-content source-code-map directive libspec pref-ns)
             refer  (assemble-prefixed-namespace-details-indent file-content source-code-map directive libspec pref-ns)
             rename (assemble-prefixed-namespace-details-indent file-content source-code-map directive libspec pref-ns))
       (assemble-libspec-alias              file-content source-code-map directive pref-ns)
       (assemble-libspec-only               file-content source-code-map directive pref-ns)
       (assemble-libspec-refer              file-content source-code-map directive pref-ns)
       (assemble-libspec-rename             file-content source-code-map directive pref-ns)
       (assemble-libspec-closure            file-content source-code-map directive pref-ns)))

(defn assemble-prefixed-namespaces
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [file-content source-code-map directive {:keys [prefixed] :as libspec}]
  (letfn [(f [result pref-ns] (str result (assemble-prefixed-namespace file-content source-code-map directive libspec pref-ns)))]
         (reduce f nil prefixed)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-basic-libspec
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [file-content source-code-map directive libspec]
  (str (assemble-libspec-newline file-content source-code-map directive libspec)
       (assemble-libspec-indent  file-content source-code-map directive libspec)
       (assemble-libspec-opening file-content source-code-map directive libspec)
       (assemble-libspec-closure file-content source-code-map directive libspec)))

(defn assemble-detailed-libspec
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [file-content source-code-map directive libspec]
  (str (assemble-libspec-newline        file-content source-code-map directive libspec)
       (assemble-libspec-indent         file-content source-code-map directive libspec)
       (assemble-libspec-opening        file-content source-code-map directive libspec)
       (assemble-libspec-details-indent file-content source-code-map directive libspec)
       (assemble-libspec-alias          file-content source-code-map directive libspec)
       (assemble-libspec-only           file-content source-code-map directive libspec)
       (assemble-libspec-refer          file-content source-code-map directive libspec)
       (assemble-libspec-rename         file-content source-code-map directive libspec)
       (assemble-libspec-closure        file-content source-code-map directive libspec)))

(defn assemble-prefixed-libspec
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [file-content source-code-map directive libspec]
  (str (assemble-libspec-newline     file-content source-code-map directive libspec)
       (assemble-libspec-indent      file-content source-code-map directive libspec)
       (assemble-libspec-opening     file-content source-code-map directive libspec)
       (assemble-prefixed-namespaces file-content source-code-map directive libspec)
       (assemble-libspec-closure     file-content source-code-map directive libspec)))

(defn assemble-libspec
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (map) libspec
  ;
  ; @return (string)
  [file-content source-code-map directive {:keys [alias only prefixed refer rename] :as libspec}]
  (cond prefixed (assemble-prefixed-libspec file-content source-code-map directive libspec)
        alias    (assemble-detailed-libspec file-content source-code-map directive libspec)
        only     (assemble-detailed-libspec file-content source-code-map directive libspec)
        refer    (assemble-detailed-libspec file-content source-code-map directive libspec)
        rename   (assemble-detailed-libspec file-content source-code-map directive libspec)
        :basic   (assemble-basic-libspec    file-content source-code-map directive libspec)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-ns-directive-opening
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (string)
  [file-content source-code-map directive]
  (let [ns-directive-indent (ns.indents/ns-directive-indent-length file-content source-code-map directive)]
       (str "(:" (name directive))))

(defn assemble-ns-directive-closure
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (string)
  [_ _ _]
  (str ")"))

(defn assemble-ns-directive-libspecs
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (string)
  [file-content source-code-map directive]
  (let [libspecs (-> source-code-map :ns first directive :deps)]
       (letfn [(f [result libspec] (str result (assemble-libspec file-content source-code-map directive libspec)))]
              (reduce f nil libspecs))))

(defn assemble-ns-directive
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (string)
  [file-content source-code-map directive]
  (str (assemble-ns-directive-opening  file-content source-code-map directive)
       (assemble-ns-directive-libspecs file-content source-code-map directive)
       (assemble-ns-directive-closure  file-content source-code-map directive)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-ns-directive-comment
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ; @param (string) comment
  ;
  ; @return (string)
  [file-content source-code-map directive comment]
  (let [libspec-indent-length (ns.indents/libspec-indent-length file-content source-code-map directive)]
       (str comment (string/repeat " " libspec-indent-length))))

(defn assemble-ns-directive-comments
  ; @ignore
  ;
  ; @param (string) file-content
  ; @param (map) source-code-map
  ; @param (keyword) directive
  ;
  ; @return (string)
  [file-content source-code-map directive]
  (if-let [[started-at ended-at :as ns-directive-bounds] (-> source-code-map :ns first directive :bounds)]
          (let [commented-parts (syntax-reader/get-commented-parts file-content {} {:offset started-at :endpoint ended-at})]
               (-> commented-parts (vector/->items (fn [%] (assemble-ns-directive-comment file-content source-code-map directive %)))
                                   (string/join)))))
