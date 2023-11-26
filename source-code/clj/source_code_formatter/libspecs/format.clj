
(ns source-code-formatter.libspecs.format
    (:require [source-code-formatter.libspecs.read  :as libspecs.read]
              [source-code-formatter.libspecs.utils :as libspecs.utils]
              [source-code-map.api                  :as source-code-map]
              [io.api                               :as io]
              [seqable.api                          :as seqable]
              [string.api                           :as string]
              [syntax.api                           :as syntax]
              [syntax-reader.api                    :as syntax-reader]
              [vector.api                           :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn compare-deps
  ; @ignore
  ;
  ; @param (map) a
  ; @param (map) b
  ;
  ; @return (boolean)
  [a b]
  (letfn [(f0 [{:keys [alias only rename refer]}] (or alias only rename refer))
          (f1 [{:keys [prefixed]}]                (-> prefixed))]
         (cond (and (f0 a) (f0 b)) (string/abc? (:name a) (:name b))
               (and (f1 a) (f1 b)) (string/abc? (:name a) (:name b))
               (and (f0 a) (f1 b)) (-> true)
               (and (f1 a) (f0 b)) (-> false)
               (and (f0 a))        (-> false)
               (and (f1 a))        (-> false)
               (and        (f0 b)) (-> true)
               (and        (f1 b)) (-> true)
               :both-basic         (string/abc? (:name a) (:name b)))))

(defn longest-libspec-name
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ;
  ; @return (string)
  [_ _ deps]
  (letfn [(f [result {:keys [alias name refer]}]
             (if (or alias refer)
                 (max result (count name))
                 (->  result)))]
         (reduce f 0 deps)))

(defn libspec-details-indent-length
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result directive deps {:keys [name]}]
  (- (longest-libspec-name result directive deps)
     (count name)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-libspec-newline
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result _ _ _]
  (if result "\n"))

(defn assemble-libspec-opening
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [_ _ _ {:keys [name]}]
  (str "[" name))

(defn assemble-libspec-closure
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [_ _ _ _]
  (str "]"))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-libspec-indent
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result _ _ _]
  (if result (string/repeat " " 14)
             (string/repeat " "  1)))

(defn assemble-libspec-details-indent
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result directive deps {:keys [alias refer] :as dep}]
  (cond alias (string/repeat " " (libspec-details-indent-length result directive deps dep))
        refer (string/repeat " " (libspec-details-indent-length result directive deps dep))))

(defn assemble-prefixed-namespace-indent
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result _ _ {:keys [name]}]
  (if result (string/repeat " " (+ 16 (count name)))
             (string/repeat " "  1)))

(defn assemble-libspec-alias
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [_ _ _ {:keys [alias]}]
  (if alias (str " :as " alias)))

(defn assemble-libspec-only
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [_ _ _ {:keys [only]}]
  (if only (str " :only [" (-> only vector/abc-items (string/join " ")) "]")))

(defn assemble-libspec-rename
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [_ _ _ {:keys [rename]}]
  (if rename (str " :rename {" (-> rename vector/flat-items (string/join " ")) "}")))

(defn assemble-libspec-refer
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [_ _ _ {:keys [refer]}]
  (if refer (str " :refer [" (-> refer vector/abc-items (string/join " ")) "]")))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-prefixed-namespace
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result directive deps {:keys [prefixed] :as dep} pref]
  (str result (assemble-libspec-newline           result directive deps pref)
              (assemble-prefixed-namespace-indent result directive deps dep)
              (assemble-libspec-opening           result directive deps pref)
              (assemble-libspec-details-indent    result directive prefixed pref)
              (assemble-libspec-alias             result directive deps pref)
              (assemble-libspec-only              result directive deps pref)
              (assemble-libspec-refer             result directive deps pref)
              (assemble-libspec-rename            result directive deps pref)
              (assemble-libspec-closure           result directive deps pref)))

(defn assemble-prefixed-namespaces
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result directive deps {:keys [prefixed] :as dep}]
  (let [prefixed (vector/sort-items prefixed compare-deps)]
       (letfn [(f [result pref] (assemble-prefixed-namespace result directive deps dep pref))]
              (reduce f nil prefixed))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-basic-libspec
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result directive deps dep]
  (str result (assemble-libspec-newline result directive deps dep)
              (assemble-libspec-indent  result directive deps dep)
              (assemble-libspec-opening result directive deps dep)
              (assemble-libspec-closure result directive deps dep)))

(defn assemble-detailed-libspec
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result directive deps dep]
  (str result (assemble-libspec-newline        result directive deps dep)
              (assemble-libspec-indent         result directive deps dep)
              (assemble-libspec-opening        result directive deps dep)
              (assemble-libspec-details-indent result directive deps dep)
              (assemble-libspec-alias          result directive deps dep)
              (assemble-libspec-only           result directive deps dep)
              (assemble-libspec-refer          result directive deps dep)
              (assemble-libspec-rename         result directive deps dep)
              (assemble-libspec-closure        result directive deps dep)))

(defn assemble-prefixed-libspec
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result directive deps dep]
  (str result (assemble-libspec-newline     result directive deps dep)
              (assemble-libspec-indent      result directive deps dep)
              (assemble-libspec-opening     result directive deps dep)
              (assemble-prefixed-namespaces result directive deps dep)
              (assemble-libspec-closure     result directive deps dep)))

(defn assemble-libspec
  ; @ignore
  ;
  ; @param (string) result
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ; @param (map) dep
  ;
  ; @return (string)
  [result directive deps {:keys [alias only prefixed rename refer] :as dep}]
  (cond prefixed (assemble-prefixed-libspec result directive deps dep)
        alias    (assemble-detailed-libspec result directive deps dep)
        only     (assemble-detailed-libspec result directive deps dep)
        refer    (assemble-detailed-libspec result directive deps dep)
        rename   (assemble-detailed-libspec result directive deps dep)
        :basic   (assemble-basic-libspec    result directive deps dep)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn assemble-ns-directive-opening
  ; @ignore
  ;
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ;
  ; @return (string)
  [directive _]
  (str "\n" (string/repeat " " 4) "(" directive))

(defn assemble-ns-directive-closure
  ; @ignore
  ;
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ;
  ; @return (string)
  [_ _]
  (str ")"))

(defn assemble-ns-directive-libspecs
  ; @ignore
  ;
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ;
  ; @return (string)
  [directive deps]
  (let [deps (vector/sort-items deps compare-deps)]
       (letfn [(f [result dep] (assemble-libspec result directive deps dep))]
              (reduce f nil deps))))

(defn assemble-ns-directive
  ; @ignore
  ;
  ; @param (keyword) directive
  ; @param (maps in vector) deps
  ;
  ; @return (string)
  [directive deps]
  (str (assemble-ns-directive-opening  directive deps)
       (assemble-ns-directive-libspecs directive deps)
       (assemble-ns-directive-closure  directive deps)))

(defn assemble-ns-deps
  ; @ignore
  ;
  ; @param (map) source-code-map
  ;
  ; @return (string)
  [{:keys [ns]}]
  (str (if-let [import  (-> ns first :ns/import)]  (assemble-ns-directive :import  import))
       (if-let [require (-> ns first :ns/require)] (assemble-ns-directive :require require))
       (if-let [use     (-> ns first :ns/use)]     (assemble-ns-directive :use     use))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn source-code->before-ns-deps
  [source-code])


(defn format-ns-deps!
  ; @description
  ; Reads the content of a Clojure source code file found on the given filepath and formats the libspecs within the namespace declaration:
  ; 1. Groups the libspecs by type:
  ;    1. Raw libspecs                               (e.g. 'namespace-a')                       ; <- now it is vectorized also!
  ;    2. Vector libspecs                            (e.g. '[namespace-a]')
  ;    3. Vector libspecs with ':as', ':refer', etc. (e.g. '[namespace-a :as a]')
  ;    4. Prefixed libspecs                          (e.g. '[prefix [namespace-a :as a]]')
  ;    + Commented libspecs are ordered as they were not commented.
  ; 2. Sorts the libspecs alphabetically within the type groups.
  ; 3. Indents the libspecs.
  ; 4. Aligns the vector type libspecs.
  ;
  ; @param (string) filepath
  ;
  ; @usage
  ; (format-ns-deps! "my-namespace.clj")
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (if-let [source-code-map (source-code-map/read-source-file filepath)]
                  (str
                       (assemble-ns-deps source-code-map)))))



  ;(if-let [source-code (io/read-file filepath {:warn? true})]
  ;        (if-let [ns (libspecs.read/source-code->ns source-code)]
  ;                (if-let [require (libspecs.read/ns->require ns)]
  ;                        (-> require (libspecs.read/require->libspecs))])
                                      ;(prepare-libspecs)
                                      ;(split-lines)
                                      ;(order-lines)
  ;                                    (str)
  ;                                    (println))))))
  ;(println)
  ;(println))

  ;        (if (string/contains-part? source-code "(:require ")
  ;            (io/write-file! filepath (str (libspecs.read/source-code->before-require source-code)
  ;                                          (-> source-code (libspecs.read/source-code->require)
  ;                                                          (libspecs.read/require->libspecs)
  ;                                                          (split-libspecs)
  ;                                                          (order-libspecs)
  ;                                                          (align-vectors)
  ;                                                          (indent-libspecs)
  ;                                                          (join-libspecs)
  ;                                                          (wrap-libspecs)
  ;                                          (libspecs.read/source-code->after-require source-code)])
