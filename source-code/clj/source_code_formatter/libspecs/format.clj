
(ns source-code-formatter.libspecs.format
    (:require [source-code-formatter.libspecs.read  :as libspecs.read]
              [source-code-formatter.libspecs.utils :as libspecs.utils]
              [io.api                               :as io]
              [string.api                           :as string]
              [syntax.api                           :as syntax]
              [vector.api                           :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn indent-libspec
  ; @ignore
  ;
  ; @description
  ; - Indents the given libspec string with whitespaces.
  ; - The first libspec (0th index) gets only 1 whitespace, because it will be placed after the "(:require " string.
  ;
  ; @param (integer) dex
  ; @param (string) libspec
  ;
  ; @example
  ; (indent-libspec 3 "[namespace-a :as a]")
  ; =>
  ; "              [namespace-a :as a]"
  ;
  ; @return (string)
  [dex libspec]
  (if (= dex 0)
      (str (string/multiply " "  1) libspec)
      (str (string/multiply " " 14) libspec)))

(defn indent-libspecs
  ; @ignore
  ;
  ; @description
  ; Indents each libspec in the 'libspecs' vector by the 'indent-libspec' function.
  ;
  ; @param (strings in vector) libspecs
  ;
  ; @return (strings in vector)
  [libspecs]
  (vector/->items-indexed libspecs indent-libspec))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn align-vector
  ; @ignore
  ;
  ; @description
  ; If the given libspec is a vector type libspec with ':as', ':refer', etc, it appends whitespaces
  ; to the namespace to make it as long as the longest namespace of all libspecs that are vector types
  ; and also contain ':as', ':refer', etc.
  ;
  ; @param (string) libspec
  ; @param (integer) max-vector-ns-length
  ;
  ; @example
  ; (align-vector "[abc :as a]" 5)
  ; =>
  ; "[abc   :as a]"
  ;
  ; @return (string)
  [libspec max-vector-ns-length]
  (if (-> libspec libspecs.utils/libspec->type (= :vector))
      (if-let [insert-pos (-> libspec (string/first-dex-of " :"))]
              (let [indent (- max-vector-ns-length (dec insert-pos))]
                   (string/insert-part libspec (string/multiply " " indent) insert-pos))
              (-> libspec))
      (-> libspec)))

(defn align-vectors
  ; @ignore
  ;
  ; @param (strings in vector) libspecs
  ;
  ; @example
  ; (align-vectors ["abc"
  ;                 "defghi"
  ;                 "[jkl]"
  ;                 "[mno :as m]"
  ;                 "[pqrstuv :as p]"])
  ; =>
  ; ["abc"
  ;  "defghi"
  ;  "[jkl]"
  ;  "[mno     :as m]"   <- Only affects on libspecs that are vector types and contain ':as', ':refer', etc.
  ;  "[pqrstuv :as p]"]
  ;
  ; @return (strings in vector)
  [libspecs]
  (letfn [; ...
          (f1 [result libspec] (if (-> libspec libspecs.utils/libspec->type (= :vector))
                                   (max (or (-> result)                             0)
                                        (or (-> libspec (string/first-dex-of " :")) 0))
                                   (-> result)))

          ; Returns the longest namespace name of vector type libspecs that contain ':as', ':refer', etc.
          (f0 [libspecs] (if-let [result (reduce f1 nil libspecs)] (dec result)))]

         ; If the 'f0' function returns an integer that means there is at least one vector type libspecs that contains ':as', ':refer', etc.
         (if-let [max-vector-ns-length (f0 libspecs)]
                 (vector/->items libspecs #(align-vector % max-vector-ns-length))
                 ;(println max-vector-ns-length))
                 libspecs)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn split-libspecs
  ; @ignore
  ;
  ; @description
  ; Takes the libspecs string formatted by the 'require->libspecs' function and splits it into separate strings.
  ;
  ; @param (string) libspecs
  ; The 'libspecs' string ...
  ; ... must be trimmed,
  ; ... must not contain duplicated whitespaces,
  ; ... must not contain newlines.
  ;
  ; @example
  ; (split-libspecs "namespace-a [namespace-b] ; [namespace-c :as c] (prefix [namespace-d] [namespace-e])")
  ; =>
  ; ["namespace-a" "[namespace-b]" "; [namespace-c :as c]" "(prefix [namespace-d] [namespace-e])"]
  ;
  ; @return (strings in vector)
  [libspecs]
  (letfn [; Detaches the first libspec from the given 'libspecs' string.
          ; Returns a vector with the type of the detached libspec string and the detached string itself.
          (detach-libspec-f [libspecs]
                            ; The 'libspec->type' function works only with the first character of the given string,
                            ; so it does not cause any problem if it takes the whole 'libspecs' string in order to
                            ; determine what is the type of the first libspec in the string.
                            (case (libspecs.utils/libspec->type libspecs)
                                  :vector  (detach-vector-f  libspecs)
                                  :comment (detach-comment-f libspecs)
                                  :list    (detach-list-f    libspecs)
                                  :raw     (detach-raw-f     libspecs)))

          ; Removes the following part after the first libspec (it must be a vector libspec!).
          (detach-vector-f  [libspecs] (let [close-tag-pos (syntax/close-bracket-position libspecs)]
                                            (string/part libspecs 0 (inc close-tag-pos))))

          ; Removes the following part after the first libspec (it must be a prefix list!).
          (detach-list-f    [libspecs] (let [close-tag-pos (syntax/close-paren-position libspecs)]
                                            (string/part libspecs 0 (inc close-tag-pos))))

          ; Removes the following part after the first libspec (it must be a raw libspec!).
          (detach-raw-f     [libspecs] (let [next-ws-pos (string/first-dex-of libspecs " ")]
                                            (string/part libspecs 0 next-ws-pos)))

          ; Removes the following part after the first libspec (it must be a commented libspec with any type!).
          (detach-comment-f [libspecs] (as-> libspecs % (string/after-first-occurence % "; " {:return? false})
                                                        (detach-libspec-f             %)
                                                        (str                     "; " %)))

          ; ...
          (split-f [libspecs splitted-libspecs]
                   (if ; If the libspecs string is finally empty, it quits the splitting and returns the output.
                       (-> libspecs str count (= 0))
                       [libspecs splitted-libspecs]
                       ; If the libspecs string is not empty yet, it splits the string by detaching the first libspec
                       ; and calls itself recursivelly.
                       (let [first-libspec (detach-libspec-f libspecs)]
                            (split-f (-> libspecs (string/remove-part first-libspec)
                                                  (string/trim))
                                     (conj splitted-libspecs first-libspec)))))]

         ; ...
         (let [[_ splitted-libspecs] (split-f libspecs [])]
              (-> splitted-libspecs))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn order-libspecs
  ; @ignore
  ;
  ; @description
  ; - Groups the given libspecs in the 'libspecs' vector by their type and sorts every group in alphabetical order.
  ; - Order of libspecs:
  ;   1. Raw libspecs
  ;   2. Vector libspecs
  ;   3. Vector libspecs with ':as', ':refer', etc.
  ;   4. List libspecs
  ;   + Commented libspecs are compared as they were not commented.
  ; - If a libspec is a prefix list, this function sorts it recursivelly.
  ;
  ; @param (strings in vector) libspecs
  ;
  ; @example
  ; (order-libspecs ["namespace-c" "namespace-a" "; [namespace-d]" "[namespace-c :as c]"])
  ; =>
  ; ["namespace-a"         <- The first group of the output is the raw libspecs.
  ;  "namespace-c"         <- They are ordered alphabetically in theirt group.
  ;  "[namespace-c :as c]" <- The second group is the vector libspecs in alphabetical order as well.
  ;  "; [namespace-d]"]    <- The ordering function dismisses the comments and handles the commented libspecs as they were not commented.
  ;
  ;
  ; @return (strings in vector)
  [libspecs]
  (letfn [; ...
          (comparator-f [a b] (let [a (string/after-first-occurence a "; " {:return? true})  ; <- Removing the comment semicolon (if any)
                                    b (string/after-first-occurence b "; " {:return? true})] ; <- Removing the comment semicolon (if any)

                                   (cond ; If both 'a' and 'b' libspecs are vectors ...
                                         ; ...
                                         (= :vector (-> a libspecs.utils/libspec->type)
                                                    (-> b libspecs.utils/libspec->type))
                                         (if (string/contains-part? a " :")
                                             (if (string/contains-part? b " :")
                                                 (string/abc? a b)
                                                 (-> false))
                                             (if (string/contains-part? b " :")
                                                 (-> true)
                                                 (string/abc? a b)))

                                         ; If the type of the 'a' and 'b' libspecs are the same ...
                                         ; ... they are alphabetically compared.
                                         (= (-> a libspecs.utils/libspec->type)
                                            (-> b libspecs.utils/libspec->type))
                                         (string/abc? a b)

                                         ; If the type of the 'a' libspec is ':raw' ...
                                         ; ... the type of 'b' libspec definitely has a lower order.
                                         (-> a libspecs.utils/libspec->type (= :raw))
                                         (-> true)

                                         ; If the type of the 'a' libspec is ':vector' ...
                                         ; ... only ':raw' libspecs have a higher order.
                                         (-> a libspecs.utils/libspec->type (= :vector))
                                         (-> b libspecs.utils/libspec->type (not= :raw))

                                         ; If the type of the 'a' libspec is ':list' ...
                                         ; ... it definitely has a lower order.
                                         (-> a libspecs.utils/libspec->type (= :list))
                                         (-> false))))

          ; If the given libspec is a prefix list, sorts its items.
          (order-list-f [%] (if (-> % libspecs.utils/libspec->type (= :list))
                                (-> %)
                                (-> %)))]

         ; Sorts the libspecs in the given 'libspecs' vector, then sorts every prefix list separatelly.
         (-> libspecs (vector/sort-items comparator-f)
                      (vector/->items    order-list-f))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn join-libspecs
  ; @ignore
  ;
  ; @description
  ; Takes the indented libspecs in a vector of strings and joins them into a string with newlines between each libspec.
  ;
  ; @param (strings in vector)
  ;
  ; @return (string)
  [libspecs]
  (string/join libspecs "\n"))

(defn wrap-libspecs
  ; @ignore
  ;
  ; @description
  ; Takes the joint libspecs in a string and wraps them with ':require' macro.
  ;
  ; @param (string)
  ;
  ; @return (string)
  [libspecs]
  (-> libspecs (string/prepend ":require")
               (syntax/paren)
               (string/prepend (string/multiply " " 4))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn format-libspecs!
  ; @description
  ; - Reads the content of a Clojure source code file found on the given filepath and formats the libspecs within the ':require' macro:
  ;   1. Groups the libspecs by type:
  ;      1. Raw libspecs                               (e.g. 'namespace-a')
  ;      2. Vector libspecs                            (e.g. '[namespace-a]')
  ;      3. Vector libspecs with ':as', ':refer', etc. (e.g. '[namespace-a :as a]')
  ;      4. Prefix lists                               (e.g. '(prefix [namespace-a :as a])')
  ;      + Commented libspecs are ordered as they were not commented.
  ;   2. Sorts the libspecs alphabetically within the type groups.
  ;   3. Indents the libspecs.
  ;   4. Aligns the vector type libspecs.
  ;
  ; @param (string) filepath
  ;
  ; @example
  ; (format-libspecs! "my-namespace.clj")
  [filepath]
  (if-let [source-code (io/read-file filepath {:warn? true})]
          (io/write-file! filepath (str (libspecs.read/source-code->before-require source-code)
                                        (-> source-code (libspecs.read/source-code->require)
                                                        (libspecs.read/require->libspecs)
                                                        (split-libspecs)
                                                        (order-libspecs)
                                                        (align-vectors)
                                                        (indent-libspecs)
                                                        (join-libspecs)
                                                        (wrap-libspecs))
                                        (libspecs.read/source-code->after-require source-code)))))
