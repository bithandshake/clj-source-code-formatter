
(ns source-code-formatter.libspecs.format
    (:require [cljfmt.core :as fmt]
              [source-code-formatter.libspecs.read  :as libspecs.read]
              [source-code-formatter.libspecs.utils :as libspecs.utils]
              [io.api                               :as io]
              [string.api                           :as string]
              [syntax.api                           :as syntax]
              [vector.api                           :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn indent-line
  ; @ignore
  ;
  ; @description
  ; - Indents the given 'line' string with whitespaces.
  ; - The first line (0th index) gets only 1 whitespace, because it will be placed after the "(:require " string.
  ;
  ; @param (integer) dex
  ; @param (string) line
  ;
  ; @example
  ; (indent-line 3 "[namespace-a :as a]")
  ; =>
  ; "              [namespace-a :as a]"
  ;
  ; @return (string)
  [dex line]
  (if (zero? dex)
      (str (string/multiply " "  1) line)
      (str (string/multiply " " 14) line)))

(defn indent-lines
  ; @ignore
  ;
  ; @description
  ; Indents each line in the 'lines' vector with the 'indent-line' function.
  ;
  ; @param (strings in vector) lines
  ;
  ; @return (strings in vector)
  [lines]
  (vector/->items-indexed lines indent-line))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn align-vector
  ; @ignore
  ;
  ; @description
  ; If the given 'line' is a vector type libspec with ':as', ':refer', etc, it appends whitespaces
  ; to the namespace to make it as long as the longest namespace of all libspecs that are vector types
  ; and also contain ':as', ':refer', etc.
  ;
  ; @param (string) line
  ; @param (integer) ns-length
  ;
  ; @example
  ; (align-vector "[abc :as a]" 5)
  ; =>
  ; "[abc   :as a]"
  ;
  ; @return (string)
  [line ns-length]
  (if (-> line libspecs.utils/line->type (= :vector))
      (if-let [insert-pos (-> line (string/first-dex-of " :"))]
              (let [indent (- ns-length (dec insert-pos))]
                   (string/insert-part line (string/multiply " " indent) insert-pos))
              (-> line))
      (-> line)))

(defn align-vectors
  ; @ignore
  ;
  ; @param (strings in vector) lines
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
  ;  "[mno     :as m]"   <- Only affects on lines that are vector type libspecs and contain ':as', ':refer', etc.
  ;  "[pqrstuv :as p]"]
  ;
  ; @return (strings in vector)
  [lines]
  (letfn [; ...
          (f1 [result line] (if (-> line libspecs.utils/line->type (= :vector))
                                (max (or (-> result)                          0)
                                     (or (-> line (string/first-dex-of " :")) 0))
                                (-> result)))

          ; Returns the longest namespace name of vector type libspecs that contain ':as', ':refer', etc.
          (f0 [lines] (if-let [result (reduce f1 nil lines)] (dec result)))]

         ; If the 'f0' function returns an integer that means there is at least one vector type libspecs that contains ':as', ':refer', etc.
         (if-let [max-vector-ns-length (f0 lines)]
                 (vector/->items lines #(align-vector % max-vector-ns-length))
                 (->             lines))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn order-lines
  ; @ignore
  ;
  ; @description
  ; - Groups the given lines in the 'lines' vector by their type and sorts every group in alphabetical order.
  ; - Order of lines:
  ;   1. Raw libspecs
  ;   2. Vector libspecs
  ;   3. Vector libspecs with ':as', ':refer', etc.
  ;   4. List libspecs
  ;   + Commented lines are compared as they were not commented.
  ; - If a line is a prefix list, this function sorts it recursivelly.
  ;
  ; @param (strings in vector) lines
  ;
  ; @example
  ; (order-lines ["namespace-c" "namespace-a" "; [namespace-d]" "[namespace-c :as c]"])
  ; =>
  ; ["namespace-a"         <- The first group of the output is the raw libspecs.
  ;  "namespace-c"         <- They are ordered alphabetically within their group.
  ;  "[namespace-c :as c]" <- The second group is the vector libspecs in alphabetical order as well.
  ;  "; [namespace-d]"]    <- The ordering function dismisses the comments and handles the commented libspecs as they were not commented.
  ;
  ; @return (strings in vector)
  [lines]
  (letfn [; ...
          (comparator-f [a b] (let [a (string/after-first-occurence a "; " {:return? true})  ; <- Removing the comment semicolon (if any)
                                    b (string/after-first-occurence b "; " {:return? true})] ; <- Removing the comment semicolon (if any)

                                   (cond ; If both 'a' and 'b' lines are vector libspecs ...
                                         ; ...
                                         (= :vector (-> a libspecs.utils/line->type)
                                                    (-> b libspecs.utils/line->type))
                                         (if (string/contains-part? a " :")
                                             (if (string/contains-part? b " :")
                                                 (string/abc? a b)
                                                 (-> false))
                                             (if (string/contains-part? b " :")
                                                 (-> true)
                                                 (string/abc? a b)))

                                         ; If the type of the 'a' and 'b' lines are the same ...
                                         ; ... they are alphabetically compared.
                                         (= (-> a libspecs.utils/line->type)
                                            (-> b libspecs.utils/line->type))
                                         (string/abc? a b)

                                         ; If the type of the 'a' line is ':raw' ...
                                         ; ... the type of 'b' line definitely has a lower order.
                                         (-> a libspecs.utils/line->type (= :raw))
                                         (-> true)

                                         ; If the type of the 'a' line is ':vector' ...
                                         ; ... only ':raw' lines have a higher order.
                                         (-> a libspecs.utils/line->type (= :vector))
                                         (-> b libspecs.utils/line->type (not= :raw))

                                         ; If the type of the 'a' line is ':list' ...
                                         ; ... it definitely has a lower order.
                                         (-> a libspecs.utils/line->type (= :list))
                                         (-> false))))

          ; If the given line is a prefix list, it sorts its items.
          (order-list-f [%] (if (-> % libspecs.utils/line->type (= :list))
                                (-> %)
                                (-> %)))]

         ; Sorts the lines in the given 'lines' vector, then sorts every prefix list lines separatelly.
         (-> lines (vector/sort-items comparator-f)
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

(defn prepare-libspecs
  ; @ignore
  ;
  ; @description
  ; - Joins the libspecs that are splitted into separate rows.
  ;
  ; @param (string) libspecs
  ;
  ; @example
  ; (prepare-libspecs "[namespace-a\n :as a]")
  ; =>
  ; "[namespace-a :as a]"
  ;
  ; @return (string)
  [libspecs]
  (letfn [(f [result cursor]
             (if (-> result (string/cursor-out-of-bounds? cursor))
                 (-> result)
                 (let [observed-character (string/part result cursor (inc cursor))]
                      (case observed-character "[" (let [close-pos      (syntax/close-bracket-position result {:ignore-comments? true :offset cursor})
                                                         updated-result (string/apply-on-part          result cursor close-pos string/remove-newlines)]
                                                        (if (string/same-length? result updated-result)
                                                            (f updated-result (inc cursor))
                                                            (f updated-result (->  cursor (+ 1 (- (- (count result) (count updated-result))))))))))))]

         (f libspecs 0)))

(defn split-line
  ; @ignore
  ;
  ; @description
  ; Takes the 'line' string and splits it into different chunks if it contains more than one libspec.
  ;
  ; @param (string) line
  ; The 'line' string ...
  ; ... must be trimmed,
  ; ... must not contain duplicated whitespaces,
  ; ... must not contain newlines.
  ;
  ; @example
  ; (split-line "namespace-a [namespace-b] ; [namespace-c :as c]")
  ; =>
  ; ["namespace-a"
  ;  "[namespace-b]"
  ;  "; [namespace-c :as c]"]
  ;
  ; @return (strings in vector)
  [line]
  (letfn [; Detaches the first libspec from the given 'line' string.
          ; Returns a vector with the type of the detached libspec string and the detached string itself.
          (detach-libspec-f [line]
                            ; The 'line->type' function checks only the first character of the given string,
                            ; so it does not cause any problem if it takes the whole 'line' string in order to
                            ; determine what is the type of the first libspec in the line.
                            (case (libspecs.utils/line->type line) :vector  (detach-vector-f  line)
                                                                   :comment (detach-comment-f line)
                                                                   :list    (detach-list-f    line)
                                                                   :raw     (detach-raw-f     line)))

          ; Removes the following part after the first libspec (it must be a vector libspec!).
          (detach-vector-f  [line] (let [close-tag-pos (syntax/close-bracket-position line)]
                                        (string/part line 0 (inc close-tag-pos))))

          ; Removes the following part after the first libspec (it must be a prefix list!).
          (detach-list-f    [line] (let [close-tag-pos (syntax/close-paren-position line)]
                                        (string/part line 0 (inc close-tag-pos))))

          ; Removes the following part after the first libspec (it must be a raw libspec!).
          (detach-raw-f     [line] (string/before-first-occurence line " " {:return? true}))

          ; ...
          (detach-comment-f [line] (-> line))

          ; ...
          (split-f [line splitted-line]
                   (println line)
                   (if ; If the 'line' string is finally empty, it quits the splitting and returns the output.
                       (-> line str count zero?)
                       [line splitted-line]
                       ; If the 'line' string is not empty yet, it splits the string by detaching the first libspec
                       ; and calls the 'split-f' recursivelly.
                       (let [first-libspec (detach-libspec-f line)]
                            (split-f (-> line (string/remove-part first-libspec)
                                              (string/trim))
                                     (conj splitted-line first-libspec)))))]

         ; ...
         (let [[_ splitted-line] (split-f line [])]
              (println "--------------------------")
              (-> splitted-line))))

(defn split-lines
  ;
  ; "[a] b c"
  [lines]
  (letfn [(f0 [result line] (vector/concat-items result (split-line line)))]
         (reduce f0 [] lines)))





(defn format-libspecs!
  ; @description
  ; Reads the content of a Clojure source code file found on the given filepath and formats the libspecs within the ':require' macro:
  ; 1. Groups the libspecs by type:
  ;    1. Raw libspecs                               (e.g. 'namespace-a')
  ;    2. Vector libspecs                            (e.g. '[namespace-a]')
  ;    3. Vector libspecs with ':as', ':refer', etc. (e.g. '[namespace-a :as a]')
  ;    4. Prefix lists                               (e.g. '(prefix [namespace-a :as a])')
  ;    + Commented libspecs are ordered as they were not commented.
  ; 2. Sorts the libspecs alphabetically within the type groups.
  ; 3. Indents the libspecs.
  ; 4. Aligns the vector type libspecs.
  ;
  ; @param (string) filepath
  ;
  ; @usage
  ; (format-libspecs! "my-namespace.clj")
  [filepath]
  ;(println "\";abc = (123 + 456)\ndef = ((100) + 200)\"")

  ;(println (fmt/reformat-string "(ns my-namespace\n      (:require [b]\n [a] \n[d] \n[c :refer [\na\nb\nc d e ]]))"))

  (if-let [source-code (io/read-file filepath {:warn? true})]
          (if-let [ns (libspecs.read/source-code->ns source-code)]
                  (if-let [require (libspecs.read/ns->require ns)]
                          (-> require (libspecs.read/require->libspecs)
                                      ;(prepare-libspecs)
                                      ;(split-lines)
                                      ;(order-lines)
                                      (str)
                                      (println)))))
  (println)
  (println))

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
