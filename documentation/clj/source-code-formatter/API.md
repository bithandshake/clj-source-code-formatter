
### source-code-formatter.api

Functional documentation of the source-code-formatter.api Clojure namespace

---

##### [README](../../../README.md) > [DOCUMENTATION](../../COVER.md) > source-code-formatter.api

### Index

- [format-libspecs!](#format-libspecs)

- [format-ns!](#format-ns)

---

### format-libspecs!

```
@description
Reads the content of a Clojure source code file found on the given filepath and formats the libspecs within the ':require' macro:
1. Groups the libspecs by type:
   1. Raw libspecs                               (e.g. 'namespace-a')
   2. Vector libspecs                            (e.g. '[namespace-a]')
   3. Vector libspecs with ':as', ':refer', etc. (e.g. '[namespace-a :as a]')
   4. Prefix lists                               (e.g. '(prefix [namespace-a :as a])')
   + Commented libspecs are ordered as they were not commented.
2. Sorts the libspecs alphabetically within the type groups.
3. Indents the libspecs.
4. Aligns the vector type libspecs.
```

```
@param (string) filepath
```

```
@usage
(format-libspecs! "my-namespace.clj")
```

<details>
<summary>Source code</summary>

```
(defn format-libspecs!
  [filepath]


  (if-let [source-code (io/read-file filepath {:warn? true})]
          (if-let [ns (libspecs.read/source-code->ns source-code)]
                  (if-let [require (libspecs.read/ns->require ns)]
                          (-> require (libspecs.read/require->libspecs)
                                      (str)
                                      (println)))))
  (println)
  (println))
```

</details>

<details>
<summary>Require</summary>

```
(ns my-namespace (:require [source-code-formatter.api :refer [format-libspecs!]]))

(source-code-formatter.api/format-libspecs! ...)
(format-libspecs!                           ...)
```

</details>

---

### format-ns!

```
@description
Reads the content of a Clojure source code file found on the given filepath and formats the 'ns' macro.
```

```
@param (string) filepath
```

```
@usage
(format-ns! "my-namespace.clj")
```

<details>
<summary>Source code</summary>

```
(defn format-ns!
  [filepath]
  (if-let [source-code (io/read-file filepath {:warn? true})]
          (if-let [ns (ns.read/source-code->ns source-code)]
                  (-> ns (fmt/reformat-string)
                         (indent-lines)
                         (println)))))
```

</details>

<details>
<summary>Require</summary>

```
(ns my-namespace (:require [source-code-formatter.api :refer [format-ns!]]))

(source-code-formatter.api/format-ns! ...)
(format-ns!                           ...)
```

</details>

---

<sub>This documentation is generated with the [clj-docs-generator](https://github.com/bithandshake/clj-docs-generator) engine.</sub>

