
# source-code-formatter.api Clojure namespace

##### [README](../../../README.md) > [DOCUMENTATION](../../COVER.md) > source-code-formatter.api

### Index

- [format-libspecs!](#format-libspecs)

### format-libspecs!

```
@description
- Reads the content of a Clojure source code file found on the given filepath and formats the libspecs within the ':require' macro:
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
@example=>
```

<details>
<summary>Source code</summary>

```
(defn format-libspecs!
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

This documentation is generated with the [clj-docs-generator](https://github.com/bithandshake/clj-docs-generator) engine.

