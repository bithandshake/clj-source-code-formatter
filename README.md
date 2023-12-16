
# clj-source-code-formatter

### Overview

The <strong>clj-source-code-formatter</strong> is a set of simple source code formatter functions for Clojure projects.

### deps.edn

```
{:deps {monotech-tools/clj-source-code-formatter {:git/url "https://github.com/monotech-tools/clj-source-code-formatter"
                                                  :sha     "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}}
```

### Current version

Check out the latest commit on the [release branch](https://github.com/monotech-tools/clj-source-code-formatter/tree/release).

### Documentation

The <strong>clj-source-code-formatter</strong> functional documentation is [available here](https://mt-devtools.github.io/clj-source-code-formatter).

### Changelog

You can track the changes of the <strong>clj-source-code-formatter</strong> library [here](CHANGES.md).




; https://github.com/bbatsov/clojure-style-guide




# Usage

> Some parameters of the following functions and some further functions are not discussed in this file.
  To learn more about the available functionality, check out the [functional documentation](documentation/COVER.md)!

### Index

- [How to format libspecs in a Clojure namespace?](#how-to-format-libspecs-in-a-clojure-namespace)

### How to format libspecs in a Clojure namespace?

The [`source-code-formatter.api/format-libspecs!`](documentation/clj/source-code-formatter/API.md#format-libspecs)
function formats the libspecs in the source code of the file found under the given filepath.

```
(format-libspecs! "my-code.clj")
```


It reads the content of the file and formats the libspecs within the ':require' macro:

1. Groups the libspecs by type:
   1. Raw libspecs                               (e.g., 'namespace-a')
   2. Vector libspecs                            (e.g., '[namespace-a]')
   3. Vector libspecs with ':as', ':refer', etc. (e.g., '[namespace-a :as a]')
   4. Prefix lists                               (e.g., '(prefix [namespace-a :as a])')
   + Commented libspecs are grouped as they were not commented.


2. Sorts the libspecs alphabetically within the type groups.

3. Indents the libspecs.

4. Aligns the vector type libspecs.

```
(ns my-namespace
 (:require namespace-a   [namespace-xyz :as xyz]
  [namespace-d] [namespace-c]
  (prefix [namespace-h :as h])
  [namespace-f :as f] [namespace-g :as g] [namespace-e :as e]
  namespace-b))

=>

(ns my-namespace
    (:require namespace-a
              namespace-b
              [namespace-c]
              [namespace-d]
              [namespace-e   :as e]
              [namespace-f   :as f]
              [namespace-g   :as g]
              [namespace-xyz :as xyz]
              (prefix [namespace-h :as h])))  
```
