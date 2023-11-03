
### What are libspecs?

- A libspec is a lib name or a vector containing a lib name followed by options expressed as sequential keywords and arguments.
- Libspec: 'namespace-a', '[namespace-b]'
- Lib name: 'namespace-a', 'namespace-b'
- Prefix list: '(prefix [namespace-a :as a])'
               =>
               '[prefix.namespace-a :as a]'
