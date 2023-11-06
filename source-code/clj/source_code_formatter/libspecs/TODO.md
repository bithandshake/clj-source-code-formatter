
- Make the formatter read the '(:require ...)' section from the '(ns ...)' macro!
  Now it reads the '(:require ...)' section from the whole source code and it could
  ruin a file if it has no '(:require ...)' section in the namespace declaration
  but it contains the "(:require " string somewhere else in the file.
