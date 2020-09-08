# ewt2ptb
The format of English Web Treebank (EWT) is similar but not identical to that of Penn Treebank (PTB).
This code converts EWT into the format of PTB.

To use this code, install [SBCL](http://www.sbcl.org/) and [cl-ppcre](https://edicl.github.io/cl-ppcre/).
Below is a sample script:
```
(require :asdf)
(asdf:load-system :ewt2ptb)
(use-package :ewt2ptb)

(ewt->ptb :ewt "/eng_web_tbk/example.tree" :ptb "/data/example.mrg")
```

