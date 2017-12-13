(ql:quickload :wordnet-dsl)
(in-package :wordnet-dsl)

(defparameter wn (read-wn "../dict/*.txt"))

(txt->rdf wn "own-en.nt")
