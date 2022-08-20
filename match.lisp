;;;; match.lisp

(in-package #:fuzzy)

(serapeum:-> varp (symbol) boolean)
(defun varp (pattern)
  (equal "?" (subseq (string pattern) 0 1)))

(serapeum:-> restvarp (symbol) boolean)
(defun restvarp (pattern)
  (equal "?&" (subseq (string pattern) 0 2)))

(defun pmatch (pattern sym-list &optional (acc (fset:empty-map)))
  (cond
    ((not pattern)
     acc)
    ((restvarp (car pattern))
     (fset:map (fset:$ acc)
               ((car pattern)
                sym-list)))
    ((varp (car pattern))
     (pmatch (cdr pattern) (cdr sym-list)
             (fset:map (fset:$ acc)
                       ((car pattern)
                        (car sym-list)))))
    ((eql (car pattern)
          (car sym-list))
     (pmatch (cdr pattern) (cdr sym-list) acc))))
