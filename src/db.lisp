;;;; db.lisp

(in-package #:fae)

;; A Database is a map of Domain and Tables
;; A Table is a map of a title, and membership function
;; A Domain is a set of maps which represent Entities

(defun ff (xs)
  (first (first xs)))

(serapeum:-> get-key (keyword) function)
(defun get-key (kw)
  (lambda (xmap) (fset:@ xmap kw)))

(serapeum:-> keys (fset:wb-map) fset:wb-set)
(defun keys (xmap)
  (serapeum:assure fset:wb-set
    (fset:reduce (lambda (acc x)
                   (fset:with acc x))
                 xmap
                 :initial-value
                 (fset:empty-set))))

;; Tables
(deftype table ()
  `(satisfies table-p))

(serapeum:-> table-p (fset:wb-map) boolean)
(defun table-p (table)
  (serapeum:assure boolean
    (and
     (typep table 'fset:wb-map)
     (fset:@ table :title)
     (typep (fset:@ table :title) 'symbol)
     (fset:@ table :mu)
     (typep (fset:@ table :mu) 'function))))

(serapeum:-> make-table (symbol function) table)
(defun make-table (&optional
                     (title 't)
                     (mu (lambda (x) (fuzzy-cons x 1.0))))
  (fset:map (:title title) (:mu mu)))

;; Databases
(deftype db ()
  `(satisfies db-p))

(serapeum:-> db-p (fset:wb-map) boolean)
(defun db-p (db)
  (and
   (fset:@ db :domain)
   (typep (fset:@ db :domain) 'fset:wb-set)
   (every (lambda (table)
            (typep table 'table))
          (fset:@ db :tables))))

(serapeum:-> make-db (fset:wb-set (or (cons table) null)) db)
(defun make-db (&optional
                  (domain (fset:empty-set))
                  (tables (list (make-table))))
  (fset:map (:domain domain)
            (:tables tables)))


(defun attribute (kw)
  (make-table (read-from-string (symbol-name kw))
              (lambda (x) (if (fset:@ x kw)
                         (fuzzy-cons x 1.0)
                         (fuzzy-cons x 0.0)))))


(serapeum:-> table-lookup (symbol db) (mu-fn t))
(defun table-lookup (predicate db)
  (serapeum:assure function
    (serapeum:~>
     (remove-if-not (lambda (table)
                      (equal (fset:@ table :title)
                             predicate)) (fset:@ db :tables))
     fset:first
     (fset:@ :mu))))

(defun store-domain (db filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (print (mapcar (lambda (entity) (fset:convert 'list entity))
                   (fset:convert 'list (fset:@ db :domain)))
           stream)))

(defun load-into-domain (db filename)
  (with-open-file (stream filename)
    (fset:map
     (:domain (fset:union (fset:@ db :domain)
                          (fset:convert 'fset:wb-set
                           (mapcar
                            (lambda (entity) (fset:convert 'fset:wb-map entity))
                            (read stream)))))
     (:tables (fset:@ db :tables)))))

(deftype result-state ()
  `(satisfies result-state-p))

(serapeum:-> result-state-p (fset:wb-map) boolean)
(defun result-state-p (state)
  (and
   (fset:@ state :env)
   (fset:@ state :result)))

(serapeum:-> init-env () t)
(defun init-env ()
  (list (list (make-db))))

(serapeum:-> make-env (db) t)
(defun make-env (db)
  (list (list db)))


(defun make-result (&optional (result nil) (env (init-env)))
  (fset:map (:result result)
            (:env env)))


(deftype fail-state ()
  `(satisfies fail-state-p))

(serapeum:-> fail-state-p (fset:wb-map) boolean)
(defun fail-state-p (state)
  (and
   (fset:@ state :reason)
   (fset:@ state :env)
   (fset:@ state :result)))

(serapeum:-> make-fail (t t t) fail-state)
(defun make-fail (env reason)
  (fset:map
   (:result 'fail)
   (:env env)
   (:reason reason)))
