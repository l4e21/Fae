;;;; db.lisp

(in-package #:fuzzy)

;; A Database is a map of Domain and Tables
;; A Table is a map of a title, and membership function
;; A Domain is a set of maps which represent Entities

(serapeum:-> keys (fset:wb-map) fset:wb-set)
(defun keys (xmap)
  (serapeum:assure fset:wb-set
    (fset:reduce (lambda (acc x)
                   (fset:with acc x))
                 xmap
                 :initial-value
                 (fset:empty-set))))

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


(serapeum:-> table-lookup (symbol db) (mu-fn t))
(defun table-lookup (predicate db)
  (serapeum:assure function
    (serapeum:~>
     (remove-if-not (lambda (table)
                      (equal (fset:@ table :title)
                             predicate)) (fset:@ db :tables))
     fset:first
     (fset:@ :mu))))


(serapeum:-> interpret-rule (t db) t)
(defun interpret-rule (rule env)
  (cond
    ;; If it's a symbol look up the membership function
    ((symbolp rule)
     (table-lookup rule env))
    ;; If it's a list, make a new rule and eval
    ((typep (car rule) 'list)
     (let ((new-rule (car rule)))
       (interpret-rule
        (car new-rule)
        (fset:map
         (:domain (fset:@ env :domain))
         (:tables (cons
                   (fset:map
                    (:title (car new-rule))
                    (:mu (interpret-rule
                          (cadr new-rule)
                          env)))
                   (fset:@ env :tables)))))))
    ((equal (car rule) 'and)
     (apply #'fuzzy-and (mapcar (lambda (r)
                                  (interpret-rule r env))
                                (rest rule))))
    ((equal (car rule) 'or)
     (apply #'fuzzy-or (mapcar (lambda (r)
                                 (interpret-rule r env))
                               (rest rule))))
    ((equal (car rule) 'not)
     (compl (interpret-rule (cadr rule) env)))

    ;; If it defines a rule, append a new table
    ;; The membership function will be the interpretation of the body
    ((equal (car rule) 'rule)
     (fset:map
      (:domain (fset:@ env :domain))
      (:tables (cons
                (fset:map
                 (:title (cadr rule))
                 (:mu (interpret-rule
                       (caddr rule)
                       env)))
                (fset:@ env :tables)))))
    ((equal (car rule) 'do)
     (reduce
      (lambda (acc expression)
        (interpret-rule expression acc))
      (cdr rule)
      :initial-value env))
    (t
     (error (format nil "Help ~s" rule)))))

(serapeum:-> evaluate-rule (t db) t)
(defun evaluate-rule (rule env)
  (fset:image (interpret-rule rule env)
              (fset:@ env :domain)))

