;;;; db.lisp

(in-package #:fae)

;; A Database is a map of Domain and Tables
;; A Table is a map of a title, and membership function
;; A Domain is a set of maps which represent Entities

(defun ff (xs)
  (first (first xs)))

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

;; Interpreter
(serapeum:-> interpret-rule (t t) result-state)
(defun interpret-rule (rule env)
  (cond
    ;; If it's a symbol look up the membership function
    ((symbolp rule)
     (make-result
      (table-lookup rule (ff env))
      env))

    ;; If number just return the number
    ((numberp rule)
     (make-result rule env))

    ;; Show tables
    ((equal (car rule) 'tables)
     (make-result
      (fset:@ (ff env) :tables)
      env))

    ;; Show domain : TODO Cacheing
    ((equal (car rule) 'domain)
     (make-result
      (fset:@ (ff env) :domain)
      env))
       
    ;; Print a result
    ((equal (car rule) 'eval)
     (let ((result (evaluate-rule (cadr rule) env)))
       (make-result
        result
        env)))
    
    ;; Cut results and print
    ((equal (car rule) 'cut)
     (let ((result (alpha-cut (evaluate-rule (cadr rule) env) (caddr rule))))
       (make-result
        result
        env)))
    
    ;; Strong Cut results and print
    ((equal (car rule) 's-cut)
     (let ((result (strong-alpha-cut (evaluate-rule (cadr rule) env) (caddr rule))))
       (make-result
        result
        env)))
    
    ;; And
    ((equal (car rule) 'and)
     (make-result
      (apply #'fuzzy-and (mapcar (lambda (r)
                                   (fset:@ (interpret-rule r env) :result))
                                 (rest rule)))
      env))
    ;; Or
    ((equal (car rule) 'or)
     (make-result
      (apply #'fuzzy-or (mapcar (lambda (r)
                                  (fset:@ (interpret-rule r env) :result))
                                (rest rule)))
      env))
    ;; Not
    ((equal (car rule) 'not)
     (make-result (compl (fset:@
                          (interpret-rule (cadr rule) env)
                          :result))
                  env))
    
    ;; If it defines a rule, append a new table
    ;; The membership function will be the interpretation of the body
    ((equal (car rule) 'rule)
     (make-result
      (cadr rule)
      (cons
       (list
        (fset:map
         (:domain (fset:@ (ff env) :domain))
         (:tables (cons
                   (fset:map
                    (:title (cadr rule))
                    (:mu (fset:@
                          (interpret-rule
                           (caddr rule)
                           env) :result)))
                   (fset:@ (ff env) :tables)))))
       env)))
    
    ;; Sequential statements
    ((equal (car rule) 'do)
     (reduce
      (lambda (acc expression)
        (interpret-rule expression (fset:@ acc :env)))
      (cdr rule)
      :initial-value (make-result 't env)))

    ;; Breaking into lisp to define attributes
    ((equal (car rule) 'table)
     (make-result
      (cadr rule)
      (cons
       (list
        (fset:map
         (:domain (fset:@ (ff env) :domain))
         (:tables (cons
                   (make-table (cadr rule) (eval (caddr rule)))
                   (fset:@ (ff env) :tables)))))
       env)))
    
    ;; Define an attribute
    ((equal (car rule) 'attr)
     (make-result
      't
      (cons
       (list
        (fset:map
         (:domain (fset:@ (ff env) :domain))
         (:tables (cons (attribute (cadr rule))
                        (fset:@ (ff env) :tables)))))
       env)))
    

    ;; Delete entities by a certain reasoning
    ;; Either attribute must be a certain value
    ;; Or a rule must have a certain cut
    ((equal (car rule) 'delete)
     (if (typep (cadr rule) 'keyword)
         (make-result
          't
          (cons
           (list
            (fset:map
             (:domain (fset:filter
                       (lambda (x) (not (equal (fset:@ x (cadr rule))
                                          (caddr rule))))
                       (fset:@ (ff env) :domain)))
             (:tables (fset:@ (ff env) :tables))))
           env))
         (make-result
          't
          (cons
           (list
            (fset:map
             (:domain
              (fset:set-difference
               (fset:@ (ff env) :domain)
               (fset:image
                #'fuzzy-cons-value
                (strong-alpha-cut
                 (evaluate-rule (cadr rule) env)
                 (caddr rule)))))
             (:tables (fset:@ (ff env) :tables))))
           env))))

    ;; Mutate
    ((equal (car rule) 'mutate)
     (if (typep (cadr rule) 'keyword)
         (make-result
          't
          (cons
           (list
            (fset:map
             (:domain (fset:filter
                       (lambda (x) (equal (fset:@ x (cadr rule))
                                     (caddr rule)))
                       (fset:@ (ff env) :domain)))
             (:tables (fset:@ (ff env) :tables))))
           env))
         (make-result
          't
          (cons (list
                 (fset:map
                  (:domain
                   (fset:image
                    #'fuzzy-cons-value
                    (strong-alpha-cut
                     (evaluate-rule (cadr rule) env)
                     (caddr rule))))
                  (:tables (fset:@ (ff env) :tables))))
                env))))

    ;; Save domain data in file
    ((equal (car rule) 'save)
     (store-domain (ff env) (cadr rule))
     (make-result
      't
      env))

    ;; Load domain data from file
    ((equal (car rule) 'load)
     (make-result
      't                                
      (cons
       (list
        (load-into-domain (ff env) (cadr rule)))
       env)))

    ;; Undo env history
    ((equal (car rule) 'undo)
     (if (cadr env)
         (make-result
          't
          (cons (cadr env)
                env))
         (make-result
          'fail
          env))
     ;; go to last point in the history
     )

    ((equal (car rule) 'redo)
     (if (cadr env)
         (make-result
          't
          (rest env))
         (make-result
          'fail
          env))
     )
    
    (t                                  ;TODO fail reasons
     (make-result 'fail env))))

(serapeum:-> evaluate-rule (t t) t)
(defun evaluate-rule (rule env)
  (fset:image
   (fset:@ (interpret-rule rule env) :result)
   (fset:@ (ff env) :domain)))


(defun fae (env)
  (let* ((user-input (progn
                       (format t "FAE> ")
                       (read)))
         (state (interpret-rule user-input env)))
    (pprint (fset:@ state :result))
    (terpri)
    (fae (fset:@ state :env))))


(defun init! (&optional (script nil))
  (if script
      "TODO"
      (progn
        (pprint "FAE (Fuzzy Analysis Engine) V0.01")
        (terpri)
        (fae (make-env (make-db))))))

