;;;; repl.lisp

(in-package #:fae)

(serapeum:-> sym= (symbol keyword) boolean)
(defun sym= (s1 k1)
  (equal (symbol-name s1) (symbol-name k1)))

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
    ((sym= (car rule) :tables)
     (make-result
      (fset:@ (ff env) :tables)
      env))

    ;; Show domain : TODO Cacheing
    ((sym= (car rule) :domain)
     (make-result
      (fset:@ (ff env) :domain)
      env))
       
    ;; Print a result
    ((sym= (car rule) :eval)
     (let ((result (evaluate-rule (cadr rule) env)))
       (make-result
        result
        env)))
    
    ;; Cut results and print
    ((sym= (car rule) :cut)
     (let ((result (alpha-cut (evaluate-rule (cadr rule) env) (caddr rule))))
       (make-result
        result
        env)))
    
    ;; Strong Cut results and print
    ((sym= (car rule) :s-cut)
     (let ((result (strong-alpha-cut (evaluate-rule (cadr rule) env) (caddr rule))))
       (make-result
        result
        env)))
    
    ;; And
    ((sym= (car rule) :and)
     (let* ((maprules (mapcar (lambda (r)
                                (interpret-rule r env))
                              (rest rule)))
            (fails (remove-if-not (get-key :reason) maprules))
            (results (mapcar (get-key :result) maprules)))
       (if fails
           (first fails)
           (make-result (apply #'fuzzy-and results) env))))

    ;; Or
    ((sym= (car rule) :or)
     (let* ((maprules (mapcar (lambda (r)
                                (interpret-rule r env))
                              (rest rule)))
            (fails (remove-if-not (get-key :reason) maprules))
            (results (mapcar (get-key :result) maprules)))
       (make-result (apply #'fuzzy-or results) env)))

    ;; Not
    ((sym= (car rule) :not)
     (let ((result (interpret-rule (cadr rule) env)))
       (if (fset:@ result :reason)
           result
           (make-result (compl (fset:@ result :result)) env))))
    
    ;; If it defines a rule, append a new table
    ;; The membership function will be the interpretation of the body
    ((sym= (car rule) :rule)
     (let ((rule-body-result (interpret-rule (caddr rule) env)))
       (if (typep rule-body-result 'fail-state)
           rule-body-result
           (make-result
            (cadr rule)
            (cons
             (list
              (fset:map
               (:domain (fset:@ (ff env) :domain))
               (:tables (cons
                         (make-table (cadr rule)
                                     (fset:@ rule-body-result :result))
                         (fset:@ (ff env) :tables)))))
             env)))))
    
    ;; Sequential statements
    ((sym= (car rule) :do)
     (reduce
      (lambda (acc expression)
        (if (typep acc 'fail-state)
            acc
            (interpret-rule expression (fset:@ acc :env))))
      (cdr rule)
      :initial-value (make-result 't env)))

    ;; Breaking into lisp to define attributes
    ((sym= (car rule) :table)
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
    ((sym= (car rule) :attr)
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
    ((sym= (car rule) :delete)
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
    ((sym= (car rule) :mutate)
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
    ((sym= (car rule) :save)
     (store-domain (ff env) (cadr rule))
     (make-result
      't
      env))

    ;; Load domain data from file
    ((sym= (car rule) :load)
     (make-result
      't                                
      (cons
       (list
        (load-into-domain (ff env) (cadr rule)))
       env)))

    ;; Undo env history
    ((sym= (car rule) :undo)
     (if (cadr env)
         (make-result
          't
          (cons (cadr env)
                env))
         (make-fail
          env
          rule))
     )

    ((sym= (car rule) :redo)
     (if (cadr env)
         (make-result
          't
          (rest env))
         (make-fail
          env
          rule)))
    
    (t                                  ;TODO conditions
     (make-fail env rule))))

(serapeum:-> evaluate-rule (t t) t)
(defun evaluate-rule (rule env)
  (fset:image
   (fset:@ (interpret-rule rule env) :result)
   (fset:@ (ff env) :domain)))


;; REPL and compilation
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

