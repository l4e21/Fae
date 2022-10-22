(in-package #:fae/test)

(fiveam:def-suite fuzzy-suite)
(fiveam:in-suite fuzzy-suite)

(defun test-fuzzy-suite ()
  (fiveam:run! 'fuzzy-suite))

(serapeum:-> example-member-func (t) fuzzy-cons)
(defun example-member-func (x)
  (if (numberp x)
      (fuzzy-cons x 1.0)
      (fuzzy-cons x 0.0)))

(serapeum:-> zero-ness (number) fuzzy-cons)
(defun zero-ness (x)
  (serapeum:assure fuzzy-cons
    (fuzzy-cons x (/ 1.0 (serapeum:~> x (abs) (+ 1) (expt 2))))))

(serapeum:-> more-than-zero-ish (number) fuzzy-cons)
(defun more-than-zero-ish (x)
  (serapeum:assure fuzzy-cons
    (fuzzy-cons x (if (<= x 0)
                      0.0
                      (fuzzy-cons-weight (funcall (compl #'zero-ness) x))))))

(serapeum:-> more-than-one-ish (number) fuzzy-cons)
(defun more-than-one-ish (x)
  (serapeum:assure fuzzy-cons
    (fuzzy-cons x (if (<= x 1)
                      0.0
                      (- (fuzzy-cons-weight (funcall (compl #'zero-ness) x)) 0.01)))))

(serapeum:-> example-2-set () fuzzy-set)
(defun example-2-set ()
  "Useage like this! This one finds the union of numbers that show 'zero-ness'
   but also ones that display 'non-zero-ness'"
  (fuzzy-members (fuzzy-union #'zero-ness (compl #'zero-ness))
                 (fset:convert 'fset:wb-set (range -4 4 0.5))))


(serapeum:-> hotness (number) fuzzy-cons)
(defun hotness (x)
  (fuzzy-cons x
              (cond
                ((>= x 50.0)
                 1.0)
                ((>= x 30.0)
                 (/ (- 50.0 x) 20.0))
                (t
                 0.0))))

(serapeum:-> coldness (number) fuzzy-cons)
(defun coldness (x)
  (fuzzy-cons x
              (cond
                ((>= x 10.0)
                 0.0)
                ((>= x 0.0)
                 (- 1.0 (/ (- 10.0 x) 10.0)))
                (t
                 1.0))))

(fiveam:test compilation
  (fiveam:is (equalp
              (fset:empty-set)
              (crisp-members
               (fuzzy-intersection #'example-member-func (compl #'example-member-func))
               (fset:set 1 'hi 2))))
  (fiveam:is (equalp
              (crisp-members
               (fuzzy-union #'example-member-func (compl #'example-member-func)) (fset:set 1 2 3))
              (fset:set 3 2 1)))
  (fiveam:is (fuzzy-subset?
              #'more-than-one-ish
              #'more-than-zero-ish
              (range -4 4 0.5)))
  (fiveam:is (= 0.75
                (scalar-cardinality (fuzzy-members #'more-than-zero-ish (fset:set -1 0 1)))))
  (fiveam:is (= 0.25
                (relative-cardinality (fuzzy-members #'more-than-zero-ish (fset:set -1 0 1))
                                      '(-1 0 1))))
  (fiveam:is (= 0.0
                (funcall (hamming-distance #'coldness #'coldness) (range -50 50))))
  (fiveam:is (equalp (crisp-members (simple-difference #'example-member-func #'example-member-func) (fset:set 1 'hi 2))
                     (fset:empty-set))))

(fiveam:test match
  (fiveam:is (= 1 1)))

(fiveam:test db
  (let* ((crisp-mu
           (lambda (x) (fuzzy-cons x 1.0)))
         (female-mu
           (lambda (x)
             (case (fset:@ x :gender)
               ("Female"
                (fuzzy-cons x
                            1.0))
               ("Androgynous"
                (fuzzy-cons x
                            0.6))
               ("Male"
                (fuzzy-cons x
                            0.0))
               (otherwise
                (fuzzy-cons x
                            0.0)))))
         (parent-mu
           (lambda (x)
             (if (fset:empty? (fset:@ x :children))
                 (fuzzy-cons x 0.0)
                 (fuzzy-cons x 1.0))))
         (big-country-mu
           (lambda (x)
             (let ((land-mass (fset:@ x :land-mass/km2)))
               (if (not land-mass)
                   (fuzzy-cons x 0.0)
                   (cond
                     ((< land-mass 1000)
                      (fuzzy-cons x 0.0))
                     ((< land-mass 500000)
                      (fuzzy-cons x (float (/ land-mass 500000))))
                     (t
                      (fuzzy-cons x 1.0)))))))
         (hot-country-mu
           (lambda (x)
             (let ((temp (fset:@ x :avg-temp/c)))
               (if (not temp)
                   (fuzzy-cons x 0.0)
                   (cond
                     ((< temp 0)
                      (fuzzy-cons x 0.0))
                     ((< temp 26)
                      (fuzzy-cons x (float (/ temp 26))))
                     (t
                      (fuzzy-cons x 1.0)))))))
         (example-db (make-env
                      (fset:map
                       (:domain
                        (fset:set
                         (fset:map
                          (:id "Jane")
                          (:gender "Female")
                          (:children '("John")))
                         (fset:map
                          (:id "John")
                          (:gender "Androgynous"))
                         (fset:map
                          (:id "Steve")
                          (:gender "Male")
                          (:children '("John")))
                         (fset:map
                          (:id "Malta")
                          (:avg-temp/c 27)
                          (:land-mass/km2 316))
                         (fset:map
                          (:id "Britain")
                          (:avg-temp/c 8)
                          (:land-mass/km2 209331))
                         (fset:map
                          (:id "Chad")
                          (:avg-temp/c 27.9)
                          (:land-mass/km2 1284000))))
                       (:tables
                        (list (fset:map
                               (:title 'Crisp)
                               (:mu crisp-mu))
                              (fset:map
                               (:title 'Female)
                               (:mu female-mu))
                              (fset:map
                               (:title 'Parent)
                               (:mu parent-mu))
                              (fset:map
                               (:title 'Big-Country)
                               (:mu big-country-mu))
                              (fset:map
                               (:title 'Hot-Country)
                               (:mu hot-country-mu))))))))

    (fiveam:is (= 6 (fset:size (fset:@ (ff example-db) :domain))))
    (fiveam:is (= 5 (length (fset:@ (ff example-db) :tables))))
    ;; lookup
    (fiveam:is (equal crisp-mu
                      (table-lookup 'Crisp (ff example-db))))
    ;; lookup
    (fiveam:is (equal female-mu
                      (fset:@ (interpret-rule 'Female example-db) :result)))

    ;; eval
    (fiveam:is (equal 6
                      (length
                       (fset:convert 'list
                                     (fset:@
                                      (ff
                                       (fset:@
                                        (interpret-rule
                                         '(mutate Crisp 0.0)
                                         example-db)
                                        :env))
                                      :domain)))))

    ;; and
    (fiveam:is (= 1.6
                  (scalar-cardinality
                   (evaluate-rule
                    '(and Female Crisp)
                    example-db))))
    ;; or
    (fiveam:is (= 6.0
                  (scalar-cardinality
                   (evaluate-rule
                    '(or Female Crisp)
                    example-db))))
    ;; and
    (fiveam:is (= 1.0
                  (scalar-cardinality
                   (evaluate-rule
                    '(and Female Parent)
                    example-db))))
    ;; do, rule, and
    (fiveam:is (= 1.0 (scalar-cardinality
                       (evaluate-rule
                        '(do
                          (rule FemParent (and Female Parent))
                          FemParent)
                        example-db))))
    ;; Table
    (fiveam:is (equal
                'test-table
                (serapeum:~> (interpret-rule
                              '(table test-table (lambda (x) (fuzzy-cons x 1.0)))
                              example-db)
                             (fset:@ :env)
                             ff
                             (fset:@ :tables)
                             first
                             (fset:@ :title))))
    ;; Attr
    (fiveam:is (= 3
                  (length
                   (fset:convert
                    'list
                    (strong-alpha-cut
                     (evaluate-rule
                      '(do (attr :gender) gender)
                      example-db)
                     0.0)))))
    ;; Do
    (fiveam:is (equal
                ""
                (strong-alpha-cut
                 (evaluate-rule
                  '(do
                    (rule Big-And-Hot
                     (and Hot-Country Big-Country))
                    Big-And-Hot)
                  example-db)
                 0.0)))
    ;; Delete
    (fiveam:is (equal 0
                      (length
                       (fset:convert 'list
                                     (fset:@
                                      (ff
                                       (fset:@
                                        (interpret-rule
                                         '(delete Crisp 0.0) example-db)
                                        :env))
                                      :domain)))))
    ;; Strong Cut
    (fiveam:is (equal 2
                      (length
                       (fset:convert 'list
                                     (ff
                                      (fset:@
                                       (interpret-rule
                                        '(eval Female)
                                        example-db)
                                       :env))))))
    ;; Read & Write 1
    (let ((filename "resources/example-db.lisp"))
      (store-domain (ff example-db) filename)
      (fiveam:is (equal 6
                        (length
                         (fset:convert 'list
                                       (fset:@
                                        (load-into-domain
                                         (make-db) filename)
                                        :domain))))))
    ))
