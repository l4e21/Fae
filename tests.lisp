(in-package #:fuzzy)

(fiveam:def-suite fuzzy-suite)

(fiveam:in-suite fuzzy-suite)

(defun test-quasi ()
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
