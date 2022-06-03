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

(fiveam:test compilation
  (fiveam:is (not
              (fuzzy-members (fuzzy-intersection #'example-member-func (comp #'example-member-func))
                             '(1 hi 2))))
  (fiveam:is (set=
              (crisp-members (fuzzy-union #'example-member-func (comp #'example-member-func)) '(1 2 3))
              '(3 2 1))))
