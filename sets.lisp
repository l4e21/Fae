;;;; fuzzy.lisp

(in-package #:fuzzy)

;; Crisp Sets have characteristic functions that map elements of X and determines
;; membership. For a Crisp Set, membership is determined by a boolean.
;; Number of members is referred to as cardinality

;; Fuzzy Sets generalise the characteristic function as a membership function.
;; A membership function grades an element by membership level between 0 and 1.

(serapeum:defconstructor fuzzy-cons
  (value t)
  (weight single-float))

(deftype mu-fn (a) `(serapeum:-> (,a) fuzzy-cons))

(deftype set-elem ()
  `(or fuzzy-cons t))

(deftype any-set ()
  `(or fuzzy-set list))

(deftype fuzzy-set ()
  `(satisfies fuzzy-set-p))

(serapeum:-> fuzzy-set-p (t) boolean)
(defun fuzzy-set-p (xs)
  (every (lambda (x) (typep x 'fuzzy-cons)) xs))

(serapeum:-> fuzzy-members ((mu-fn t) list) fuzzy-set)
(defun fuzzy-members (func xs)
  (remove-if (lambda (x) (= (fuzzy-cons-weight x) 0))
             (mapcar func xs)))

(serapeum:-> crisp-members ((mu-fn t) list) list)
(defun crisp-members (func xs)
  (fuzzy-to-crisp (fuzzy-members func xs)))

(serapeum:-> fuzzy-to-crisp (fuzzy-set) list)
(defun fuzzy-to-crisp (s)
  (reduce (lambda (acc x)
            (multiple-value-bind (value weight)
                (serapeum:deconstruct x)
              (if (= weight 1)
                  (cons value acc)
                  acc)))
          s
          :initial-value nil))

(serapeum:-> crisp-to-fuzzy (list) fuzzy-set)
(defun crisp-to-fuzzy (s)
  (mapcar (lambda (x) (fuzzy-cons x 1.0)) s))

(serapeum:-> comp ((mu-fn t)) (mu-fn t))
(defun comp (func)
  (lambda (x)
    (multiple-value-bind (value weight)
        (serapeum:deconstruct (funcall func x))
      (fuzzy-cons value (- 1.0 weight)))))

(serapeum:-> fuzzy-intersection ((mu-fn t) (mu-fn t)) (mu-fn t))
(defun fuzzy-intersection (fn1 fn2)
  (lambda (x)
    (serapeum:mvlet ((v1 w1 (serapeum:deconstruct (funcall fn1 x)))
                     (v2 w2 (serapeum:deconstruct (funcall fn2 x))))
      (if (< w1 w2)
          (fuzzy-cons v1 w1)
          (fuzzy-cons v2 w2)))))

(serapeum:-> fuzzy-union ((mu-fn t) (mu-fn t)) (mu-fn t))
(defun fuzzy-union (fn1 fn2)
  (lambda (x)
    (serapeum:mvlet ((v1 w1 (serapeum:deconstruct (funcall fn1 x)))
                     (v2 w2 (serapeum:deconstruct (funcall fn2 x))))
      (if (> w1 w2)
          (fuzzy-cons v1 w1)
          (fuzzy-cons v2 w2)))))

(serapeum:-> subset (any-set any-set) boolean)
(defun subset (s1 s2)
  (if (reduce
       (lambda (acc x)
         (remove-if (lambda (y) (= y x)) acc))
       s2
       :initial-value s1)
      nil
      t))

(serapeum:-> set= (any-set any-set) boolean)
(defun set= (s1 s2)
  (and (subset s1 s2)
       (subset s2 s1)))

