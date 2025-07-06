#lang racket

;;; This module provides a collection of functions for numerical vector operations.
;;; It includes element-wise arithmetic, reduction, normalization, and other
;;; common vector manipulations.

(require "utility.rkt")

(provide vector-reduce
         reduce-vectors
         v+
         v-
         v*
         vectors-reduce
         vector-foldl
         vsum
         vmin
         vmax
         to-vector
         norm
         normalize
         vector-append!)

;; Converts a scalar, list, or existing vector into a vector.
;; Errors if the input is not one of these types.
(define to-vector
  (lambda (x)
    (cond
      ((scalar? x) (vector x))
      ((vector? x) x)
      ((list? x) (list->vector x))
      (else (error "to-vector: only scalar, list or vector supported"))
    )))

;; Reduces a vector `xs` to a single value by repeatedly applying a binary function `f`.
;; `f` takes two arguments: the current element and the accumulator.
;; (vector-reduce + #(1 2 3)) -> 6
(define vector-reduce
  (lambda (f xs)
    (do ((i 1 (+ i 1))
         (a (vector-ref xs 0) (f (vector-ref xs i) a)))
        ((= i (vector-length xs)) a))))


;; Applies a binary function `f` element-wise to two vectors `xs` and `ys`.
;; Returns a new vector containing the results.
;; (reduce-vectors + #(1 2) #(3 4)) -> #(4 6)
(define reduce-vectors
  (lambda (xs ys f)
      (do ((i 0 (+ i 1))
           (zs (make-vector (vector-length xs) 0.0)))
          ((= i (vector-length zs)) zs)
        (vector-set! zs i (f (vector-ref xs i) (vector-ref ys i))))))

;; Helper function for applying an operation `f` to the `i`-th element of a list of vectors `vs`.
(define apply-element
  (lambda (vs i f acc)
      (cond
       ((empty? vs) acc)
       (else
        (apply-element (cdr vs) i f (f (vector-ref (car vs) i) acc))))))

;; Folds a list of vectors `vs` using a binary function `f` and an initial accumulator `acc`.
;; It applies `reduce-vectors` for each vector in the list.
(define vector-foldl
  (lambda (f vs acc)
    (if (empty? vs)
        acc
        (vector-foldl f (cdr vs) (reduce-vectors acc (car vs) f)))))

;; Variadic function for element-wise vector addition.
;; (v+ #(1 2) #(3 4) #(5 6)) -> #(9 12)
(define (v+ . args)
  (vector-foldl + (cdr args) (car args)))

;; Variadic function for element-wise vector subtraction.
;; (v- #(10 10) #(1 2) #(3 4)) -> #(6 4)
(define (v- . args)
  (vector-foldl - (cdr args) (car args)))

;; Variadic function for element-wise vector multiplication.
;; If the first argument is a scalar, it performs scalar multiplication.
;; (v* 2 #(1 2 3)) -> #(2 4 6)
;; (v* #(1 2) #(3 4)) -> #(3 8)
(define (v* . args)
  (if (scalar? (car args))
      (vector-foldl * (cdr args)
                    (make-vector (vector-length (cadr args)) (car args)))
      (vector-foldl * (cdr args) (car args))))

;; Generic variadic function to reduce a list of vectors.
;; The first argument is the binary function `f` to apply.
;; The second argument is the initial accumulator vector.
;; The rest of the arguments are the vectors to be folded.
(define (vectors-reduce  . args)
  (vector-foldl (car args) (cddr args) (cadr args)))

;; Calculates the sum of all elements in a vector `t`.
(define vsum
  (λ (t)
    (vsummed t (- (vector-length t) 1) 0.0)))

;; Tail-recursive helper function for `vsum`.
(define vsummed
  (λ (t i acc)
    (cond
     ((= i 0) (+ acc (vector-ref t 0)))
     (else
      (vsummed t (- i 1) (+ acc (vector-ref t i)))))))

;; Finds the minimum value in a vector `v`.
;; Note: Depends on `vector-argmin` which is not defined in this file.
(define vmin
  (λ (v)
    (vector-argmin (λ (x) x) v)))

;; Finds the maximum value in a vector `v`.
;; Note: Depends on `vector-argmax` which is not defined in this file.
(define vmax
  (λ (v)
    (vector-argmax (λ (x) x) v)))

;; Normalizes a vector `xs` to have a Euclidean norm (magnitude) of 1.
(define normalize
  (lambda (xs)
    (let ((m (norm xs)))
      (vector-map (lambda (x) (/ x m)) xs))))

;; Calculates the Euclidean norm (L2 norm or magnitude) of a vector `xs`.
;; Also handles scalars.
(define norm
  (lambda (xs)
    (if (scalar? xs)
        (sqrt (sqr xs))
        (sqrt (vsum (vector-map sqr xs))))))

;; Appends an element `e` to a vector `v`.
;; Note: The `!` suggests mutation, but this function returns a *new* vector.
(define vector-append!
  (lambda (v e)
    (let ((v (vector-append v (vector e))))
      v)))
