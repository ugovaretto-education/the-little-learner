#lang racket

;;; This module provides a basic tensor implementation, where tensors are
;;; represented as nested vectors. It includes functions for creating,
;;; manipulating, and performing operations on these tensors.

(provide uv/rank
         uv/shape
         uv/make-tensor
         uv/tensor-ref
         uv/tensor-set!
         uv/tensor-fold
         uv/tensor-mfold
         uv/md-reduce
         uv/tensor-kernel
         uv/tensor-bin-op
         uv/tensor-map
         uv/build-tensor)

(require "vector.rkt" "utility.rkt")

;; Calculates the rank (number of dimensions) of a tensor.
;; A scalar has rank 0, a vector rank 1, a matrix rank 2, and so on.
(define uv/rank
  (λ (t)
    (ranked t 0)))

;; Helper function for uv/rank, recursively determines the rank.
(define ranked
  (λ (t acc)
    (cond
     ((number? t) acc)
     (else (ranked (vector-ref t 0) (+ 1 acc))))))


;; Returns the shape of a tensor as a list of its dimensions.
;; (uv/shape (uv/make-tensor '(2 3))) -> '(2 3)
(define uv/shape
  (λ (t)
    (cond
     ((= 1 (uv/rank t)) (list (vector-length t)))
     (else (cons (vector-length t) (uv/shape (vector-ref t 0)))))))


;; Creates a new tensor with the given `shape` and initializes all
;; elements to `init-value`.
(define uv/make-tensor
  (lambda (shape (init-value 0.0))
    (let ((v (make-vector (car shape) init-value)))
      (do ((i 0 (+ i 1)))
          ((or (= i (vector-length v)) (empty? (cdr shape))) v)
        (vector-set! v i (uv/make-tensor (cdr shape) init-value))))))

;; Retrieves an element from a tensor `t` at the specified `coord` (a list of indices).
(define uv/tensor-ref
  (lambda (t coord)
    (if (empty? (cdr coord))
        (vector-ref t (car coord))
        (uv/tensor-ref (vector-ref t (car coord)) (cdr coord)))))

;; Sets the value of an element `e` in a tensor `t` at the specified `coord`.
(define uv/tensor-set!
  (lambda (t coord e)
    (if (empty? (cdr coord))
        (vector-set! t (car coord) e)
        (uv/tensor-set! (vector-ref t (car coord)) (cdr coord) e))))

;; Applies a binary function `f` element-wise to two tensors `t1` and `t2`.
(define uv/tensor-bin-op
  (lambda (f t1 t2)
      (if (number? (vector-ref t1 0))
          (reduce-vectors t1 t2 f)
          (do ((i 0 (+ 1 i))
               (t (make-vector (vector-length t1))))
              ((= i (vector-length t1)) t)
            (vector-set! t i
                         (uv/tensor-bin-op
                          f
                          (vector-ref t1 i) (vector-ref t2 i)))))))

;; Helper function for uv/tensor-fold, performs a left-fold over a list of tensors.
(define uv/tensor-foldl
  (lambda (ts f acc)
    (match ts
        ((list h) (uv/tensor-bin-op f acc h))
        (_ (uv/tensor-foldl (cdr ts) f (uv/tensor-bin-op f acc (car ts)))))))

;; Folds a list of tensors `ts` using a binary function `f`.
;; The first tensor in the list is used as the initial accumulator.
(define uv/tensor-fold
  (lambda (f ts)
    (uv/tensor-foldl (cdr ts) f (car ts))))

;; Variadic version of `uv/tensor-fold`.
(define (uv/tensor-mfold . args)
  (uv/tensor-fold (car args) (cdr args)))

;; Reduces a multi-dimensional tensor `t` to a single value by repeatedly applying `f`.
(define uv/md-reduce
  (lambda (f t acc)
    (if (number? (vector-ref t 0))
        (f (vector-reduce f t) acc)
        (do ((i 1 (+ 1 i))
             (a (uv/md-reduce
                 f (vector-ref t 0) acc)
                (f (uv/md-reduce f (vector-ref t i) a))))
            ((= i (vector-length t)) a)))))

;; Retrieves a random element from the tensor `t`.
(define uv/tensor-rand
  (lambda (t)
    (let ((coord (tensor-rand-coord (uv/shape t) '())))
      (uv/tensor-ref t coord))))

;; Helper to generate a random coordinate within the shape `s`.
(define tensor-rand-coord
  (lambda (s r)
    (if (empty? s)
        (r)
        (tensor-rand-coord (cdr s) (cons (random (car s)) r)))))

;; Applies a kernel function `k` to each element of the input tensor `in`,
;; storing the results in the `out` tensor.
(define uv/tensor-kernel
  (lambda (in out k)
    (tensor-kernel in out k #())))

;; Recursive helper for `uv/tensor-kernel`.
(define tensor-kernel
  (lambda (in out k coord)
    (if (= 1 (uv/rank in))
        (do ((i 0 (+ 1 i)))
            ((= i (vector-length in)) out)
          (vector-set! out i
                       (k (vector-append coord (vector i)) (vector-ref in i))))
        (do ((i 0 (+ 1 i)))
            ((= i (vector-length in)) out)
          (tensor-kernel
           (vector-ref in i)
           (vector-ref out i)
           k
           (vector-append coord (vector i)))))))


;; Applies a function `f` to each element of a tensor `t`,
;; returning a new tensor with the results.
(define uv/tensor-map
  (lambda (f t)
    (if (= (uv/rank t) 1)
        (vector-map f t)
        (vector-map (lambda (e) (uv/tensor-map f e)) t))))

;; Builds a tensor of a given `shape` by calling a `generator` function
;; for each coordinate. The generator function receives the coordinate and returns the value.
(define uv/build-tensor
  (lambda (shape generator)
    (let ((io (uv/make-tensor shape)))
      (uv/tensor-kernel io io (lambda (coord _) (generator coord))))))
