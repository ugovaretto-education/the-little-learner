#lang racket

;;; This module provides functions for calculus operations,
;;; including a numerical gradient implementation.

(provide uv/sqr
         uv/line
         uv/line-eq
         uv/gradient)

(require "tensor.rkt"
         "utility.rkt"
         "vector.rkt")

;; A small value for numerical stability in gradient calculation.
(define epsilon 1e-10)

;; Squares a scalar or applies sqr to each element of a tensor.
;; (uv/sqr 3) -> 9
;; (uv/sqr (vector 1 2 3)) -> (vector 1 4 9)
(define uv/sqr
  (lambda (xs)
    (if (scalar? xs)
        (sqr xs)
        (uv/tensor-map sqr xs))))


;; Creates a linear function y = mx + c from a list of parameters.
(define line-eq-l
  (λ (θ)
    (λ (x)
      (+
       (* (car θ) x)
       (cadr θ)))))

;; Creates a linear function y = mx + c from a vector of parameters.
(define line-eq-v
  (λ (θ)
    (λ (x)
      (+
       (* (vector-ref θ 0) x)
       (vector-ref θ 1)))))

;; Returns a linear function y = mx + c.
;; The parameters `m` and `c` are taken from the input `xs`,
;; which can be a list or a vector.
(define uv/line-eq
  (lambda (xs)
    (cond
     ((list? xs) (line-eq-l xs))
     ((vector? xs) (line-eq-v xs))
     (else "error only list or vector supported"))))

;; Creates a function that applies a linear equation to a vector of inputs.
;; Takes a vector `xs` and returns a function that takes `theta` (m and c).
;; This returned function then applies the line equation to each element of `xs`.
(define uv/line
  (lambda (xs)
    (lambda (theta)
      (vector-map (uv/line-eq theta) xs))))


;; Calculates the numerical gradient of a function `f`.
;; It takes a function `f` and returns a new function.
;; The returned function takes a vector `xs` and computes the gradient of `f` at `xs`
;; using the finite difference method.
(define uv/gradient
  (lambda (f)
    (lambda (xs) ;; xs must be a vector
      (do ((i 0 (+ i 1))
           (dx (* 2 epsilon))
           (gs (make-vector (vector-length xs)))
           (dxs+ (vector-copy xs) (vector-copy xs))
           (dxs- (vector-copy xs) (vector-copy xs)))
          ((= i (vector-length xs)) gs)
        (let* ((x (vector-ref xs i))
               (x+ (+ x epsilon))
               (x- (- x epsilon)))
          (begin
            (vector-set! dxs+ i x+)
            (vector-set! dxs- i x-)
            (vector-set! gs i
                         (/ (-
                             (f dxs+) ;; call with (f <vector>)
                             (f dxs-)) ;; call with (f <vector>)
             ;;                (apply f (vector->list dxs+)) ;; call with (f x y ...)
             ;;                (apply f (vector->list dxs-)));; call with (f x y ...)
                            dx)))
            gs)))))
