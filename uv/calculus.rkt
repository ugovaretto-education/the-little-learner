#lang racket

(provide uv/sqr
         uv/line
         uv/l2-loss
         uv/line-eq)

(require "tensor.rkt"
         "utility.rkt"
         "vector.rkt")

(define uv/sqr
  (lambda (xs)
    (if (scalar? xs)
        (sqr xs)
        (uv/tensor-map sqr xs))))


(define uv/line-eq
  (λ (θ)
    (λ (x)
      (+
       (* (car θ) x)
       (cadr θ)))))

(define uv/line
  (lambda (xs)
    (lambda (theta)
      (vector-map (uv/line-eq theta) xs))))

(define uv/l2-loss
  (lambda (target-fun)
    (lambda (xs ys)
      (lambda (params)
        (let ((pred-ys ((target-fun xs) params)))
          (vsum
           (uv/sqr
            (v- ys pred-ys))))))))
