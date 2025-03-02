#lang racket

(provide uv/sqr
         uv/line
         uv/l2-loss
         uv/line-eq
         uv/loss-line
         uv/loss-line-m)

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
            (v- pred-ys ys))))))))

(define uv/loss-line
  (lambda (xs ys)
    (lambda (theta)
    (((uv/l2-loss uv/line) xs ys) theta))))


(define uv/loss-line-m ;; y = mx + q, q = 0
  (lambda (xs ys)
    (lambda (m)
           ((uv/loss-line xs ys) (list m 0)))))
