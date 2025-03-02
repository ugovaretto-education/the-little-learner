#lang racket

(provide uv/sqr
         uv/line
         uv/l2-loss
         uv/line-eq
         uv/loss-line
         uv/loss-line-m
         uv/gradient
         uv/normalize
         uv/norm)

(require "tensor.rkt"
         "utility.rkt"
         "vector.rkt")

(define epsilon 1e-10)

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

(defne vector-)

(define uv/gradient
  (lambda (f)
    (lambda (xs)
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
                             (apply f (vector->list dxs+))
                             (apply f (vector->list dxs-)))
                            dx))
            gs))))))

(define uv/normalize
  (lambda (xs)
    (let ((m (uv/norm xs)))
      (vector-map (lambda (x) (/ x m)) xs))))

(define uv/norm
  (lambda (xs)
    (sqrt (vsum (vector-map sqr xs)))))
