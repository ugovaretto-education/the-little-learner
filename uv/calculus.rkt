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


(define line-eq-l
  (λ (θ)
    (λ (x)
      (+
       (* (car θ) x)
       (cadr θ)))))

(define line-eq-v
  (λ (θ)
    (λ (x)
      (+
       (* (vector-ref θ 0) x)
       (vector-ref θ 1)))))

(define uv/line-eq
  (lambda (xs)
    (cond
     ((list? xs) (line-eq-l xs))
     ((vector? xs) (line-eq-v xs))
     (else "error only list or vector supported"))))

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
                             (f dxs+) ;; call with (f <vector>)
                             (f dxs-)) ;; call with (f <vector>)
             ;;                (apply f (vector->list dxs+)) ;; call with (f x y ...)
             ;;                (apply f (vector->list dxs-)));; call with (f x y ...)
                            dx)))
            gs))))))

(define uv/normalize
  (lambda (xs)
    (let ((m (uv/norm xs)))
      (vector-map (lambda (x) (/ x m)) xs))))

(define uv/norm
  (lambda (xs)
    (sqrt (vsum (vector-map sqr xs)))))
