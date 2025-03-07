#lang racket

(provide uv/sqr
         uv/line
         uv/line-eq
         uv/gradient)

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
            gs)))))
