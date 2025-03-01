#lang racket

(require "utility.rkt")

(provide vector-reduce
         reduce-vectors
         reduce-multi-vectors
         v+
         v-
         vectors-reduce
         vsum
         vmin
         vmax)

(define vector-reduce
  (lambda (f xs)
    (do ((i 1 (+ i 1))
         (a (vector-ref xs 0) (f (vector-ref xs i) a)))
        ((= i (vector-length xs)) a))))


(define reduce-vectors
  (lambda (xs ys f)
      (do ((i 0 (+ i 1))
           (zs (make-vector (vector-length xs) 0.0)))
          ((= i (vector-length zs)) zs)
        (vector-set! zs i (f (vector-ref xs i) (vector-ref ys i))))))

(define apply-element
  (lambda (vs i f acc)
      (cond
       ((empty? vs) acc)
       (else
        (apply-element (cdr vs) i f (f (vector-ref (car vs) i) acc))))))

(define reduce-multi-vectors
  (lambda (vs f)
    (do ((i 0 (+ i 1))
         (f (symbol->function f))
         (zs (make-vector (vector-length (car vs)) 0.0)))
        ((= i (vector-length zs)) zs)
      (vector-set! zs i
                   (apply-element (cdr vs) i f (vector-ref (car vs) 0))))))

(define (v+ . args)
  (reduce-multi-vectors args +))

(define (v- . args)
  (reduce-multi-vectors args -))


;; call as (vectors-reduce )
(define (vectors-reduce  . args)
  (reduce-multi-vectors (cdr args) (car args)))

(define vsum
  (λ (t)
    (vsummed t (- (vector-length t) 1) 0.0)))

(define vsummed
  (λ (t i acc)
    (cond
     ((= i 0) (+ acc (vector-ref t 0)))
     (else
      (vsummed t (- i 1) (+ acc (vector-ref t i)))))))

(define vmin
  (λ (v)
    (vector-argmin (λ (x) x) v)))

(define vmax
  (λ (v)
    (vector-argmax (λ (x) x) v)))
