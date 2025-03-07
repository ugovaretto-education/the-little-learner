#lang racket

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

(define to-vector
  (lambda (x)
    (cond
      ((scalar? x) (vector x))
      ((vector? x) x)
      ((list? x) (list->vector x))
      (else (error "to-vector: only scalar, list or vector supported"))
    )))

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

(define vector-foldl
  (lambda (f vs acc)
    (if (empty? vs)
        acc
        (vector-foldl f (cdr vs) (reduce-vectors acc (car vs) f)))))
  ;; (lambda (vs f)
  ;;   (do ((i 0 (+ i 1))
  ;;        (f (symbol->function f))
  ;;        (zs (make-vector (vector-length (car vs)) 0.0)))
  ;;       ((= i (vector-length zs)) zs)
  ;;     (vector-set! zs i
  ;;                  (apply-element (cdr vs) i f (vector-ref (car vs) 0))))))

(define (v+ . args)
  (vector-foldl + (cdr args) (car args)))

(define (v- . args)
  (vector-foldl - (cdr args) (car args)))

(define (v* . args)
  (if (scalar? (car args))
      (vector-foldl * (cdr args)
                    (make-vector (vector-length (cadr args)) (car args)))
      (vector-foldl * (cdr args) (car args))))

;; call as (vectors-reduce )
(define (vectors-reduce  . args)
  (vector-foldl (car args) (cddr args) (cadr args)))

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

(define normalize
  (lambda (xs)
    (let ((m (norm xs)))
      (vector-map (lambda (x) (/ x m)) xs))))

(define norm
  (lambda (xs)
    (if (scalar? xs)
        (sqrt (sqr xs))
        (sqrt (vsum (vector-map sqr xs))))))

(define vector-append!
  (lambda (v e)
    (let ((v (vector-append v (vector e))))
      v)))
