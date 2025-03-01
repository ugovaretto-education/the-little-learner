#lang racket

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

(define uv/rank ;; tail recursive
  (λ (t)
    (ranked t 0)))

(define ranked
  (λ (t acc)
    (cond
     ((number? t) acc)
     (else (ranked (vector-ref t 0) (+ 1 acc))))))


(define uv/shape
  (λ (t)
    (cond
     ((= 1 (uv/rank t)) (list (vector-length t)))
     (else (cons (vector-length t) (uv/shape (vector-ref t 0)))))))


(define uv/make-tensor
  (lambda (shape (init-value 0.0))
    (let ((v (make-vector (car shape) init-value)))
      (do ((i 0 (+ i 1)))
          ((or (= i (vector-length v)) (empty? (cdr shape))) v)
        (vector-set! v i (uv/make-tensor (cdr shape) init-value))))))

(define uv/tensor-ref
  (lambda (t coord)
    (if (empty? (cdr coord))
        (vector-ref t (car coord))
        (uv/tensor-ref (vector-ref t (car coord)) (cdr coord)))))

(define uv/tensor-set!
  (lambda (t coord e)
    (if (empty? (cdr coord))
        (vector-set! t (car coord) e)
        (uv/tensor-set! (vector-ref t (car coord)) (cdr coord) e))))

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

(define uv/tensor-foldl
  (lambda (ts f acc)
    (match ts
        ((list h) (uv/tensor-bin-op f acc h))
        (_ (uv/tensor-foldl (cdr ts) f (uv/tensor-bin-op f acc (car ts)))))))

(define uv/tensor-fold
  (lambda (f ts)
    (uv/tensor-foldl (cdr ts) f (car ts))))

(define (uv/tensor-mfold . args)
  (uv/tensor-fold (car args) (cdr args)))

(define uv/md-reduce
  (lambda (f t acc)
    (if (number? (vector-ref t 0))
        (f (vector-reduce f t) acc)
        (do ((i 1 (+ 1 i))
             (a (uv/md-reduce
                 f (vector-ref t 0) acc)
                (f (uv/md-reduce f (vector-ref t i) a))))
            ((= i (vector-length t)) a)))))

(define uv/tensor-rand
  (lambda (t)
    (let ((coord (tensor-rand-coord (uv/shape t) '())))
      (uv/tensor-ref t coord))))

(define tensor-rand-coord
  (lambda (s r)
    (if (empty? s)
        (r)
        (tensor-rand-coord (cdr s) (cons (random (car s)) r)))))

(define uv/tensor-kernel
  (lambda (in out k)
    (tensor-kernel in out k #())))

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


(define uv/tensor-map
  (lambda (f t)
    (if (= (uv/rank t) 1)
        (vector-map f t)
        (vector-map (lambda (e) (uv/tensor-map f e)) t))))

(define uv/build-tensor
  (lambda (shape generator)
    (let ((io (uv/make-tensor shape)))
      (uv/tensor-kernel io io (lambda (coord _) (generator coord))))))
