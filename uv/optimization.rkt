#lang racket

(provide uv/revise
         uv/grad-descent
         uv/line-loss
         uv/grad-descent-line
         uv/grad-descent-line-log
         uv/l2-loss
         uv/l2-loss-sample
         uv/loss-line
         uv/loss-line-m)

(require "calculus.rkt")
(require "vector.rkt")
(require "utility.rkt")

(define rev-eps 1e-8)

(define uv/revise
  (lambda (f revisions p)
    (if (or (zero? revisions) (<= (- (norm (f p)) (norm p)) rev-eps))
        p
        (uv/revise f (- revisions 1) (f p)))))

(define uv/grad-descent
  (lambda (f revisions p a)
    (uv/revise
     (lambda (v)
       (v- v (v* a ((uv/gradient f) (to-vector v)))))
       revisions
       p)))

(define uv/line-loss
  (lambda (xs ys)
    (lambda (theta)
      (((uv/l2-loss uv/line) xs ys) theta))))

(define uv/grad-descent-line
  (lambda (xs ys revisions p a)
    (uv/grad-descent (uv/line-loss xs ys) revisions p a)))

(define uv/revise-log
  (lambda (f revisions p (log (lambda (_ __) (null))))
    (do ((i 0 (+ i 1))
         (np p (f np)))
        ((= i revisions) np) ;;(<= (- (norm (f np)) (norm np)) rev-eps))
      (log i np))))

(define uv/grad-descent-line-log
  (lambda (f revisions p a log)
    (uv/revise-log
     (lambda (v)
       (v- v (v* a ((uv/gradient f) (to-vector v)))))
       revisions
       p
       log)))

(define uv/l2-loss
  (lambda (target-fun)
    (lambda (xs ys)
      (lambda (params)
        (let ((pred-ys ((target-fun xs) params)))
          (vsum
           (uv/sqr
            (v- pred-ys ys))))))))

(define uv/l2-loss-sample
  (lambda (target-fun)
    (lambda (xs ys)
      (lambda (params)
        (lambda (num-samples)
          (let* (;;(idx (sample-sequence num-samples (vector-length xs)))
                 (xs-ys (sample-vectors num-samples (list xs ys)));;xs-ys (extract-sub-vectors idx (list xs ys)))
                 (xs (car xs-ys))
                 (ys (cadr xs-ys)))
            (((uv/l2-loss target-fun) xs ys) params)))))))

(define uv/loss-line
  (lambda (xs ys)
    (lambda (theta)
    (((uv/l2-loss uv/line) xs ys) theta))))


(define uv/loss-line-m ;; y = mx + q, q = 0
  (lambda (xs ys)
    (lambda (m)
           ((uv/loss-line xs ys) (list m 0)))))
