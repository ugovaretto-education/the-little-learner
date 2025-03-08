#lang racket

(provide uv/revise
         uv/grad-descent
         uv/line-loss
         uv/grad-descent-adaptive
         uv/grad-descent-line
         uv/grad-descent-line-log
         uv/l2-loss
         uv/l2-loss-sample
         uv/loss-line
         uv/loss-line-m
         declare-hyper
         hyper-param
         hyper-param-set!)

(require "calculus.rkt")
(require "vector.rkt")
(require "utility.rkt")

(define hyper-parameters
  (make-hash
   (list
    '(revisions . 1000)
    '(bucket-size . 100)
    '(learning-rate . 0.01)
    '(adaptive-learning-rate . 0.1)
    '(learning-rate-rate . 1))))

(define-syntax-rule (hyper-param k)
    (hash-ref hyper-parameters (quote k)))

(define-syntax-rule (hyper-param-set! k v)
    (hash-set! hyper-parameters (quote k) v))

(define-syntax declare-hyper
  (syntax-rules ()
    ((declare-hyper k)
     (hyper-param-set! (quote k) null))
    ((declare-hyper k v)
     (hyper-param-set! (quote k) v))))

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
       (v- (to-vector v) (v* a ((uv/gradient f) (to-vector v)))))
       revisions
       p)))

;; update the learning rate by an amount proportional to the speed of change
;; of the parameter vector (e.g. theta for a line)
;; the speed is the difference in values of the previous vector and the updated one
;; and the difference between learning rates i.e. the derivative at the current learning rate point.
(define uv/grad-descent-adaptive
  (lambda (f revisions pa u) ;; pa includes both the theta parameter and the learning-rate
    (uv/revise
     (lambda (pa)
       ((let* ((v (car pa)) ;; loss function parameters (e.g. theta for line)
               (a (cadr pa)) ;; learning rate
               (g (lambda (x) ;; updated loss function parameters
                    (v- (to-vector v) (v* x ((uv/gradient f) (to-vector v))))))
               (loss-gradient (g v))
               (ll (lambda (x) (norm (v- v (g x)))))) ;; difference between previous vector and updated one
          (list
           loss-gradient
           (- a (* u ((uv/gradient ll) (to-vector a)))))
          revisions
          pa))))))

(define uv/line-loss
  (lambda (xs ys)
    (lambda (theta)
      (((uv/l2-loss uv/line) xs ys) theta))))

(define uv/grad-descent-line
  (lambda (xs ys revisions p a)
    (uv/grad-descent (uv/line-loss xs ys) revisions p a)))

(define uv/grad-descent-line-adaptive
  (lambda (xs ys revisions pa u)
    (uv/grad-descent-adaptive (uv/line-loss xs ys) revisions pa u)))

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
