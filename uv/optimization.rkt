#lang racket

;;; This module provides functions for optimization, including various
;;; implementations of gradient descent. It also includes utilities for

;;; managing hyperparameters.

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

;; A hash to store global hyperparameters for optimization algorithms.
(define hyper-parameters
  (make-hash
   (list
    '(revisions . 1000)
    '(bucket-size . 100)
    '(learning-rate . 0.01)
    '(adaptive-learning-rate . 0.1)
    '(meta-learning-rate . 1))))

;; Macro to get a hyperparameter value by its key.
;; (hyper-param 'learning-rate)
(define-syntax-rule (hyper-param k)
    (hash-ref hyper-parameters (quote k)))

;; Macro to set a hyperparameter value.
;; (hyper-param-set! 'learning-rate 0.001)
(define-syntax-rule (hyper-param-set! k v)
    (hash-set! hyper-parameters (quote k) v))

;; Macro to declare a new hyperparameter, optionally with a default value.
;; (declare-hyper 'new-param 5)
(define-syntax declare-hyper
  (syntax-rules ()
    ((declare-hyper k)
     (hyper-param-set! (quote k) null))
    ((declare-hyper k v)
     (hyper-param-set! (quote k) v))))

;; Small epsilon value to check for convergence in revision processes.
(define rev-eps 1e-8)

;; Recursively revises a parameter `p` by applying function `f` for `revisions` times,
;; or until the change in norm is smaller than `rev-eps`.
(define uv/revise
  (lambda (f revisions p)
    (if (or (zero? revisions) (<= (- (norm (f p)) (norm p)) rev-eps))
        p
        (uv/revise f (- revisions 1) (f p)))))

;; Performs gradient descent for a given function `f`.
;; `revisions`: The number of iterations.
;; `p`: The initial parameter vector.
;; `a`: The learning rate.
(define uv/grad-descent
  (lambda (f revisions p a)
    (uv/revise
     (lambda (v)
       (v- (to-vector v) (v* a ((uv/gradient f) (to-vector v)))))
       revisions
       p)))

;; Performs gradient descent with an adaptive learning rate.
;; `f`: The loss function.
;; `revisions`: The number of iterations.
;; `pa`: A list containing the initial parameters `v` and the initial learning rate `a`.
;; `u`: The meta-learning rate for adapting `a`.
(define uv/grad-descent-adaptive
  (lambda (f revisions pa u)
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

;; Creates a loss function for a linear model based on L2 loss.
;; `xs`: Input data points.
;; `ys`: True output values.
;; Returns a function that takes `theta` (line parameters) and computes the loss.
(define uv/line-loss
  (lambda (xs ys)
    (lambda (theta)
      (((uv/l2-loss uv/line) xs ys) theta))))

;; Performs gradient descent specifically for a linear model.
;; `xs`, `ys`: The training data.
;; `revisions`: Number of iterations.
;; `p`: Initial line parameters (theta).
;; `a`: Learning rate.
(define uv/grad-descent-line
  (lambda (xs ys revisions p a)
    (uv/grad-descent (uv/line-loss xs ys) revisions p a)))

;; Performs adaptive gradient descent specifically for a linear model.
(define uv/grad-descent-line-adaptive
  (lambda (xs ys revisions pa u)
    (uv/grad-descent-adaptive (uv/line-loss xs ys) revisions pa u)))

;; A version of `uv/revise` that logs the parameter `p` at each iteration.
;; `log` is a function that takes the iteration number `i` and the parameter `np`.
(define uv/revise-log
  (lambda (f revisions p (log (lambda (_ __) (null))))
    (do ((i 0 (+ i 1))
         (np p (f np)))
        ((= i revisions) np)
      (log i np))))

;; A version of gradient descent that logs the parameters at each step.
(define uv/grad-descent-line-log
  (lambda (f revisions p a log)
    (uv/revise-log
     (lambda (v)
       (v- v (v* a ((uv/gradient f) (to-vector v)))))
       revisions
       p
       log)))

;; Generic L2 loss (sum of squared differences).
;; `target-fun`: The model function (e.g., `uv/line`).
;; Returns a function that computes the L2 loss for given data `xs`, `ys` and `params`.
(define uv/l2-loss
  (lambda (target-fun)
    (lambda (xs ys)
      (lambda (params)
        (let ((pred-ys ((target-fun xs) params)))
          (vsum
           (uv/sqr
            (v- pred-ys ys))))))))

;; Calculates L2 loss on a random subsample of the data (for stochastic gradient descent).
;; `num-samples`: The size of the random subsample to use.
(define uv/l2-loss-sample
  (lambda (target-fun)
    (lambda (xs ys)
      (lambda (params)
        (lambda (num-samples)
          (let* ((xs-ys (sample-vectors num-samples (list xs ys)))
                 (xs (car xs-ys))
                 (ys (cadr xs-ys)))
            (((uv/l2-loss target-fun) xs ys) params)))))))

;; A convenience function for creating a linear model loss function.
(define uv/loss-line
  (lambda (xs ys)
    (lambda (theta)
    (((uv/l2-loss uv/line) xs ys) theta))))

;; A specialized loss function for a line of the form y = mx (intercept is 0).
(define uv/loss-line-m
  (lambda (xs ys)
    (lambda (m)
           ((uv/loss-line xs ys) (list m 0)))))
