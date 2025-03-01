(define line-xs #(2.0 1.0 4.0 3.0))
(define line-ys #(1.8 1.2 4.2 3.3))


;; t0 = t0 - learning-rate * rate-of-change

(define line-loss-theta-0
  (lambda (theta-1)
    (lambda (xs ys)
      (lambda (x)
        (((uv-l2-loss line) xs ys (list x theta-1)))))))

(define line-loss-theta
  (lambda (xs ys theta1)
    (lambda(x)
      (((uv-l2-loss line) xs ys) (list x theta1)))))

(require plot)
(define plot-loss-theta-0
  (lambda (xs ys theta-1 xmin xmax ymin ymax)
    (plot
     (function ((line-loss-theta theta-1) xs ys) xmin xmax
               #:x-min xmin #:x-max xmax #:y-min ymin #:y-max ymax)
