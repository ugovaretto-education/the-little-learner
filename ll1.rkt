(require "uv/calculus.rkt")
(require "uv/plot.rkt")
(require plot)

;; Points
(define line-xs #(2.0 1.0 4.0 3.0))
(define line-ys #(1.8 1.2 4.2 3.3))

(define theta-0 '(0 0))
(define theta-1 '(0.0099 0))


(define loss-theta-0 ((uv/loss-line line-xs line-ys) '(0 0)))
(define loss-theta-1 ((uv/loss-line line-xs line-ys) '(0.009 0)))

(define diff-theta-1-0 (- loss-theta-1 loss-theta-0))

(define plot-theta-0
  (lambda (xs ys)
    (plot (function (uv/loss-line-m xs ys) 0 4
                    #:samples 50
                    #:y-min -10
                    #:y-max 200
                    #:color 'red))))

(define plot-theta-0-fun
  (lambda (xs ys color)
    (lambda()
      '(function (uv/loss-line-m xs ys) 0 4
                    #:samples 50
                    #:y-min -10
                    #:y-max 200
                    #:color 'red))))

(define plot-points-fun
  (lambda (xs ys color)
    (lambda()
      '(points
       (vector-map vector xs ys)
       #:color color))))

(define loss-weights
  (lambda (xs ys ws)
    (let
      ((w (map ((uv/l2-loss uv/line) xs ys) ws))
       (x (map car ws)))
      (list->vector (map vector x w)))))

(define weights '((-1 0) (0 0) (1 0) (2 0) (3 0)))

(define plot-loss-weights
  (lambda (xs ys ws c cw)
    (plot
     (list
      (function
       (uv/loss-line-m line-xs line-ys) -1.1 4
       #:samples 50
       #:y-min -10
       #:y-max 200
       #:color c)
      (points (loss-weights xs ys ws) #:color cw)))))

(define plot-theta-3d
  (lambda (xs ys)
    (plot3d (surface3d
             (lambda (x y)
               ((uv/loss-line line-xs line-ys) (list x y)))
               0 4 -2 2
               #:z-min -10
               #:z-max 200
               #:samples 100
               #:style 'solid)
            #:altitude 10)))

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
