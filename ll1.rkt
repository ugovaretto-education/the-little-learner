#lang racket

(require "uv/calculus.rkt")
(require "uv/plot.rkt")
(require "uv/optimization.rkt")
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
    (let ((x (car (car ws))))
    (plot
     (list
      (function
       (uv/loss-line-m line-xs line-ys) -1.1 4
       #:samples 50
       #:y-min -10
       #:y-max 200
       #:color c)
      (points (loss-weights xs ys ws) #:color cw)
      (point-label (vector x ((uv/loss-line-m line-xs line-ys) x)) ""
                   #:point-size 8 #:point-color "firebrick"))))))

(define plot-loss-weight-file
  (lambda (xs ys w filename)
    (plot-file
     (list
      (function
       (uv/loss-line-m line-xs line-ys) -1.1 4
       #:samples 50
       #:y-min -10
       #:y-max 200
       #:color 'blue)
      (point-label (vector w ((uv/loss-line-m line-xs line-ys) w)) ""
                   #:point-size 8 #:point-color "firebrick"))
     filename
     'jpeg)))



(define plot-theta-3d
  (lambda (xs ys theta)
    (plot3d
     (list
      (surface3d
             (lambda (x y)
               ((uv/loss-line line-xs line-ys) (vector x y)))
               0 4 -2 2
               #:z-min -10
               #:z-max 200
               ;;#:samples 100
               #:color 5
               #:line-color 5)
               ;;#:style 'solid)
      (point-label3d (vector (vector-ref theta 0) (vector-ref theta 1)
                             (((uv/l2-loss uv/line) line-xs line-ys)
                              theta))
                     "" #:point-size 10 #:point-color "firebrick" )
      (lines3d (list (vector (vector-ref theta 0) (vector-ref theta 1) -10)
                     (vector (vector-ref theta 0) (vector-ref theta 1)
                             (((uv/l2-loss uv/line) xs ys) theta)))))
                     ;;      (((uv/l2-loss uv/line) line-xs line-ys)
                     ;;        theta)))))
     #:altitude -2
     #:angle 15)))

(define plot-loss-3d-file
  (lambda (xs ys theta filename)
    (plot3d-file
     (list
      (surface3d
             (lambda (x y)
               ((uv/loss-line line-xs line-ys) (vector x y)))
               0 4 -2 2
               #:z-min -10
               #:z-max 200
               ;;#:samples 100
               #:color 5
               #:line-color 5)
               ;;#:style 'solid)
      (point-label3d (vector (vector-ref theta 0) (vector-ref theta 1)
                             (((uv/l2-loss uv/line) line-xs line-ys)
                              theta))
                     "" #:point-size 10 #:point-color "firebrick" )
      (lines3d (list (vector (vector-ref theta 0) (vector-ref theta 1) -10)
                     (vector (vector-ref theta 0) (vector-ref theta 1)
                             (((uv/l2-loss uv/line) xs ys) theta)))))
     #:altitude -2
     #:angle 15
     filename
     'jpeg)))


;; t0 = t0 - learning-rate * rate-of-change

(define padding 4)

(define zero-pad
  (lambda (n)
    (left-pad n 0 padding)))

(define line-loss-theta-0
  (lambda (theta-1)
    (lambda (xs ys)
      (lambda (x)
        (((uv-l2-loss line) xs ys (list x theta-1)))))))

(define line-loss-theta
  (lambda (xs ys theta1)
    (lambda(x)
      (((uv-l2-loss line) xs ys) (list x theta1)))))

(define render-step-fun
  (lambda (xs ys file-prefix (sample-step 1))
    (lambda (i theta)
      (let ((fname (format "~a~a.jpg" file-prefix (left-pad i 0 padding)))
        (begin;;when (= (remainder i sample-step) 0)
          (line-scatter-plot-file
           xs ys theta 'blue 'red fname (format "~a" theta))
          (plot-loss-weight-file xs ys
                                 (vector-ref theta 0)
                                 (format "loss-~a~a.jpg" file-prefix
                                         (left-pad i 0 padding)))
          (plot-loss-3d-file xs ys theta
                             (format "loss-3d-~a~a.jpg" file-prefix
                                     (left-pad i 0 padding)))))))))

(define grad-descent-and-plot-line
  (lambda (xs ys descent-steps
              (init-value #(0 0))
              (learning-rate 0.01)
              (filename #f))
    (let ((opt-theta
           (uv/grad-descent
            (uv/line-loss line-xs line-ys)
            descent-steps
            init-value
            learning-rate)))
     (line-scatter-plot
      xs
      line-ys
      opt-theta
      'blue
      'red
      (format "~a" opt-theta)
      filename))))

(define grad-descent-and-plot-anim
  (lambda (xs ys descent-steps fname
              (init-value #(0 0))
              (learning-rate 0.01))
           (uv/grad-descent-line-log
            (uv/line-loss line-xs line-ys)
            descent-steps
            init-value
            learning-rate
            (render-step-fun xs ys fname))))

(define make-animation
  (lambda (glob delay fname) ;; glob = "*.jpg"
    (system
     (format "magick convert -delay ~a -loop 0 ~a ~a" delay glob fname))))

(define stich-images
  (lambda (n prefix-1 prefix-2 prefix-3 out-prefix)
    (do ((i 0 (+ 1 i))
         (out (format "~a0.jpg" out-prefix)
              (format "~a~a.jpg" out-prefix i))
         (in-1 (format "~a0.jpg" prefix-1)
               (format "~a~a.jpg" prefix-1 i))
         (in-2 (format "~a0.jpg" prefix-2)
               (format "~a~a.jpg" prefix-2 i))
         (in-3 (format "~a0.jpg" prefix-3)
               (format "~a~a.jpg" prefix-3 i)))
        ((= i n))
      (begin
        ;;(displayln (format "~a ~a ~a ~a" i in-1 in-2 out))
        (system (format "magick convert +append ~a ~a ~a ~a"
                      in-1 in-2 in-3 out))))))
;;

(define-syntax-rule (declare-hyper x)
  (define x null))

;; like let macro
(define-syntax with-hyper
  (syntax-rules ()
    ((with-hyper ((var val) ...) body ...)
     (begin
       (set! var val) ...
       body ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax uv-let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
      ((lambda (var ...) body ...) val ...))))

(define-syntax uv-let*
  (syntax-rules ()
    ((let* () body ...) ; base case
      ((lambda () body ...)))
    ((let* ((var val) rest ...) body ...) ; binding case
      ((lambda (var) (let* (rest ...) body ...)) val))))
