#lang racket

;;; Linear Learning 1 (ll1.rkt)
;;; 
;;; This file demonstrates machine learning fundamentals using gradient descent
;;; applied to linear regression. It showcases how to:
;;; 1. Define training data points
;;; 2. Create loss functions for linear models
;;; 3. Apply gradient descent optimization
;;; 4. Visualize the learning process through plots and animations
;;;
;;; The code uses modules from the uv/ folder which provide:
;;; - calculus.rkt: Numerical gradient computation and line equations
;;; - optimization.rkt: Gradient descent algorithms and loss functions
;;; - plot.rkt: Visualization functions for data and models
;;; - utility.rkt: Helper functions for formatting and sampling
;;; - vector.rkt: Vector operations for numerical computations

(require "uv/calculus.rkt")      ; For gradient computation and line equations
(require "uv/plot.rkt")          ; For creating scatter plots and line plots
(require "uv/optimization.rkt")  ; For gradient descent and loss functions
(require "uv/utility.rkt")       ; For utility functions like left-pad
(require plot)                   ; Racket's built-in plotting library

;;; TRAINING DATA
;;; Define a small dataset for linear regression
;;; These represent (x, y) coordinate pairs that we want to fit a line to
(define line-xs #(2.0 1.0 4.0 3.0))  ; Input features (x-coordinates)
(define line-ys #(1.8 1.2 4.2 3.3))  ; Target values (y-coordinates)

;;; MODEL PARAMETERS (THETA)
;;; In linear regression, theta = (m, c) where y = mx + c
;;; theta[0] = slope (m), theta[1] = intercept (c)
(define theta-0 '(0 0))      ; Initial parameters: no slope, no intercept
(define theta-1 '(0.0099 0)) ; Slightly adjusted slope for comparison

;;; LOSS FUNCTION EVALUATION
;;; Demonstrate how loss changes with different parameter values
;;; uv/loss-line creates a loss function for linear regression using L2 loss
(define loss-theta-0 ((uv/loss-line line-xs expected-ys) '(0 0)))     ; Loss at theta-0
(define loss-theta-1 ((uv/loss-line line-xs expected-ys) '(0.009 0))) ; Loss at theta-1

;;; Calculate the difference in loss between the two parameter sets
;;; This demonstrates how small changes in parameters affect the loss
(define diff-theta-1-0 (- loss-theta-1 loss-theta-0))

;;; VISUALIZATION FUNCTIONS
;;; These functions create various plots to visualize the loss function
;;; and the optimization process

;;; Plot the loss function as a function of slope (m) with intercept fixed at 0
;;; This creates a 2D plot showing how loss varies with the slope parameter
(define plot-theta-0
  (lambda (xs ys)
    (plot (function (uv/loss-line-m xs ys) 0 4  ; uv/loss-line-m fixes intercept at 0
                    #:samples 50                 ; Number of points to sample
                    #:y-min -10
                    #:y-max 200
                    #:color 'red))))

;;; Function factory for creating loss function plots (currently returns quoted expression)
;;; This appears to be a template for creating plot functions
(define plot-theta-0-fun
  (lambda (xs ys color)
    (lambda()
       (function (uv/loss-line-m xs ys) 0 4
                    #:samples 50
                    #:y-min -10
                    #:y-max 200
                    #:color 'red))))

;;; Function factory for creating scatter plots of data points
(define plot-points-fun
  (lambda (xs ys color)
    (lambda()
       (points
       (vector-map vector xs ys)  ; Convert x,y vectors to list of point vectors
       #:color color))))

;;; Calculate loss values for a list of weight (parameter) combinations
;;; Returns a vector of (parameter, loss) pairs for plotting
(define loss-weights
  (lambda (xs ys ws)
    (let
      ((w (map ((uv/l2-loss uv/line) xs ys) ws))  ; Calculate loss for each weight set
       (xs (map car ws)))                          ; Extract first parameter (slope)
      (list->vector (map vector xs w)))))          ; Create vector of (slope, loss) pairs

;;; Sample weight combinations to evaluate
;;; Each pair represents (slope, intercept) parameters for the line
(define weights '((-1 0) (0 0) (1 0) (2 0) (3 0)))

;;; Plot the loss function curve with specific weight points highlighted
;;; Shows both the continuous loss function and discrete evaluation points
;;; Parameters:
;;;   xs, ys: training data
;;;   ws: list of weight combinations to highlight
;;;   c: color for the loss function curve
;;;   cw: color for the weight points
(define plot-loss-weights
  (lambda (xs ys ws c cw)
    (let ((x (car (car ws))))  ; Get the first slope value for labeling
    (plot
     (list
      ;; Plot the continuous loss function (slope vs loss, intercept=0)
      (function
       (uv/loss-line-m line-xs line-ys) -1.1 4
       #:samples 50
       #:y-min -10
       #:y-max 200
       #:color c)
      ;; Plot discrete points showing loss at specific weights
      (points (loss-weights xs ys ws) #:color cw)
      ;; Highlight a specific point with a label
      (point-label (vector x ((uv/loss-line-m line-xs line-ys) x)) ""
                   #:point-size 8 #:point-color "firebrick"))))))

;;; Save a loss function plot with a single highlighted point to a file
;;; Similar to plot-loss-weights but for a single weight value and saves to JPEG
(define plot-loss-weight-file
  (lambda (xs ys w filename)
    (plot-file
     (list
      ;; Loss function curve
      (function
       (uv/loss-line-m line-xs line-ys) -1.1 4
       #:samples 50
       #:y-min -10
       #:y-max 200
       #:color 'blue)
      ;; Single highlighted point at weight w
      (point-label (vector w ((uv/loss-line-m line-xs line-ys) w)) ""
                   #:point-size 8 #:point-color "firebrick"))
     filename
     'jpeg)))

;;; 3D LOSS SURFACE VISUALIZATION
;;; These functions create 3D plots showing the loss surface over both parameters
;;; (slope and intercept), which helps visualize the optimization landscape

;;; Create a 3D plot of the loss surface with a highlighted point
;;; Shows how loss varies with both slope (x-axis) and intercept (y-axis)
;;; The z-axis represents the loss value
(define plot-theta-3d
  (lambda (xs ys theta)
    (plot3d
     (list
      ;; Create the 3D loss surface
      (surface3d
             (lambda (x y)
               ((uv/loss-line line-xs line-ys) (vector x y)))  ; Loss function over (slope, intercept)
               0 4 -2 2                                        ; x-range: 0-4, y-range: -2 to 2
               #:z-min -10
               #:z-max 200
               ;;#:samples 100
               #:color 5
               #:line-color 5)
               ;;#:style 'solid)
      ;; Mark the current parameter point on the surface
      (point-label3d (vector (vector-ref theta 0) (vector-ref theta 1)
                             (((uv/l2-loss uv/line) line-xs line-ys)
                              theta))
                     "" #:point-size 10 #:point-color "firebrick" )
      ;; Draw a vertical line from the base to the point on the surface
      (lines3d (list (vector (vector-ref theta 0) (vector-ref theta 1) -10)
                     (vector (vector-ref theta 0) (vector-ref theta 1)
                             (((uv/l2-loss uv/line) xs ys) theta)))))
                     ;;      (((uv/l2-loss uv/line) line-xs line-ys)
                     ;;        theta)))))
     #:altitude -2   ; Camera altitude for 3D viewing
     #:angle 15)))   ; Camera angle for 3D viewing

;;; Save a 3D loss surface plot to a file
;;; Same as plot-theta-3d but saves the result as a JPEG file
(define plot-loss-3d-file
  (lambda (xs ys theta filename)
    (plot3d-file
     (list
      ;; 3D loss surface
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
      ;; Current parameter point
      (point-label3d (vector (vector-ref theta 0) (vector-ref theta 1)
                             (((uv/l2-loss uv/line) line-xs line-ys)
                              theta))
                     "" #:point-size 10 #:point-color "firebrick" )
      ;; Vertical line to the point
      (lines3d (list (vector (vector-ref theta 0) (vector-ref theta 1) -10)
                     (vector (vector-ref theta 0) (vector-ref theta 1)
                             (((uv/l2-loss uv/line) xs ys) theta)))))
     #:altitude -2
     #:angle 15
     filename
     'jpeg)))

;;; GRADIENT DESCENT IMPLEMENTATION
;;; The core gradient descent update rule: theta = theta - alpha * gradient(theta)
;;; where alpha is the learning rate and gradient(theta) is the gradient

;;; Configuration for file naming and padding
(define padding 4)  ; Number of digits for zero-padding filenames

;;; Helper function for zero-padding (appears to have a bug - recursive call without termination)
(define zero-pad
  (lambda (n)
    (zero-pad n 0 padding)))

;;; Create a loss function with one parameter fixed
;;; This allows us to visualize how loss changes with respect to one parameter
;;; while keeping the other constant
(define line-loss-theta-0
  (lambda (theta-1)
    (lambda (xs ys)
      (lambda (x)
        (((uv/l2-loss line) xs ys (list x theta-1)))))))

;;; Alternative formulation of the above function
;;; Creates a loss function for varying the first parameter (slope)
;;; while keeping the second parameter (intercept) fixed
(define line-loss-theta
  (lambda (xs ys theta1)
    (lambda(x)
      (((uv/l2-loss line) xs ys) (list x theta1)))))

;;; ANIMATION AND VISUALIZATION DURING TRAINING
;;; These functions create visualizations at each step of gradient descent
;;; to show how the algorithm converges to the optimal parameters

;;; Create a function that renders plots at each step of gradient descent
;;; This is used as a callback during the optimization process
;;; Parameters:
;;;   xs, ys: training data
;;;   file-prefix: prefix for output filenames
;;;   sample-step: how often to save plots (default: every step)
(define render-step-fun
  (lambda (xs ys file-prefix (sample-step 1))
    (lambda (i theta)
      (let ((fname (format "~a~a.jpg" file-prefix (left-pad i 0 padding))))
        (begin;;when (= (remainder i sample-step) 0)
          ;; Save scatter plot with current line fit
          (line-scatter-plot-file
           xs ys theta 'blue 'red fname (format "~a" theta))
          ;; Save 2D loss plot with current parameter highlighted
          (plot-loss-weight-file xs ys
                                 (vector-ref theta 0)
                                 (format "loss-~a~a.jpg" file-prefix
                                         (left-pad i 0 padding)))
          ;; Save 3D loss surface with current parameter highlighted
          (plot-loss-3d-file xs ys theta
                             (format "loss-3d-~a~a.jpg" file-prefix
                                     (left-pad i 0 padding))))))))

;;; HIGH-LEVEL GRADIENT DESCENT FUNCTIONS
;;; These functions combine gradient descent optimization with visualization

;;; Perform gradient descent and plot the final result
;;; Uses uv/grad-descent from optimization.rkt to find optimal parameters
;;; then visualizes the result using line-scatter-plot from plot.rkt
(define grad-descent-and-plot-line
  (lambda (xs ys descent-steps
              (init-value #(0 0))      ; Initial parameter guess
              (learning-rate 0.01)     ; Step size for gradient descent
              (filename #f))           ; Optional filename to save plot
    (let ((opt-theta
           (uv/grad-descent
            (uv/line-loss line-xs line-ys)  ; Loss function for linear regression
            descent-steps                   ; Number of optimization steps
            init-value                      ; Starting parameters
            learning-rate)))                ; Learning rate
     ;; Plot the final result: data points and fitted line
     (line-scatter-plot
      xs
      line-ys
      opt-theta
      'blue                              ; Color for data points
      'red                               ; Color for fitted line
      (format "~a" opt-theta)            ; Label showing final parameters
      filename))))                       ; Optional output file

;;; Perform gradient descent with step-by-step visualization
;;; Creates an animation showing how the algorithm converges
;;; Uses uv/grad-descent-line-log which calls a callback at each step
(define grad-descent-and-plot-anim
  (lambda (xs ys descent-steps fname
              (init-value #(0 0))      ; Initial parameter guess
              (learning-rate 0.01))    ; Learning rate
           (uv/grad-descent-line-log
            (uv/line-loss line-xs line-ys)  ; Loss function
            descent-steps                   ; Number of steps
            init-value                      ; Starting parameters
            learning-rate                   ; Learning rate
            (render-step-fun xs ys fname)))) ; Callback to save plots at each step

;;; ANIMATION UTILITIES
;;; Functions for creating animations from sequences of images

;;; Create an animated GIF from a sequence of JPEG images
;;; Uses ImageMagick's convert command to combine images
;;; Parameters:
;;;   glob: file pattern (e.g., "*.jpg") to match image files
;;;   delay: delay between frames in the animation
;;;   fname: output filename for the animated GIF
(define make-animation
  (lambda (glob delay fname) ;; glob = "*.jpg"
    (system
     (format "magick convert -delay ~a -loop 0 ~a ~a" delay glob fname))))

;;; Combine three sequences of images side-by-side
;;; Creates a composite animation showing multiple views simultaneously
;;; (e.g., data plot, 2D loss, and 3D loss surface)
;;; Parameters:
;;;   n: number of images in each sequence
;;;   prefix-1, prefix-2, prefix-3: filename prefixes for the three sequences
;;;   out-prefix: prefix for output combined images
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
        ;; Use ImageMagick to horizontally append three images
        (system (format "magick convert +append ~a ~a ~a ~a"
                      in-1 in-2 in-3 out))))))

;;; EXPERIMENTAL FUNCTIONS
;;; Additional utilities and experimental features

;;; Smoothing function for gradient descent (incomplete implementation)
;;; This appears to be the start of implementing momentum or exponential moving averages
;;; Formula: decay-rate * average + (1 - decay-rate) * gradient
(define smooth ;; decay-rate * average + (1 - decay-rate) * gradient, average = historical average
  (lambda (decay-rate average g)
    (+ (* decay-rate average))))  ; Note: missing the gradient term

;;; Generate random data points for testing
;;; Creates n points with x-coordinates from 0 to n-1
;;; and random y-coordinates between m and M
(define gen-points
  (lambda(n m M)
    (let ((xs (build-vector n (lambda (i) i)))
          (ys (build-vector n (lambda (i)
                                (+ m (random (- M m)))))))
      (vector-map vector xs ys))))

;;; CUSTOM SYNTAX EXPERIMENTS
;;; Alternative implementations of let and let* using lambda expressions
;;; These demonstrate how let forms can be implemented as syntactic sugar

;;; Custom let implementation using lambda
;;; Transforms (let ((var val) ...) body ...) into ((lambda (var ...) body ...) val ...)
(define-syntax uv-let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
      ((lambda (var ...) body ...) val ...))))

;;; Custom let* implementation using nested lambdas
;;; Transforms sequential bindings into nested lambda expressions
(define-syntax uv-let*
  (syntax-rules ()
    ((let* () body ...) ; base case
      ((lambda () body ...)))
    ((let* ((var val) rest ...) body ...) ; binding case
      ((lambda (var) (let* (rest ...) body ...)) val))))
