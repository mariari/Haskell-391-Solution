(load "./other-code/haskell.lisp")
(load "./other-code/functions.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia :let-over-lambda :group-by :eazy-gnuplot cl-csv))
  (use-package :eazy-gnuplot)
  (use-package 'haskell-lisp)
  (use-package 'trivia))


;; SETUP================================================================================================================
(ensure-directories-exist "images/")

;; [[chr]]
(defparameter *csv* (cl-csv:read-csv #P"irisdata.csv"))

(defstruct plant
  (species "" :type String)
  (sepal-length 0 :type number)
  (sepal-width 0 :type number)
  (petal-length 0 :type number)
  (petal-width 0 :type number))

(defun create-plant (sepal-length sepal-width petal-length petal-width species)
  (make-plant :sepal-length sepal-length
              :sepal-width sepal-width
              :petal-length petal-length
              :petal-width petal-width
              :species species))

;; PART 1===============================================================================================================
;; chr -> (∨ chr ℝ)
(defun digit-char-to-num (chr)
  (if (digit-char-p (char chr 0))
      (read-from-string chr)
      chr))

;; [[(string,string,string,string)]] -> [[Plants]]
(defun type-data (xss)
  (mapcar (compose-1 (curry apply #'create-plant)
                     (curry mapcar #'digit-char-to-num))
          xss))

;; [[plant]] -> [[plant]]
(defun group-data (xss)
  (group-by:group-by xss :value #'identity :key #'plant-species))


(defun corrected (csv)
  "groups the csv data and fixes the type"
  (funcall (comp group-data type-data cdr) csv))

(defun corrected-no-group-filter (csv str)
  "fixes the types of the csv data"
  (funcall (compose-1 (curry remove-if (compose-1 (curry string= str) #'plant-species))
                      #'type-data #'cdr)
           csv))

(defun corrected-no-group-or-setsona (csv)
  "fixes the types of the csv data"
  (corrected-no-group-filter csv "setosa"))

(defun linear-decision (csv string)
  "returns a vector with the petal length and width and another petal based on the class"
  (flet ((class (str)
           (if (string= string str) 1 -1)))
    (list (mapcar (lambda (x) (list 1 (plant-petal-length x) (plant-petal-width x))) csv)
          (mapcar (compose-1 #'class #'plant-species) csv))))

;; max is the maximum number of steps one wants to go
(defun decision-boundary (data-vec class-vec &optional (max 20000))
  (let* ((num-samples (length data-vec))
         (zipped      (zip data-vec class-vec))
         (δ           1e-10))
    (labels ((rec (iter vector)
               (if (> iter max)
                   vector
                   (let ((new-vec (reduce (lambda (ys x)
                                            (let* ((class  (cadr x))
                                                   (vector (car x))
                                                   (dotted (dot-prduct ys vector)))
                                              (if (= class (signum dotted))
                                                  ys ; don't update
                                                  (let* ((normed (expt (norm vector) 2))
                                                         (η      (/ (- class dotted) normed)))
                                                    (mapcar (lambda (a y) (+ y (* a η))) vector ys)))))
                                          zipped :initial-value vector)))
                     (if (< (* (/ 1 num-samples) (abs (apply #'+ (mapcar #'- vector new-vec)))) δ)
                         vector
                         (rec (1+ iter) new-vec))))))
      (rec 0 (list 0 0 0)))))


(flet ((graph-1-clos (str &optional x)
         (graph-1 str (cdr (corrected *csv*)) x)))
  (defun answer-1.a ()
    (graph-1-clos "images/1-a.png"))

  (defun answer-1.b ()
    (flet ((draw-boundary (x) (* .9 (+ -.3 (abs (- 7 x))))))
      (graph-1-clos "images/1-b.png"
                    (lambda ()
                      (mapcar (lambda (x) (format t "~&~a ~a" x (draw-boundary x)))
                              (f:range 3.5 7 .5))))))
  (defun answer-1.c ()
    (flet ((decision (filter class-group)
             (apply #'decision-boundary
                    (append (linear-decision (corrected-no-group-filter *csv* filter) class-group) '(188888))))
           (calced (data-vec)
             (lambda ()
               (mapcar (lambda (x)
                         (format t "~&~a ~a" x (solve-at-x data-vec x))
                         (list x (solve-at-x data-vec x)))
                       (f:range 0 7 .1)))))
      (let ((new-csv (corrected *csv*)))
        (graph-1 "images/1-c.png"   (cdr new-csv)                        (calced (decision "setsona" "virginica")))
        (graph-1 "images/1-c-1.png" (list (car new-csv) (cadr new-csv))  (calced (decision "virginica"  "versicolor")))
        (graph-1 "images/1-c-2.png" (list (car new-csv) (caddr new-csv)) (calced (decision "versicolor" "virginica")))))))

;; PART2================================================================================================================

(defun data-class-vectors (csv)
  (linear-decision (corrected-no-group-or-setsona csv) "virginica"))

(defun solve-at-x (data-vec x)
  (match data-vec
    ((list a xscalor yscalor) (/ (+ (- a) (* x (- xscalor))) yscalor))
    (_                        (error "error, send a valid input vector"))))

(flet ((new-class-val (class)
         (if (= -1 class) 0 1)))

  (defun mean-square-error (boundary-vec data-vec class-vec)
    (/ (apply #'+
              (mapcar (lambda (location class)
                        (expt (- (dot-prduct location boundary-vec)
                                 (new-class-val class))
                              2))
                      data-vec class-vec))
       2))

  (defun gradient-square (boundary-vec data-vec class-vec)
    (apply (curry mapcar #'+)
           (mapcar (lambda (location class)
                     (mapcar (curry *  (- (dot-prduct location boundary-vec)
                                          (new-class-val class)))
                             location))
                   data-vec class-vec))))

(defun gradient-step (boundary-vec data-vec class-vec &optional (ε 1e-4))
  (mapcar #'- boundary-vec
              (mapcar (curry * ε)
                      (gradient-square boundary-vec data-vec class-vec))))

(defun answer-2.a (data-vec class-vec boundary-vec)
  (mean-square-error boundary-vec data-vec class-vec))

;; 2 test
(defparameter *small-error* '(-30.16284 4.6719003 4.854288))
(defparameter *large-error* '(-17.7 1 13))

(let ((val (data-class-vectors *csv*)))
  (defun answer-2.b ()
    (flet ((calced (error-set)
             (mean-square-error error-set (car val) (cadr val))))
      (graph-2 "images/2-b.png" (cdr (corrected *csv*))
               *small-error* (calced *small-error*)
               *large-error* (calced *large-error*))))

  (defun answer-2.e ()
    (flet ((descend (vector)
             (gradient-step vector (car val) (cadr val))))
      (graph-gen "images/2-e-large.png" (cdr (corrected *csv*)) (plot-this 1 *large-error*
                                                                         (descend *large-error*)
                                                                         (descend (descend *large-error*))))
      (graph-gen "images/2-e-small.png" (cdr (corrected *csv*)) (plot-this 1 *small-error*
                                                                         (descend *small-error*)
                                                                         (descend (descend *small-error*)))))))

;; PART3================================================================================================================
(defun gradient-descent (boundary-vec data-vec class-vec &optional (max-iter 5000) (δ 1e-3))
  (labels ((rec (boundary iter)
             (let ((stepped (gradient-step boundary data-vec class-vec)))
               (if (or (< (norm-2 stepped boundary) δ) (>= iter max-iter))
                   boundary
                   (rec stepped (1+ iter))))))
    (rec boundary-vec 0)))

(defun answer-3.b ()
  (let ((val (data-class-vectors *csv*)))
    ;; rather inefficient, could just pass the gradient at every point with a foldr!
    (let ((gradients (mapcar (curry gradient-descent *large-error* (car val) (cadr val))
                             (remove-if-not #'evenp
                                            (f:range 0 10))))
          (all-gradients (mapcar (curry gradient-descent *large-error* (car val) (cadr val))
                                 (f:range 0 120))))
      (graph-gen "images/3-b-dots.png"
                 (cdr (corrected *csv*))
                 (eval `(plot-this 2 ,@(mapcar (lambda (x) (cons 'list x)) gradients))))
      (with-plots (*standard-output* :debug nil)
        (gp-setup :terminal '(pngcairo :size |35cm,20cm|)
                  :output "images/3-b-errors.png"
                  :xlabel "number of iterations"
                  :ylabel "mean squared error"
                  :key '(:left))
        (plot (lambda ()
                (mapcar (lambda (x vals)
                          (let ((err-val (mean-square-error vals (car val) (cadr val))))
                            (format t "~&~a ~a" x err-val)
                            (list x err-val)))
                        (f:range 0 120)
                        all-gradients))
              :with '(:line)
              :title "The descent curve")))))

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun make-random-boundary ()
  (list (- (random-from-range 24 35))
        (- (random 8.0) 1)
        (- (random 10.0) 2)))

(defun answer-3.c ()
  (flet ((descend (boundary num)
           (gradient-descent boundary (car (data-class-vectors *csv*)) (cadr (data-class-vectors *csv*)) num)))
    (let ((boundary (make-random-boundary)))
      (print (descend boundary 20))
      (graph-gen "images/3-c-random.png"
          (cdr (corrected *csv*))
        (plot-this 5 boundary
                 (descend boundary 5)
                 (descend boundary 10))))))


;; Helper functions=====================================================================================================
(defun dot-prduct (xs ys)
  (apply #'+ (mapcar #'* xs ys)))

(defun norm-2 (xs ys)
  (sqrt (apply #'+ (mapcar (lambda (x y) (expt (- x y) 2)) xs ys))))

(defun norm (xs)
  (sqrt (apply #'+ (mapcar (lambda (x) (expt x 2)) xs))))

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

;; not my code
(defun shuffle (input-list &optional accumulator)
  "Shuffle a list using tail call recursion."
  (if (eq input-list nil)
      accumulator
      (progn
    (rotatef (car input-list)
         (nth (random (length input-list)) input-list))
    (shuffle (cdr input-list)
             (append accumulator (list (car input-list)))))))
;; Graphing functions===================================================================================================
(defmacro graph-gen (output xss &body rest)
  `(with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(pngcairo :size |35cm,20cm|)
              :output ,output
              :yrange :|[0:3]|
              :xlabel "petal-length"
              :ylabel "petal-width"
              :key '(:left))
    (flet ((graph (xs name)
             (plot (lambda ()
                     (mapcar (lambda (x)
                               (format t "~&~a ~a" (plant-petal-length x) (plant-petal-width x)))
                             xs))
                   :title name)))
      (mapcar (lambda (xs) (graph (cdr xs) (car xs))) ,xss)
      ,@rest)))

(defun graph-1 (output xss &optional extra-graphing)
  (graph-gen output
             xss
            (when extra-graphing
              (plot extra-graphing
                    :with '(:line)
                    :title ""))))

(defun compute-at-x-for-graph (vec &optional (range (f:range 2.5 7 .1)))
  (lambda () (mapcar (lambda (x) (format t "~&~a ~a" x (solve-at-x vec x))) range)))

(defmacro plot-this (scalor &rest list-values)
  `(list
    ,@(mapcar (lambda (x num)
                `(plot (compute-at-x-for-graph ,x)
                       :with '(:line)
                       :title (concatenate 'string
                                           "Descent #"
                                           (princ-to-string ,(* scalor num))
                                           " error value "
                                           (princ-to-string (mean-square-error ,x (car (data-class-vectors *csv*))
                                                                                (cadr (data-class-vectors *csv*)))))))
              list-values
              (f:range 0 100))))

(defun graph-2 (output xss small small-value large large-value)
  (graph-gen output
             xss
    (plot (compute-at-x-for-graph small)
          :with '(:line)
          :title (concatenate 'string  "error of " (princ-to-string small-value)))
    (plot (compute-at-x-for-graph large)
          :with '(:line)
          :title (concatenate 'string  "error of " (princ-to-string large-value)))))

;; extra data that didn't make the cut==================================================================================

;; (defparameter *generator-for-small-error*
;;   (apply #'decision-boundary
;;          '(((1 5 1.5) (1 6.7 2.2) (1 4.4 1.4) (1 3.5 1) (1 3.9 1.2) (1 4.6 1.4)
;;             (1 6.1 1.9) (1 5.6 1.8) (1 4.8 1.8) (1 5.8 2.2) (1 5.5 1.8) (1 4.9 1.5)
;;             (1 6.6 2.1) (1 4.5 1.6) (1 4.8 1.8) (1 5 1.7) (1 4.4 1.4) (1 4.3 1.3)
;;             (1 5.6 2.1) (1 3.3 1) (1 4.2 1.5) (1 4.5 1.5) (1 3.9 1.1) (1 4.2 1.3)
;;             (1 5.5 1.8) (1 5.1 1.9) (1 4.6 1.5) (1 5.1 1.5) (1 6.4 2) (1 4.4 1.2)
;;             (1 4.5 1.5) (1 5.7 2.3) (1 4.8 1.8) (1 6 1.8) (1 4.9 1.8) (1 5.1 1.9)
;;             (1 5.1 1.6) (1 4 1.3) (1 5.6 2.4) (1 4.5 1.5) (1 5.5 2.1) (1 5.7 2.1)
;;             (1 3.3 1) (1 4.8 1.4) (1 3.8 1.1) (1 4.9 1.5) (1 5.9 2.1) (1 5 1.9)
;;             (1 4.7 1.5) (1 5.8 1.8) (1 5.1 2.3) (1 4.5 1.7) (1 6.9 2.3) (1 5.6 2.2)
;;             (1 5.1 2) (1 5.3 2.3) (1 4.9 2) (1 5.1 2.4) (1 5.8 1.6) (1 4.5 1.5)
;;             (1 4.3 1.3) (1 6 2.5) (1 3.6 1.3) (1 3.5 1) (1 5.4 2.3) (1 4.7 1.4)
;;             (1 4.6 1.3) (1 4.5 1.5) (1 6.1 2.3) (1 4.7 1.4) (1 6.7 2) (1 3.7 1)
;;             (1 4.1 1.3) (1 4 1) (1 4.7 1.6) (1 4.1 1) (1 4.2 1.3) (1 5.3 1.9) (1 4.4 1.3)
;;             (1 4 1.3) (1 4.2 1.2) (1 4.9 1.8) (1 5.4 2.1) (1 5 2) (1 5.9 2.3) (1 5.6 1.4)
;;             (1 4 1.2) (1 4 1.3) (1 5.2 2) (1 6.3 1.8) (1 5.2 2.3) (1 5.6 2.4) (1 3.9 1.4)
;;             (1 4.1 1.3) (1 4.7 1.2) (1 5.7 2.5) (1 4.5 1.3) (1 6.1 2.5) (1 3 1.1)
;;             (1 5.1 1.8))
;;            (1 1 -1 -1 -1 -1 1 1 1 1 1 -1 1 -1 -1 -1 -1 -1 1 -1 -1 -1 -1 -1 1 1 -1 1 1 -1
;;             -1 1 1 1 1 1 -1 -1 1 -1 1 1 -1 -1 -1 -1 1 1 -1 1 1 1 1 1 1 1 1 1 1 -1 -1 1 -1
;;             -1 1 -1 -1 -1 1 -1 1 -1 -1 -1 -1 -1 -1 1 -1 -1 -1 1 1 1 1 1 -1 -1 1 1 1 1 -1
;;             -1 -1 1 -1 1 -1 1))))

;; boundary-vec = [θ, αx, βy,...]
;; (θ - α)/β is negative in slope ⇔ sign(-α) ≠ sign(β)
;; [ℝ] -> [ℝ] -> fixnum -> Βoolean
(defun mean-square-error%% (boundary-vec data-vec class-vec)
  (sqrt
   (/ (apply #'+
       (mapcar (lambda (location class)
                 (if (in-correct-class-p boundary-vec location class)
                     0
                     (expt (norm-2 location (list (solve-at-x boundary-vec (car  location))
                                                  (solve-at-y boundary-vec (cadr location)))) 2)))
               data-vec class-vec)) ; else just use the l_2 norm squared, which is just (x-x_l)^2 + (y-y_l)^2
      (- (length data-vec) 2))))

(defun in-correct-class-p (boundary-vec location class &optional (calc #'solve-at-x))
  (match location
    ((list _ x-cord y-cord)
     (let* ((y-boundary (funcall calc boundary-vec x-cord))
            (slope-neg? (zerop (- (signum (cadr boundary-vec))
                                  (signum (caddr boundary-vec))))))
       (= class
          (if (or (and (< y-cord y-boundary) slope-neg?)
                 (and (> y-cord y-boundary) (not slope-neg?)))
              -1 1))))))

(defun solve-at-y (data-vec y)
  (let-match1 (list a x1 y2) data-vec
    (solve-at-x (list a y2 x1) y)))
