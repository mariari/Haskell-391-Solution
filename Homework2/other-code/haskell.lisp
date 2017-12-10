;; This would be a lot more elegant if Common Lisp was a Lisp-1!!!
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia
                  :swank
                  :let-over-lambda)))

(defpackage #:haskell-lisp
  (:nicknames hl)
  (:documentation "Abstractions Inspired from Haskell")
  (:use #:let-over-lambda)
  (:shadowing-import-from #:let-over-lambda #:when-match #:if-match)
  (:use #:swank-backend
        #:common-lisp
        #:trivia)
  (:export :join :flip
           :curry :currys
           :curryl :comp :∘
           :compose-1
           :<*> :<*>!
           :<>  :<>!  :<>!!
           :>>= :>>=! :=<< :=<<!))

(in-package :haskell-lisp)
;;; Miscellaneous Haskell Commands-------------------------------------------------------------
(defun join (lis)
  "((1) (2)) --> (1 2) removes 1 layer of a list"
  (apply #'append lis))

(defmacro flip (fn x y . rest)
  "Flips the first two arguments that is applied to a function"
  `(if (listp ',fn)                             ; check if the fn has arguments already applied to it (>>= (+))
       (macrolet ((flip-fn-list (fn x y . rest) ; need to create a local macro because we can't splice a non-list
                    `(if (null ',@rest)
                         (,@fn ,y ,x)
                         (,@fn ,y ,x ,rest))))
         (flip-fn-list ,fn ,x ,y ,rest))
       (,fn ,y ,x . ,rest)))

(defmacro curry (fn . args)
  "Creates a partially applied function that takes 1 argument if it is a macro
   (a limitation of &rest closures in CL) and multiple if it is a function"
  (if (functionp (macro-function fn))
      `(currym ,fn ,@args)
      `(curryf #',fn ,@args)))

;; Maybe use macrolet to create our lexical closure or at least get the list so we can take multiple arguments
(defmacro currym (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(lambda (,arg) (,fn ,@args ,arg))))

(declaim (ftype (function (function &rest t) function) curryf)
         (inline curryf))
(defun curryf (fn &rest args)
  "Creates a partially applied function that takes many argument"
  (lambda (&rest args2) (apply fn (append args args2))))

;; can now take variables as input!! (let ((y 2)) (currys 2 + 1 2 3))
(defmacro currys (num fn . args)
  "Creates a partially applied function that takes 1 argument"
  (if (integerp num)                   ; can't expand the environment optimally if a number isn't directly passed
      (if (functionp (macro-function fn))
          `(currym ,@(gensymbol-list (1- num) 'currym) ,fn ,@args)
          `(curryf ,@(gensymbol-list (1- num) #'curryf) #',fn ,@args))
      `(apply #'curryf (nconc (gensymbol-list (1- ,num) #'curryf)
                              (list (curry ,fn ,@args))))))

(locally (declare (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (declaim (ftype (function (fixnum function &rest t) function) curryf-num)
           (notinline curryf-num))
  (defun curryf-num (num fn &rest args)
    "contentiously curries a function until the num has been surpassed"
    (lambda (&rest args2)
      (let ((left      (- num (length args2)))
            (args-comb (append args args2)))
        (declare (type fixnum left)
                 (type list args-comb))
        (if (> left 0)
            (curryf-num left
                        (apply (curryf #'curryf fn) args-comb))
            (apply fn args-comb))))))

;;  Will correctly display the right amount for &rest but not for &optional and &keyword yet
;; arglist is also very slow (8k processor cycles!!!) so make sure to optimize this away by having it only expand in a macro!!
(defun num-args (fn)
  "Gets the number of args in a function"
  (let* ((args (arglist fn))
         (len  (length args)))
    (if (and (< 2 len) (member '&rest args))
        (- len 2)
        len)))

(defun auto-curryf (fn &rest args)
  (apply #'curryf-num (list* (num-args fn) fn args)))

;; Can't take (let ((y +)) (auto-curry y)) but can do (auto-curry +) with 10k less processor cycles than auto-curryf
(defmacro auto-curry (fn &rest args)
  (if (null args)
      `(curryf-num ,(num-args fn) #',fn)
      `(curryf-num ,(num-args fn) #',fn ,@args)))

(defmacro curryl (&rest fn-list)
  "curries a list by default 1... if you supply a number as the
     first argument it will curry the entire list by that amount"
  (flet ((struct (lambda list)
           `(list ,@(mapcar lambda list))))
    (let ((1st (car fn-list)))
      (if (numberp 1st)
          (struct (lambda (x)
                    (if (listp x)
                        `(currys ,1st ,@x)
                        `(currys ,1st ,x)))
                  (cdr fn-list))
          (struct (lambda (x)
                    (if (listp x)
                        `(curry ,@x)
                        `(curry ,x)))
                  fn-list)))))


(defmacro auto-curryl-gen (function &rest fn-list)
  "generator function for auto-curryl and auto-currylf"
  `(list ,@(mapcar (lambda (x)
                     (if (listp x)
                         `(,function ,@x)
                         `(,function ,x)))
                   fn-list)))

(defmacro auto-curryl (&rest fn-list)
  "automatically curries a list"
  `(auto-curryl-gen auto-curry ,@fn-list))

(defmacro auto-currylf (&rest fn-list)
  "automatically curries a list with the function alternative for auto-curry"
  `(auto-curryl-gen auto-curryf ,@fn-list))


(defun compose-1 (&rest fns)
  "Very much like a normal lisp compose, except that it only applies the first args
   to the far right function instead of all args, the rest of the args
   gets sent to the first function in the list (car position)"
  (if fns
      (let* ((fns (reverse fns)))
        (alambda (&rest args)
          (if args                        ; checks if any arguments were given
              (funcall (alambda (fns arg) ; do the function if args are given
                         (match fns
                           ((list f)     (apply f (cons arg (cdr args))))
                           ((list* f fs) (self fs (funcall f arg)))))
                       fns (car args))
              (curry self))))             ; else just wait for proper inputs
      #'identity))

(defun comp-1-help (fns) (apply #'compose-1 fns))

;; Note this is only temporary that we only curry the rest of the functions once
;; as soon as the :b :a system gets up, we can curry everything normally
(defmacro ∘ (&rest fns)
  "A version of compose that curries the entire argument list by 1
   then applies what's left to the last function in the list"
  `(comp-1-help (cons ,(if (listp (car fns))
                           `(auto-curry ,@(car fns))
                           `(auto-curry ,(car fns)))
                      (curryl ,@(cdr fns)))))

(defmacro comp (&rest fns)
  `(∘ ,@fns))


;; Unfinished, and very hard to verify what the behavior should be
;; After all in (∘ f g), g can take 2 arguments, and f could take a function
;; do we keep applying g?, no!, but that's the only way I think this could work!
;; (defmacro ∘% (&rest fns)
;;     "Compose that applies all the arguments as soon as it can"
;;     `(comp-help (auto-curryl ,@fns)))

;; (defmacro auto-comp% (&rest fns)
;;   "Compose that applies all the arguments as soon as it can"
;;   `(comp-help (auto-curryl ,@fns)))


;; Read/Partially Applied Functions------------------------------------------------------------
(defmacro <*> (fn-1 fn-2 arg)
  "The applicative for partially applied functions... (f x (g x))"
  (let ((x (gensym)))
    `(let ((,x ,arg))
       (,@fn-1 ,x (,@fn-2 ,x)))))

(defmacro =<< (fn-1 fn-2 arg &optional &rest extra)
  "the reverse bind for partially applied functions (f (g x) x) || (f . g) x x"
  (let ((x (gensym)))
    `(let ((,x ,arg))
       (,@fn-1 (,@fn-2 ,x) ,x ,@extra))))


(defmacro >>= (fn-1 fn-2 arg)
  "the bind for partially applied functions (f (g x) x) || (f . g) x x"
  `(=<< ,fn-2 ,fn-1 ,arg))

;; For Lists-----------------------------------------------------------------------------------

;; call wtih (<*>! (list (curry + 2) (curry - 3)) '(1 2 3 ))
(defun <*>! (lis-fn lis)
  "The applicative for lists "
  (>>=! lis-fn (lambda (fn) (mapcar fn lis))))

(defun =<<! (fn lis)
  "The Reverse Bind Monad for Lists "
  (mapcan fn lis))

(defun >>=! (lis fn)
  "The Bind Monad for lists "
  (mapcan fn lis))

;; Rerwrite with fast-apply later

;; The Monoid ---------------------------------------------------------------------------------

(defmacro <> (&body str)
  "The monoid for Strings "
  `(concatenate 'string ,@str))

(defun <>! (&rest lis)
  "The monoid for lists"
  (apply #'append lis))

(defmacro <>!! (&body vec)
  "The monoid for Vectors "
  `(concatenate 'vector ,@vec))

;; Helper functions----------------------------------------------------------------------------

(defun gensymbol-list (num word)
  (loop for i from 1 to num
     collect word))

(defun range (max &optional (min 0) (step 1))
  (loop for x from min to max by step
     collect x))

(defmacro fn-print (fn)
  `(progn (print ',fn)
          (print ,fn)
          (print "#######")
          ,fn))

(defmacro map-fn-print (&rest fns)
  `(list ,@(mapcar (lambda (x) `(fn-print ,x)) fns)))

(defmacro alias (new current)
  `(setf (fdefinition ',new) #',current))


;;; Fun Testing--------------------------------------------------------------------------------
;;; General functions--------------------------------------------
;; FLIP IS BROKEN IN RACKET!!!
;; (flip (<*> (* 2)) 3 (-))
;; (flip + 2 3)

(defun test-general ()
  (map-fn-print
   ;; (flip (<*> (* 2)) 3 (-))
   (funcall (curry + 1 2 3) 3)
   (mapcar (curry expt 2) '(1 2 3 4))
   (funcall (funcall (comp + (- 3)) 1) 2)  ; = 4
   (funcall (∘ + (- 3)) 1 2)            ; = 4
   (funcall (comp mapcar (curry +) - ) 2 '(1 2 3))
   ;; (funcall (funcall (apply 'compose (curryl (curry + 1 2 3) (- 2 3))) 3) 2)
   ;; (curryl 2 (+ 1 2 3) (- 2 3 4))
   ))
;;; Lists--------------------------------------------------------
(defun test-list ()
  (map-fn-print
   (<*>! (list (curry + 2) (curry - 3)) '(1 2 3))
   (>>=! '((1 2 3) (2 3 4)) (curry <*>! (list (curry + 2) (curry + 3))))))

(defun test-reader ()
  (map-fn-print 
   (<*> (+ 3) (/ 2) 3)
   (mapcar (curry <*> (+ 3) (/ 2)) '(2 3 4))
   (=<< (+ 3) (/ 2) 3)
   (>>= (+ 3) (/ 2) 3)))
;; Read/Partially Applied Functions------------------------------


;;; Planned features---------------------------------------------------------------------------

;;  We will eventually want a macro that will put funcalls as
;; long as we have arguments or until the closure is finished

;; Macro that adds implicit currying... then add it to the various definitions we have here

;; Add syntax for auto currying based on a default

;; use ?a or c:a instead of :a
;; ([+ 1 2 3 :apply 2] 2)
;; ([+ 1 2 3 :a 2] 2)
;; ([+ 1 2 3 :flip-apply 2] 2)
;; ([+ 1 2 3 :b 2] 2)
