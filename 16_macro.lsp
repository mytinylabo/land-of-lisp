
(defmacro let1 (var val &body body)
    `(let ((,var ,val))
        ,@body))

(let1 foo (+ 2 3)
    (* foo foo))

(defun add (a b)
    (let1 x (+ a b)
        (format t "The sum is ~a" x)
        x))

(macroexpand '(let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))

(defun my-length (lst)
    (labels ((f (lst acc)
                (if lst
                    (f (cdr lst) (1+ acc))
                    acc)))
        (f lst 0)))

;; has a bug
(defmacro split (val yes no)
    `(if ,val
        (let ((head (car ,val))
              (tail (cdr ,val)))
            ,yes)
            ,no))

(split '(1 2 3)
    (format t "~a + ~a" head tail)
    (princ "empty"))

(defun my-length2 (lst)
    (labels ((f (lst acc)
                (split lst
                    (f (cdr lst) (1+ acc))
                    acc)))
        (f lst 0)))

;; princ is run multiple times
(split (progn (princ "Lisp rocks!") '(2 3))
    (format t "This can be split into ~a and ~a." head tail)
    (format t "This cannot be split."))

;; yet another bug
(defmacro split (val yes no)
    `(let1 x ,val
        (if x
            (let ((head (car x))
                  (tail (cdr x)))
                ,yes)
                ,no)))

;; *** - +: (2 3) is not a number
(let1 x 100
    (split '(2 3)
        (+ x head)
        nil))

(macroexpand '(split '(2 3)
                (+ x head)
                nil))

(LET ((X '(2 3))) ;; overwrites X if it is defined outside the macro
    (IF X
        (LET ((HEAD (CAR X))
              (TAIL (CDR X)))
            (+ X HEAD))
        NIL))

;; perfect
(defmacro split (val yes no)
    (let1 g (gensym)
        `(let1 ,g ,val
            (if ,g
                (let ((head (car ,g))
                    (tail (cdr ,g)))
                    ,yes)
                    ,no))))

(macroexpand '(split '(2 3)
                (+ x head)
                nil))

(defun pairs (lst)
    (labels ((f (lst acc)
                (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
        (f lst nil)))

(defmacro recurse (vars &body body)
    (let1 p (pairs vars)
        `(labels ((self ,(mapcar #'car p)
                    ,@body))
            (self ,@(mapcar #'cdr p)))))

(defun my-length (lst)
    (recurse (lst lst
              acc 0)
        (split lst
            (self tail (1+ acc))
            acc)))
