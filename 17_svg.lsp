
(defmacro let1 (var val &body body)
    `(let ((,var ,val))
        ,@body))

(defmacro split (val yes no)
    (let1 g (gensym)
        `(let1 ,g ,val
            (if ,g
                (let ((head (car ,g))
                    (tail (cdr ,g)))
                    ,yes)
                    ,no))))

(defun pairs (lst)
    (labels ((f (lst acc)
                (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
        (f lst nil)))

(defun print-tag (name alst closingp)
    (princ #\<)
    (when closingp
        (princ #\/))
    (princ (string-downcase name))
    (mapc (lambda (att)
            (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
    (princ #\>))

(print-tag 'mytag '((color . blue) (height . 9)) nil)

(defmacro tag (name atts &body body)
    `(progn
        (print-tag ',name
                   (list ,@(mapcar (lambda (x)
                                    `(cons ',(car x) ,(cdr x)))
                                (pairs atts)))
                   nil)
        ,@body
        (print-tag ',name nil t)))

;; (defmacro tag (name atts &body body)
;;     `(progn
;;         (print-tag ',name (pairs atts) nil)
;;         ,@body
;;         (print-tag ',name nil t)))

(macroexpand '(tag mytag (color 'blue height (+ 4 5))))

(tag mytag (color 'blue height (+ 4 5)))

;; quote experiments
(defparameter foo 10)
`,foo
'`,foo
(eval '`,foo)
`,'foo
`(,@(mapcar (lambda (x) x) '(1 2 3)))

(tag mytag (color 'blue size 'big)
    (tag first-inner-tag ())
    (tag second-inner-tag ()))

(defmacro html (&body body)
    `(tag html () ,@body))

(defmacro body (&body body)
    `(tag body () ,@body))

(html
    (body
        (princ "Hello World!")))

(defmacro svg (width height &body body)
    `(tag svg (xmlns "http://www.w3.org/2000/svg"
               "xmlns:xlink" "http://www.w3.org/1999/xlink"
               height ,height
               width ,width)
          ,@body))

(defun svg-style (color)
    (format nil
        "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
        (append color
                (brightness color -100))))

(defun brightness (col amt)
    (mapcar (lambda (x)
                (min 255 (max 0 (+ x amt))))
        col))

(brightness '(255 0 0) -100)

(defun circle (center radius color)
    (tag circle (cx (car center)
                 cy (cdr center)
                 r radius
                 style (svg-style color))))

(svg 150 150
    (circle '(50 . 50) 50 '(255 0 0))
    (circle '(100 . 100) 50 '(0 0 255)))

(defmacro to-file (fname &body body)
    `(with-open-file (*standard-output*
                 ,fname
                 :direction :output
                 :if-exists :supersede)
        ,@body))

(to-file "test.svg"
    (svg 150 150
        (circle '(50 . 50) 50 '(255 0 0))
        (circle '(100 . 100) 50 '(0 0 255))))

(defun polygon (points color)
    (tag polygon (points (format nil "~{~a,~a ~}"
                            (mapcan (lambda (tp) (list (car tp) (cdr tp))) points))
                  style (svg-style color))))

(defun random-walk (value length)
    (unless (zerop length)
        (cons value
              (random-walk (if (zerop (random 2))
                               (1- value)
                               (1+ value))
                           (1- length)))))

(to-file "random_walk.svg"
    (svg 400 200
        (loop repeat 10
            do (polygon (append '((0 . 200))
                                (loop for x from 0
                                      for y in (random-walk 100 400)
                                      collect (cons x y))
                                '((400 . 200)))
                        (loop repeat 3 collect (random 256))))))
