
;; the clean, functional part
(defun add-widget (database widget)
    (cons widget database))

;; the dirty, nonfunctional part
(defparameter *database* nil)

(defun main-loop ()
    (loop (princ "Please enter the name of a new widget:")
          (setf *database* (add-widget *database* (read)))
          (format t "The database contains the following: ~a~%" *database*)))

(defparameter *my-list* '(4 7 2 3))

(loop for n below (length *my-list*)
    do (setf (nth n *my-list*) (+ (nth n *my-list*) 2)))

(defun add-two (list)
    (when list
        (cons (+ (car list) 2) (add-two (cdr list)))))

(defun add-two2 (list)
    (mapcar (lambda (x) (+ x 2)) list))
