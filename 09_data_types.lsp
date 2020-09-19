(defparameter x (make-array 3))
(aref x 1) ;; NIL
(setf (aref x 1) 'foo)
(aref x 1) ;; FOO
;; 上の例では getter である (aref x 1) をそのまま setf に渡すことで
;; setter を構成できることに注目する

;; リストでも同じことができる
(defparameter foo '(a b c))
(second foo) ;; B
(setf (second foo) 'z)
(second foo) ;; Z

;; ハッシュテーブルでも同じことができる
(defparameter bar (make-hash-table))
(gethash 'baz bar) ;; NIL
(setf (gethash 'baz bar) 5)
bar ;; #S(HASH-TABLE :TEST FASTHASH-EQL (BAZ . 5))

;; 構造体
(defstruct person
           name
           age
           waist-size
           favorite-color)

(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))

*bob*
#S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")

(person-age *bob*)
(setf (person-age *bob*) 36)

;; シーケンスを扱う
(length '(a b c))
(length "bulb")
(length (make-array 5))

(find-if #'numberp '(a b 5 d))
(count #\s "mississippi")
(position #\4 "2kewl4skewl4")
(some #'numberp '(a b 5 d))
(every #'numberp '(a b 5 d))

;; find-if の罠（どっちも NIL が返る）
(find-if #'null '(a b 5 d))
(find-if #'null '(a nil 5 d))

;; 最大値を求める
(reduce (lambda (best item)
          (if (> item best) item best))
        '(1 5 7 2 0)
        :initial-value 0)

;; 合計
(defun sum (lst) (reduce #'+ lst))

(sum '(3 4 6 5 2))
(sum (make-array 5 :initial-contents '(1 2 3 4 5)))

(map 'list
    (lambda (x)
        (if (eq x #\s)
            #\S
            x))
    "this is a string")

;; ジェネリック関数の書き方
;; ダメな例（処理コストが高い、メンテしづらい）
(defun add (a b)
    (cond ((and (numberp a) (numberp b)) (+ a b))
          ((and (listp a) (listp b)) (append a b)))))

;; 良い例（type dispatching）
(defmethod add ((a number) (b number))
    (+ a b))

(defmethod add ((a list) (b list))
    (append a b))
