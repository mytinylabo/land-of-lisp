;; association list と呼ばれるデータ構造
;; ゲームメッセージはテキストではなくシンボルのリストとして保持している
;; Lisp において基本的なデータ型はシンボルとリストなので、設計も極力これらを使うようにする
;; （文字列は人間寄りのデータで、コンピューティングの対象としては基本的とは言えない）
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; mapcar はリストを構成する cons cells の先頭（car）をマップしていくという意味。つまり普通の言語でいう map
;; #'append は (function append) の省略記法
(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

;; labels はローカル関数を定義する
;; flet と違い、定義した関数はお互いを参照できる
(defun objects-at (loc objs obj-locs)
    (labels ((at-loc-p (obj)
                (eq (cadr (assoc obj obj-locs)) loc)))
        (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
    (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
        (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look()
    (append (describe-location *location* *nodes*)
            (describe-paths *location* *edges*)
            (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
    (let ((next (find direction ;; direction が一致した edge が返る
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

;; 拾ったアイテムを *object-locations* から消してないが、
;; assoc は最初に見つかった要素を返すので alist は上書きされたとみなせる
;; この push/assoc のコンビネーションは Lisp の基本的なイディオム
(defun pickup (object)
    (cond ((member object (objects-at *location* *objects* *object-locations*))
                (push (list object 'body) *object-locations*)
                `(you are now carrying the ,object))
           (t '(you cannot get that.))))

(defun inventory ()
    (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; Read-Eval-Print-Loop
(defun game-repl ()
    (loop (print (eval (read)))))

(defun game-repl()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string
                    (concatenate 'string "(" (read-line) ")")))) ;; 'string は戻り値の型の指定
         (flet ((quote-it (x)
                    (list 'quote x)))
                (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
    (when lst
        (let ((item (car lst))
              (rest (cdr lst)))
            (cond ((eql item #\space) (cons item (tweak-text rest caps lit))) ;; スペースはそのまま
                  ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit))) ;; 文の終わりなので次は大文字
                  ((eql item #\") (tweak-text rest caps (not lit))) ;; "" の内外フラグを反転
                  (lit (cons item (tweak-text rest nil lit))) ;;  "" の中は加工しない
                  (caps (cons (char-upcase item) (tweak-text rest nil lit))) ;; 大文字にして、次は小文字
                  (t (cons (char-downcase item) (tweak-text rest nil nil))))))) ;; 小文字にする

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() "
                                                     (prin1-to-string lst))
                                       'list) ;; 文字列を文字のリストに変換
                               t
                               nil)
                   'string)) ;; 文字のリストを文字列に変換
    (fresh-line))
