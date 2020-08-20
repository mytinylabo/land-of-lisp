(print "foo")

(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))

;; read/print は Lisp のデータを生のまま入出力できる
;; 例えば read に入力するのは Lisp のリテラルなので、
;; 4 と入力したらそれは数値の 4 として入力される（文字列が自動的に数値になったのではない）
(defun say-hello ()
    (print "Please type your name:")
    (let ((name (read)))
        (print "Nice to meet you, ")
        (print name)))

;; 人間向きの出力
(princ '3)
(princ '3.4)
(princ 'foo)
(princ '"foo")
(princ '#\a)
(princ '#\newline)

(progn (princ "This sentence will be interrupted")
       (princ #\newline)
       (princ "by an annoying newline character."))

(defun say-hello ()
    (princ "Please type your name:")
    (let ((name (read-line)))
        (princ "Nice to meet you, ")
        (princ name)))
