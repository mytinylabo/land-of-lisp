
;; t: 標準出力
(format t "Add onion rings for only ~$ dollers more!" 1.5)

;; nil: 文字列
(princ (reverse (format nil "Add onion rings for only ~$ dollers more!" 1.5)))

;; ~s: デリミタあり（print, prin1 と同じ形式）
(format t "I am printing ~s in the middle of this sentence." "foo")

;; ~a: デリミタなし（princ と同じ形式）
(format t "I am printing ~a in the middle of this sentence." "foo")

;; パディング右寄せ、左寄せ
(format t "[~10s][~10@a]" "foo" "bar")

;; 固定長パディング
(format t "[~,,5s][~,,10@a]" "foo" "bar")

;; 詰める文字の指定
(format t "[~10,,,'-s][~10,,,'_@a]" "foo" "bar")

;; 整数の出力
(format t "The number 1000 in hexadecimal is ~x" 1000) ;; hex
(format t "The number 1000 in hexadecimal is ~b" 1000) ;; binary
(format t "The number 1000 in hexadecimal is ~d" 1000) ;; decimal

;; ~a, ~s の時とパラメータの順番や挙動が違う
(format t "The number 1000 in hexadecimal is ~8,'0x" 1000) ;; 8 桁, 0 埋め, 右寄せ（@ 不要）

;; 浮動小数点数の出力
(format t "PI can be estimated as ~4f" pi)  ;; 総桁数
(format t "PI can be estimated as ~,4f" pi) ;; 小数点以下の桁数
(format t "Percentages are ~,,2f percent better then fractions" 0.77) ;; 10^n でスケール
(format t "I wish I had ~$ dollars in my bank account." 1000000.2) ;; 通貨（ドル）

;; ~%: terpri
(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))

(format t "this will print ~5%on two lines spread far apart") ;; 改行数の指定

;; ~&: fresh-line
(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))

;; レイアウトいろいろ
(defun random-animal ()
    (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(loop repeat 10
    do (format t "~5t~a ~15t~a ~25t~a~%"
            (random-animal)
            (random-animal)
            (random-animal)))

(loop repeat 10
    do (format t "~30<~a~;~a~;~a~>~%"
            (random-animal)
            (random-animal)
            (random-animal)))

(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))

(loop repeat 10
    do (format t "~30:@<~a~;~a~;~a~>~%"
            (random-animal)
            (random-animal)
            (random-animal)))

(loop repeat 10
    do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
            (random-animal)
            (random-animal)
            (random-animal)))

(defparameter *animals* (loop repeat 10 collect (random-animal)))

;; リストのイテレートもできる
(format t "~{I see a ~a! ~}" *animals*)

;; 1 回のイテレートで複数の要素を取り出す
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)

;; ~,n:;
;; カーソル位置が n 列を超えたときのみ、これより前にあるコントロール文字列がトリガーされる
(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))

(format t "~<foo ~,10:;aaaaaaaaaa~>")  ;; aaaaaaaaaa
(format t "~<foo ~,10:;aaaaaaaaaaa~>") ;; foo aaaaaaaaaaa
