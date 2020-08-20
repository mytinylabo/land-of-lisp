;; 普通のカッコは先頭がコマンド
;; 次の行は加算が実行されて 3 に評価される
(+ 1 2)

;; シングルクォートをつけるとデータとしてのリスト
;; 次の行は加算が実行されず、書いたままのリストになる
'(+ 1 2)

;; リストは後から eval で実行できる
(defparameter *cmd* '(+ 1 2))
(eval *cmd*)

;; 空リストと nil は同じ
;; 次の行は T に評価される
(eql nil ())

;; cons はペア（cons cell）を作る
;; 次の行は (1 . 2) に評価される。'(1 2) ではないことに注意
(cons 1 2)
(eql (cons 1 2) '(1 2)) ;; -> nil

;; cons cell が線形リストの形になっていると、それはリストとみなされる
;; リストの終端は nil
(cons 1 nil) ;; -> (1)
(cons 2 (cons 1 nil)) ;; -> (2 1)
(cons '+ (cons 2 (cons 1 nil))) ;; -> (+ 2 1)

;; 末尾が nil じゃなかったら、リストではなく入れ子の cons cell となる
;; 以下のようなデータはリストとして扱えない
;; （例えば cdr で再帰すると 1 . 0 を食わせたところでクラッシュする）
(cons '+ (cons 2 (cons 1 0))) ;; -> (+ 2 1 . 0)

;; car リストの先頭の要素を取り出す
(car '(1 2 3))

;; cdr リストの先頭以外を取り出す（リスト）
(cdr '(1 2 3))

;; car, cdr という名称は歴史的なもので、上記で示した機能とは無関係
;; Common Lisp では first, rest というエイリアスが用意されている
(first '(1 2 3))
(rest '(1 2 3))

;; 下のような関数もあるので、car, cdr で統一した方がいいかもしれない
(car '(1 2 3 4 5))    ;; -> 1
(cadr '(1 2 3 4 5))   ;; -> 2
(caddr '(1 2 3 4 5))  ;; -> 3
(cadddr '(1 2 3 4 5)) ;; -> 4

(cdr '(1 2 3 4 5))    ;; -> (2 3 4 5)
(cddr '(1 2 3 4 5))   ;; -> (3 4 5)
(cdddr '(1 2 3 4 5))  ;; -> (4 5)
(cddddr '(1 2 3 4 5)) ;; -> (5)

;; もっと複雑なパターンもある
;; ネストしたときに書く順で ar か dr の頭文字が並び、最後だけ省略せず書かれる
;; 次の行は、引数のリストに cdr > car > cdr > car をチェーンで適用する
(cadadr '((peas carrots tomatoes) (pork beef chicken) duck))
;; 以下と同じ
(car (cdr (car (cdr '((peas carrots tomatoes) (pork beef chicken) duck)))))

;; list 関数
(list 'pork 'beef 'chicken)
;; 以下と同じ
'(pork beef chicken)

;; list-eater の基本形
;; これは遅いらしい
(defun my-length (list)
    (if list
        (1+ (my-length (cdr list)))
        0))

;; nil は 'nil に評価される定数
(eql nil 'nil)
(defparameter nil 1) ;; -> 怒られる（nil の値は上書きできない）
