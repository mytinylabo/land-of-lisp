(loop for i below 5 sum i) ;; 10

(loop for i from 5 to 10 sum i) ;; 45

(loop for i in '(100 20 3) sum i) ;; 123

(loop for i below 5 do (print i))

(loop for i below 10 when (oddp i) sum i) ;; 25

(loop for i from 0 do (print i) when (= i 5) return 'falafel)

(loop for i in '(2 3 4 5 6) collect (* i i)) ;; (4 9 16 25 36)

;; 複数のループ変数は同時にインクリメントされる
(loop for x below 10 for y below 10 collect (+ x y)) ;; (0 2 4 6 8 10 12 14 16 18)

;; ループ回数は短い方
(loop for i from 0
      for day in '(monday tuesday wednesday thursday friday saturday sunday)
      collect (cons i day))

(loop for x from 1 to 9
    collect (loop for y from 1 to 9 collect (* x y)))
