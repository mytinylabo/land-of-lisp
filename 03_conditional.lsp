;; if の第 2 引数以降は、if の実行前には評価されない。これは特別な挙動（special form）
;; 以下のコードを実行しても 0 除算は発生しない
(if (oddp 5)
    'odd-number
    (/ 1 0))

;; if の結果には単一のコマンドまたは値しか渡せない
;; 複数のコマンドを実行したい場合 progn を使う
;; progn は渡された全ての引数を評価し、最後の引数の評価結果を progn 自体の値として返す
(defvar *number-was-odd* nil)
(if (oddp 5)
    (progn (setf *number-was-odd* t)
           'odd-number)
    'even-number)

;; いちいち progn を書く手間を省くため when/unless コマンドがある
;; when は第 1 引数（condition）が true なら以降の引数を全部評価して最後のものを返す
;; unless はその false(=nil) 版
(when (oddp 5)
    (setf *number-was-odd* t)
    'odd-number)

(unless (oddp 4)
    (setf *number-was-odd* nil)
    'even-number)

;; when で condition が false、unless で true の場合は nil を返す
(when (oddp 4)
    (setf *number-was-odd* t)
    'odd-number) ;; -> nil

;; cond は全部入り（ほかの言語の switch 文に近い）
;; Lisp で一番ベーシックな条件分岐は（if ではなく）cond だと考えられている
(defun my-count (x)
    (cond ((eq x 1) (setf *result* 'one)
                    '(there is one thing))
          ((eq x 2) (setf *result* 'two)
                    '(there are two things))
          ((> x 2)  (setf *result* 'many)
                    '(there are too many things))
          (t        (setf *result* 'unknown) ;; ほかの言語でいう default。built-in ではなくイディオム
                    '(I cannnot understand the situation at all))))

;; case は eq を省略できる
(case *x*
    (1 'one)
    (2 'two)
    (otherwise 'other))

;; Lisp では nil 以外は true に評価されるので、true or false を判断するコマンドでも
;; 単に true を返すのではなく「他に有用な情報を返せないか？」という観点で設計する
;; member は要素が見つかったら、その箇所以降のサブリストを返す
(member 3 '(1 2 3 4 5)) ;; -> (3 4 5)
;; もし見つかった要素をそのまま返すコマンドだったら以下の場合に誤動作する
(member nil '(1 2 nil 4 5)) ;; これが nil を返すとそもそもの真偽判定として使えない
;; ↑のミスをやっちゃってるのが find-if
(find-if #'null '(2 4 nil 6))
