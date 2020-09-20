
(output-stream-p *standard-output*)

(write-char #\x *standard-output*)

(input-stream-p *standard-input*)

(read-char *standard-input*)

;; ファイルへの読み書き
(with-open-file (my-stream "data.txt" :direction :output)
    (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :input)
    (read my-stream))

;; CL のデータタイプをそのまま読み書きできる
(let ((animal-noises '((dog . woof) (cat . meow))))
    (with-open-file (my-stream "animal-noises.txt" :direction :output)
        (print animal-noises my-stream)))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
    (read my-stream))

;; 同名のファイルがあったらエラー
(with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
    (print "my data" my-stream))

;; 同名のファイルがあったら上書き（未指定と同じ動作？）
(with-open-file (my-stream "data.txt" :direction :output :if-exists :supersede)
    (print "my data" my-stream))
