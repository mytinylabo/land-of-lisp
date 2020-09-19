(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x) (eql (car x) node))
    edge-list))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x) (eql (car x) node))
    edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
      (unless (member node visited)
        (push node visited)
        (mapc (lambda (edge) (traverse (cdr edge))) ;; ペアの方をトラバースする
          (direct-edges node edge-list))))) ;; node を起点にしたエッジが抽出されるので↑
      (traverse node))
    visited))

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
            (let ((node (car x)))
              (push (cdr x) (gethash node tab)))) ;; NIL は空リストなのでいきなり push できる
          edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) t)
                 (mapc (lambda (edge)
                         (traverse edge))
                       (gethash node edge-tab)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
        (let* ((connected (get-connected (car nodes) edge-list))
               (unconnected (set-difference nodes connected))) ;; (car nodes) から繋がってないノードのリスト
          (push connected islands)
          (when unconnected
            (find-island unconnected))))) ;; 繋がってないノードがあればそこから次の島を探す
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands)) ;; 各島の先頭ノード同士を繋ぐ
      (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x) (zerop (random *cop-odds*))) edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge))) ;; cops フラグを追加するため、アトムではなくリストにする
                    (remove-duplicates (direct-edges node1 edge-list)
                                       :test #'equal))))
    (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                ;; edge-pair で双方向のエッジを作るので
                                ;; edges-with-cops に片道のみ入っている場合でも
                                ;; 最終的に双方向に cops フラグがつく
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                  (list node2 'cops)
                                  edge)))
                      node1-edges))))
    edge-alist))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
        (neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms) '(glow-worm))
                                ((some (lambda (worm)
                                          (within-one n worm edge-alist))
                                    glow-worms)
                            '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
      (find-empty-node)
      x)))

(defun draw-city ()
  (ugraph->png "city.dot" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*) ;; 訪問済みノードならノード情報(n)を開示
              (let ((n (assoc node *congestion-city-nodes*)))
                (if (eql node *player-pos*)
                  (append n '(*))
                  n))
              (list node '?))) ;; 未訪問なら ? にする
    (remove-duplicates
      (append *visited-nodes* ;; 訪問済みノードに…
        (mapcan (lambda (node)
                  (mapcar #'car
                    (cdr (assoc node *congestion-city-edges*)))) ;; そこから繋がっているノードを加える
          *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                  (if (member (car x) *visited-nodes*)
                                    x ;; エッジの先も訪問済みなら加工しない
                                    (list (car x)))) ;; 訪問してないなら cops フラグを削除する
                          (cdr (assoc node *congestion-city-edges*)))))
    *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city.dot" (known-city-nodes) (known-city-edges)))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*)))))
  (if edge
    (handle-new-place edge pos charging)
    (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*))))) ;; 一度遭遇した Glow Worm は消える
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                    (princ "You found the Wumpus!")
                                    (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (fresh-line)
                      (handle-new-place nil new-pos nil))))))
