(def (vector-every fn v)
     (every fn (vector->list v)))

;; ------

(defenum allocation
  white black empty)

(def (board-vector? x)
     (and (vector? x)
	  (= (vector-length x) 25)
	  (vector-every allocation? x)))

(defstruct board
  constructor-name: _board
  #(board-vector? fields))

(def (make-board)
     (_board (make-vector 25 'empty)))

(def. (board.copy b)
     (_board (vector-copy (.fields b))))

(def. (board.ref b x y)
  (vector-ref (.fields b) (+ x (* 5 y))))

(def. (board.set! b x y #(allocation? v))
  (vector-set! (.fields b) (+ x (* 5 y)) v))

(def. (board.set b x y #(allocation? v))
  (let ((b* (.copy b)))
    (board.set! b* x y v)
    b*))

(def. (board.row b i)
  (map (L (j)
	  (.ref b j i))
       (iota 5)))

(def. (board.show b)
  (map (L (i)
	  (.row b i))
       (iota 5)))

;; "1 4" ~> (let x 1)(let y 4)
(def (f str fn)
     (let ((l (with-input-from-string str read-all)))
       (fn (car l) (cadr l))))

(def (go-loop)
     (read-line)
     (letrec ((loop
	       (L (board)
		  (println "Place your stone (x y)")
		  (let* ((line (read-line))
			 (board* (f line (L (x y)
					    (board.set board x y 'white)) )))
		    (println "New board:")
		    (pretty-print (.show board*))
		    (loop board*)))))
       (loop (make-board))))

