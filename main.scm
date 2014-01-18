(def (vector-every fn v)
     (every fn (vector->list v)))

;; ------ Board -------

(defenum player
  white black none)

(def (board-vector? x)
     (and (vector? x)
	  (= (vector-length x) 25)
	  (vector-every player? x)))

(defstruct board
  constructor-name: _board
  #(board-vector? fields))

(def (make-board)
     (_board (make-vector 25 'none)))

(def. (board.copy b)
     (_board (vector-copy (.fields b))))

(def. (board.ref b row col)
  (vector-ref (.fields b) (+ col (* 5 row))))

(def. (board.set! b row col #(player? v))
  (vector-set! (.fields b) (+ col (* 5 row)) v))

(def. (board.set b row col #(player? v))
  (let ((b* (.copy b)))
    (board.set! b* row col v)
    b*))

(def. (board.row b row)
  (map (L (col)
	  (.ref b row col))
       (iota 5)))

(def. (board.show b)
  (map (L (row)
	  (.row b row))
       (iota 5)))



;; ------- Round -------

(defstruct round
  #(board? board)
  #(player? next-player))

(def (other-player p)
     (xcase p
	    ((white) 'black)
	    ((black) 'white)))

(def. (round.play r row col)
     (round (.set (.board r) row col (.next-player r))
	    (other-player (.next-player r)) ))

;; input / output


;; "1 4" ~> (let x 1)(let y 4)
(def (parse-input str fn)
     (let ((l (with-input-from-string str read-all)))
       (fn (car l) (cadr l))))

(def (start-go)
     (read-line)
     (letrec ((loop
	       (L (r)
		  (println (.next-player r) ", place your stone (row col)")
		  (let* ((line (read-line))
			 (r* (parse-input line
					  (L (row col)
					     (.play r (dec row) (dec col))  ) )))
		    (println "New board:")
		    (pretty-print (.show (.board r*)))
		    (loop r*)))))
       (loop (round (make-board) 'white))))

