(def (vector-every fn v)
     (every fn (vector->list v)))

;; ------ Board -------

(def (user-pos? x)
     (and (integer? x)
	  (<= 1 x 5)))

(def (internal-pos? x)
     (and (integer? x)
	  (<= 0 x 4)))

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

(def. (board.ref b #(internal-pos? row) #(internal-pos? col))
  (vector-ref (.fields b) (+ col (* 5 row))))

(def. (board.set! b #(internal-pos? row) #(internal-pos? col) #(player? v))
  (vector-set! (.fields b) (+ col (* 5 row)) v))

(def. (board.set b #(internal-pos? row) #(internal-pos? col) #(player? v))
  (let ((b* (.copy b)))
    (board.set! b* row col v)
    b*))

(def. (board.if-set b
		    #(internal-pos? row)
		    #(internal-pos? col)
		    #(player? v)
		    then
		    else)
  (xcase (.ref b row col)
	 ((none) (then (.set b row col v)))
	 ((white black) (else))))

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

(def. (round.if-play r row col then else)
  (let ((current-player (.next-player r)))
    (.if-set (.board r)
	    row
	    col
	    current-player
	    (L (b*)
	       (then (round b* 
			    (other-player current-player))))
	    else)))

;; input / output


;; "1 4" ~> (let x 1)(let y 4)
(def (if-parse-input str then els)
     (mcase (with-input-from-string str read-all)
	    (`(`row `col)
	     (if (and (user-pos? row)
		      (user-pos? col))
		 (then row col)
		 (els)))
	    (else
	     (els))))

(def (start-go)
     (read-line)
     (letrec ((loop
	       (L (r)
		  (println (.next-player r) ", place your stone (row col)")
		  (let* ((line (read-line)))
		    (if-parse-input
		     line
		     (L (row col)
			(.if-play r
				  (dec row)
				  (dec col)
				  (L (r*)
				     (println "New board:")
				     (pretty-print (.show (.board r*)))
				     (loop r*))
				  (L ()
				     (println "Position already set.")
				     (loop r))))
		     (L ()
			(println "invalid input, try again")
			(loop r)))))))
       (loop (round (make-board) 'white))))

