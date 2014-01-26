;; ------- Round -------

(defstruct Round
  #(board? board)
  #(player? next-player))

(def (other-player p)
     (xcase p
	    ((white) 'black)
	    ((black) 'white)))

(def. (Round.if-play r row col then else)
  (let ((current-player (.next-player r)))
    (.if-set/emptycheck (.board r)
			row
			col
			current-player
			(L (b*)
			   (then (Round b* 
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
       (loop (Round (make-board) 'white))))

