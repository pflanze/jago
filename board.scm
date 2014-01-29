
(def board-dim 5)

(def (user-pos? x)
     (and (integer? x)
	  (<= 1 x board-dim)))

(def (internal-pos? x)
     (and (integer? x)
	  (<= 0 x (dec board-dim))))

(defenum player
  white black)

(defenum none
  none)

(def player-or-none? (either player? none?))


(def (board-vector? x)
     (and (vector? x)
	  (= (vector-length x) (square board-dim))
	  (vector-every player-or-none? x)))

(defstruct board
  constructor-name: _board
  #(board-vector? fields))

(def (make-board)
     (_board (make-vector (square board-dim) 'none)))

(def. (board.show b)
  (map (L (row)
	  (.row b row))
       (iota board-dim)))

(def (2d-list-square? v)
     (and (list? v)
	  (let ((l (length v)))
	    (every (both list?
			 (L (v)
			    (= (length v) l)))
		   v))))

(def (board #(2d-list-square? m))
     (_board (list->vector (apply append m))))


(def. (board.copy b)
     (_board (vector-copy (.fields b))))

(def (board-position->index row col)
     (+ col (* board-dim row)))

(def (board-positions->indexes l)
     (fold-right (L (ps l*)
		    (mcase ps
			   (`(`row `col)
			    (cons (board-position->index row col) l*))))
		 '()
		 l))

(def board-positions->sorted-indexes
     (compose (C sort _ <) board-positions->indexes))

(def. (board.ref b #(internal-pos? row) #(internal-pos? col))
  (vector-ref (.fields b) (board-position->index row col)))

(def. (board.set! b #(internal-pos? row) #(internal-pos? col) #(player-or-none? v))
  (vector-set! (.fields b) (board-position->index row col) v))

(def. (board.set b #(internal-pos? row) #(internal-pos? col) #(player-or-none? v))
  (let ((b* (.copy b)))
    (board.set! b* row col v)
    b*))

(def. (board.if-set/emptycheck b
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
       (iota board-dim)))


(def. (board.freedoms b
		      #(internal-pos? row)
		      #(internal-pos? col)
		      #(player? me))
  (-> (values-of boolean? set?)
      (let* ((cond-is-free
	      (L (row col yes no continue)
		 (let ((v (.ref b row col)))
		   (xcase v
			  ((none) (yes))
			  ((white black)
			   (if (eq? v me)
			       (continue)
			       (no)))))))
	     (if-fst/pass
	      (L (vals f)
		 (letv ((done? state) vals)
		       (if done?
			   vals
			   (f state))))))
	
	(letrec ((search
		  ;; return false if position has been found to not have
		  ;; freedoms
		  (L (searched row col)
		     (if (and (internal-pos? row)
			      (internal-pos? col))
			 (let ((index (board-position->index row col)))
			   (if (set-contains? searched index)
			       (values #f searched)
			       (cond-is-free
				row col
				(C values #t searched)
				(C values #f searched)
				(L ()
				   (LA if-fst/pass
				       (values #f (set-add! searched index))
				       (C search _ row (dec col)) ;; west
				       (C search _ (dec row) col) ;; north
				       (C search _ row (inc col)) ;; east
				       (C search _ (inc row) col) ;; south
				       )))))
			 (values #f searched)))))
	  (search (make-set (square board-dim)) row col)))))

(def. (board.has-freedoms? b row col)
  (letv ((has? set) (board.freedoms b row col (.ref b row col)))
	has?))


(TEST
 ;;                 0     1     2     3     4
 > (def b (board '((none  none  none  none  none)     ;; 0
		   (white white black none  none)     ;; 1
		   (none  none  black none  none)     ;; 2 
		   (none  none  none  black black)    ;; 3
		   (none  none  none  black white)))) ;; 4
 > (.has-freedoms? b 4 4)
 #f
 > (.has-freedoms? b 3 4)
 #t
 > (.has-freedoms? b 3 3)
 #t
 > (.has-freedoms? b 1 2)
 #t
 > (def b (board '((none  none  none  none  none)
		   (none  black black black none)
		   (none  black white white white)
		   (none  black white black black)
		   (none  black white black white))))
 > (.has-freedoms? b 4 2)
 #t
 > (def b (board '((none  none  none  none  none)
		   (none  black black black black)
		   (none  black white white white)
		   (none  black white black black)
		   (none  black white black white))))
 > (.has-freedoms? b 4 2)
 #f
 > (equal? (set->sorted-list (snd (.freedoms b 4 2 'white)))
	   (board-positions->sorted-indexes
	    '((2 2) (2 3) (2 4) (3 2) (4 2))))
 #t
 > (def b (board '((none  none  none  none  none)
		   (none  black black black black)
		   (none  black white white white)
		   (none  black white black black)
		   (none  black white none  white))))
 > (.has-freedoms? b 4 2)
 #t
 ;; > (set->sorted-list (snd (.freedoms b 4 2 'white)))
 ;; (22) ;; well undefined in #t cases
 )


