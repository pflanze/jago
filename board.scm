
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


(def board-index? (both natural0? (C < _ (square board-dim))))

(def (board-index->position #(board-index? i))
     (list (quotient i board-dim)
	   (modulo i board-dim)))

(def (board-position->index row col)
     (+ col (* board-dim row)))

(TEST
 > (def (t row col)
	(equal? (board-index->position (board-position->index row col))
		(list row col)))
 > (every t
	  '(0 0 1 1 4 4)
	  '(0 1 3 4 0 4))
 #t)

;; (def (board-positions->indexes l)
;;      (fold-right (L (ps l*)
;; 		    (mcase ps
;; 			   (`(`row `col)
;; 			    (cons (board-position->index row col) l*))))
;; 		 '()
;; 		 l))

;; (def board-positions->sorted-indexes
;;      (compose (C sort _ <) board-positions->indexes))

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


;; >> is part of the concept of monads, and chains two actions,
;; creating a new combined action. search>> is our >> for search
;; actions.

;; We want search actions to have two features: the ability to
;; shortcut evaluation (call |found|), and to pass on ("threading") of
;; the |searched| set (it is passed to |notfound|, for continuing the
;; search or to hand out as result).  We define a search action as a
;; function taking (searched found notfound) and tail-calling either
;; found or notfound (continuation-passing style).

(def (search>> a b)
     (L (searched found notfound)
	;; search using a
	(a searched
	   ;; if found, no need to deal with b
	   found
	   ;; if not found, we will search using b
	   (L (searched*)
	      (b searched* found notfound)))))

;; (Note that search>> is independent from whether found takes 0 or 1
;; arguments (or really what arguments either of found or notfound
;; take).)

;; |LA| (a macro) takes the first argument as a function and generates
;; code that applies it pair-wise, Left-Associative, to the remaining
;; arguments:
(TEST
 > (expansion#LA search>>
		 (search-free row (dec col))
		 (search-free (dec row) col)
		 (search-free row (inc col))
		 (search-free (inc row) col))
 (search>> (search>> (search>> (search-free row (dec col))
			       (search-free (dec row) col))
		     (search-free row (inc col)))
	   (search-free (inc row) col)))


(def. (board.if-has-freedoms b
			     #(internal-pos? row)
			     #(internal-pos? col)
			     #(player? me)
			     yes/0
			     no/1 ;; receives set of captured positions
			     )
  (let* ((cond-free
	  (L (row col yes no continue)
	     (let ((v (.ref b row col)))
	       (xcase v
		      ((none) (yes))
		      ((white black)
		       (if (eq? v me)
			   (continue)
			   (no))))))))
		
    (letrec
	((search-free
	  (L (row col)
	     ;; a search action
	     (L (searched found/0 notfound/1)			
		(if (and (internal-pos? row)
			 (internal-pos? col))
		    (let ((index (board-position->index row col)))
		      (if (set-contains? searched index)
			  (notfound/1 searched)
			  (cond-free row col
				     found/0
				     (C notfound/1 searched)
				     (&
				      ((LA search>>
					   (search-free row (dec col))
					   (search-free (dec row) col)
					   (search-free row (inc col))
					   (search-free (inc row) col))
				       (set-add! searched index)
				       found/0
				       notfound/1)))))
		    (notfound/1 searched))))))
	  
      ((search-free row col)
       (make-set (square board-dim))
       yes/0
       no/1))))


(def. (board.has-freedoms? b row col)
  (board.if-has-freedoms b row col (.ref b row col)
			 true/0
			 false/1))


(TEST
 > (def (free? b row col player)
	(if (eq? player (.ref b row col))
	    (.if-has-freedoms b row col player
			      (& 'free)
			      (L (captured)
				 (cons 'captured
				       (map board-index->position
					    (set->sorted-list captured)))))
	    'wrong-player))
 
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
 > (free? b 4 4 'white)
 (captured (4 4))
 > (free? b 3 4 'black)
 free
 > (free? b 3 3 'black)
 free
 > (free? b 1 2 'black)
 free
 > (def b (board '((none  none  none  none  none)
		   (none  black black black none)
		   (none  black white white white)
		   (none  black white black black)
		   (none  black white black white))))
 > (free? b 4 2 'white)
 free
 > (def b (board '((none  none  none  none  none)
		   (none  black black black black)
		   (none  black white white white)
		   (none  black white black black)
		   (none  black white black white))))
 > (free? b 4 2 'white)
 (captured (2 2) (2 3) (2 4) (3 2) (4 2))
 > (def b (board '((none  none  none  none  none)
		   (none  black black black black)
		   (none  black white white white)
		   (none  black white black black)
		   (none  black white none  white))))
 > (free? b 4 2 'white)
 free
 )


