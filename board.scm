
(def board-dim 5)

(def (user-pos? x)
     (and (integer? x)
	  (<= 1 x board-dim)))

(def (internal-pos? x)
     (and (integer? x)
	  (<= 0 x (dec board-dim))))

(defenum player
  white black none)

(def (board-vector? x)
     (and (vector? x)
	  (= (vector-length x) (square board-dim))
	  (vector-every player? x)))

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
			 (lambda (v)
			   (= (length v) l)))
		   v))))

(def (board #(2d-list-square? m))
     (_board (list->vector (apply append m))))


(def. (board.copy b)
     (_board (vector-copy (.fields b))))

(def (board-pos->field-index row col)
     (+ col (* board-dim row)))

(def. (board.ref b #(internal-pos? row) #(internal-pos? col))
  (vector-ref (.fields b) (board-pos->field-index row col)))

(def. (board.set! b #(internal-pos? row) #(internal-pos? col) #(player? v))
  (vector-set! (.fields b) (board-pos->field-index row col) v))

(def. (board.set b #(internal-pos? row) #(internal-pos? col) #(player? v))
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


(def (set-contains? s v)
     (and (memq v s) #t))

(def (set-add s v)
     (cons v s))

(def empty-set '())

(def. (board.has-freedoms? b row col)
  (let* ((me (.ref b row col))	 
	 (cond-is-free
	  (L (row col yes no continue)
	     (if (and (internal-pos? row)
		      (internal-pos? col))
		 (let ((v (.ref b row col)))
		   (xcase v
			  ((none) (yes))
			  ((white black)
			   (if (eq? v me)
			       (continue)
			       (no)))))
		 (no)))))
    (assert (not (eq? me 'none)))
    (letrec ((search
	      (L (searched row col)
		 (let ((index (board-pos->field-index row col)))
		   (if (set-contains? searched index)
		       #f
		       (cond-is-free
			row col
			true/0
			false/0 ;;XXXX
			(L ()
			   (let ((searched* (set-add searched index)))
			     (or (search searched* row (dec col)) ;; west
				 (search searched* (dec row) col) ;; north
				 (search searched* row (inc col)) ;; east
				 (search searched* (inc row) col) ;; south
				 )))))))))
      (search empty-set row col))))


(TEST
 > (def b (board '((none  none  none  none  none)
		   (white white black none  none)
		   (none  none  black none  none)
		   (none  none  none  black black)
		   (none  none  none  black white))))
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
 > (def b (board '((none  none  none  none  none)
		   (none  black black black black)
		   (none  black white white white)
		   (none  black white black black)
		   (none  black white none  white))))
 > (.has-freedoms? b 4 2)
 #t
 )

