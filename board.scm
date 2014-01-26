
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


(IF #f
    (begin
      ;; functional list based implementation
      (def (set-contains? s v)
	   (and (memq v s) #t))

      (def (set-add s v)
	   (cons v s))

      (def empty-set '())

      (def (set-for-each proc s)
	   (for-each proc s))
      (def set-add! set-add)
      (def (make-set) empty-set))
    (begin
      ;; destructive bit-vector based implementation
      (def (set-contains? s v)
	   (vector-ref s v))
      (def (set-add! s v)
	   (vector-set! s v #t)
	   s)
      (def (set-for-each proc s)
	   (for..< (i 0 (vector-length s))
		   (if (vector-ref s i)
		       (proc i))))
      (def (make-set)
	   (make-vector (square board-dim) #f))))

(TEST
 > (def s (make-set))
 > (set-add! s 5)
 > (set-add! s 6)
 > (def l '())
 > (set-for-each (C push! l _) s)
 > l
 (6 5)
 )

(def. (board.has-freedoms? b row col)
  (let* ((me (.ref b row col))	 
	 (cond-is-free
	  (L (row col yes no continue)
	     (let ((v (.ref b row col)))
	       (xcase v
		      ((none) (yes))
		      ((white black)
		       (if (eq? v me)
			   (continue)
			   (no))))))))
    (assert (not (eq? me 'none)))
    (letrec ((search
	      ;; return false if position has been found to not have
	      ;; freedoms
	      (L (searched row col)
		 (and (internal-pos? row)
		      (internal-pos? col)
		      (let ((index (board-pos->field-index row col)))
			(if (set-contains? searched index)
			    #f
			    (cond-is-free
			     row col
			     true/0
			     false/0 ;;XXXX
			     (L ()
				(let ((searched* (set-add! searched index)))
				  (or (search searched* row (dec col)) ;; west
				      (search searched* (dec row) col) ;; north
				      (search searched* row (inc col)) ;; east
				      (search searched* (inc row) col) ;; south
				      ))))))))))
      (search (make-set) row col))))


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

