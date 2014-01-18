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

(def. (board.ref b x y)
  (vector-ref (.fields b) (+ x (* 5 y))))

(def. (board.set! b x y #(allocation? v))
  (vector-set! (.fields b) (+ x (* 5 y)) v))

(def (loop)
  (println "What's your name?")
  ((L ()
     (def line (read-line))
     (println "You are: " line)))
  ;; (let ()
  ;;  (define line (read-line))
  ;;  (println "You are: " line))
  (loop))

