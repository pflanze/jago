(IF #t
    (begin
      ;; functional list based implementation
      (def (set-contains? s v)
	   (and (memq v s) #t))

      (def (set-add s v)
	   (cons v s))

      (def empty-set '())

      (def set? list?)

      (def (set-for-each proc s)
	   (for-each proc s))
      (def set-add! set-add)
      ;; ^ still returns the value to be used as the new set
      (def (make-set _maxsize)
	   empty-set))
    (begin
      ;; destructive bit-vector based implementation
      (def (set-contains? s v)
	   (vector-ref s v))
      (def (set-add! s v)
	   (vector-set! s v #t)
	   ;; still return the value to be used as the new set:
	   s)

      (def set? vector?) ;; size? well.

      (def (set-for-each proc s)
	   (def len (vector-length s))
	   (for..< (i 0 len)
		   (if (vector-ref s i)
		       (proc i))))
      (def (make-set maxsize)
	   (make-vector maxsize #f))))

(TEST
 > (def s
	(chain (make-set 10)
	       (set-add! 5)
	       (set-add! 6)))
 > (def l '())
 > (set-for-each (C push! l _) s)
 > (sort l <)
 (5 6)
 )

