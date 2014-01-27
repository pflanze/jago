;; short identifiers

(define-macro* (defstruct . args)
  `(define-struct. ,@args))

(define-macro* (def first . rest)
  (if (pair? (source-code first))
      `(define-typed ,first ,@rest)
      `(define ,first ,@rest)))

(define-macro* (def. . args)
  `(define. ,@args))

(define-macro* (defenum name . args)
  `(define-enum ,name ,@args))


;; lib

(def (vector-every fn v)
     (every fn (vector->list v)))

(def (values-of . preds)
     (let ((len (length preds)))
       (if (= len 1)
	   (car preds)
	   (L (v)
	      (and (values? v)
		   (let ((vals (values->list v)))
		     (and (= (length vals) len)
			  (every (L (val pred)
				    (pred val))
				 vals
				 preds))))))))

(TEST
 > ((values-of boolean? string?) (values #f ""))
 #t
 > ((values-of boolean? string?) (values #f))
 #f
 > ((values-of boolean?) (values #f))
 #t
 > ((values-of) (values))
 #t
 > ((values-of) (values 1 2))
 #f
 > ((values-of integer? number?) (values 1.4 2))
 #f
 > ((values-of integer? number?) (values 2 1.4))
 #t
 )

