;; short identifiers

(define-macro* (& . args)
  ;; `(thunk ,@args)
  `(lambda () ,@args))

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

(define-macro* (defmacro . args)
  `(define-macro* ,@args))

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

(defmacro (-> pred expr)
  (with-gensym V
	       `(let ((,V ,expr))
		  (if (,pred ,V) ,V
		      (error "value fails to meet predicate:" ,V)))))

(TEST
 > (-> number? 5)
 5
 > (%try-error (-> number? "5"))
 #(error "value fails to meet predicate:" "5")
 )


;; better names, ok?
(define (id x) x) ;; == identity
(defmacro (let-gensym . args)
  `(with-gensym ,@args))
(defmacro (let-gensyms . args)
  `(with-gensyms ,@args))


;; like fold-right/last but do not take a tail
(def (fold-right/last* fn fnlast lis)
     (let rec ((lis lis))
       (let-pair
	((a lis*) lis)
	(if (null? lis*) (fnlast a) (fn a (rec lis*))))))

(def (or-fst-expand es)
     (fold-right/last*
      (L (e rest)
	 (let-gensym
	  V
	  `(let ((,V ,e))
	     (if (fst ,V)
		 ,V
		 ,rest))))
      id
      es))

(defmacro (or-fst . exprs)
  (or-fst-expand exprs))

(TEST
 > (values->vector (or-fst (values #f 'b) (values #t 'c) (values #f 'd)))
 #(#t c)
 > (values->vector (or-fst (values #f 'b) (values #f 'c) (values #t 'd)))
 #(#t d)
 > (values->vector (or-fst (values #f 'b) (values #f 'c) (values #f 'd)))
 #(#f d)
 )
