(define-macro* (defstruct . args)
  `(define-struct. ,@args))

(define-macro* (def . args)
  `(define ,@args))

(define-macro* (def. . args)
  `(define. ,@args))

(define-macro* (defenum name . args)
  `(define-enum ,name ,@args))

