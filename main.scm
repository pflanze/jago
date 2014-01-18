(define square
  (lambda (x)
    (warn "x=" x)
    (* x x)))

(define (loop)
  (println "What's your name?")
  ((lambda()
     (define line (read-line))
     (println "You are: " line)))
  ;; (let ()
  ;;  (define line (read-line))
  ;;  (println "You are: " line))
  (loop))

