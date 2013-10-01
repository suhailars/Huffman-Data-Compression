;tes.rkt
(define left "")
(define right "")
(define codes (make-hash))
(define freqs (make-hash))
(define tuple empty)
(define tuples empty)
(define tree empty)

(define (freq  str)     
      (for ([ch (in-string str)])
       (if (dict-has-key? freqs ch)
          (hash-set! freqs ch (add1(hash-ref freqs ch)))
          (hash-set! freqs ch 1)))
    ) 

(define (sortfreq freqs)
          (begin  (define list1 (hash-keys freqs)) 
                  (for-each (lambda (x)
                            (set! tuple (cons (list (hash-ref freqs x) x) tuple))) 
                            list1)
                  tuple   ))
                  

(define (buildtree tuple)
   (define least empty)
   (define Rest empty)
   (define branch empty)
   (define combfreq 0)    
   (if (> (length tuple) 1)
    (begin
      (set! least (list (car tuple) (car (cdr tuple))))
      (set! Rest (list-tail tuple 2))
      (set! combfreq  (+ (car (car least)) (car (car (cdr least)))))
      (set! branch (list  combfreq least))
      (set! tuple (append Rest (list branch)))
      (set! tuples (sort tuple #:key car <))
       
      (buildtree tuple) ) 
 
    (begin
         (set! tuples (first tuple))
          tuples ) ) )    


(define (trimtree tree)
            (begin 
               (define p (second tree))    
               (cond [(char? p) p]
                     [else 
                      (list (trimtree (first p)) (trimtree (second p)))] )))
             

(define (assigncodes node [pat ""])
         (if (char? node )
             (hash-set! codes node pat)                        
             (begin
  		   (set! left (string-append pat "0"))	
                   (assigncodes (first node) left) 
                   (set! right (string-append pat "1"))                                           
                   (assigncodes (second node) right)  )))

 
(define (encode str)
         (begin
               (define output "")
               (for ([ch (in-string str)])
                   (set! output (string-append output (hash-ref codes ch)))
                      ) 
               output))
    
(define (decode tree str)
        (begin
             (define output "")
             (define p tree)
             (for ([bit (in-string str)])
                (begin 
                   (if (equal? bit #\0)
                      (set! p (first p))
                      (set! p (second p)))
                   (when (char? p)
                        (set! output (string-append output (~a p)))
                        (set! p tree))
                      ))
                       output    ))
(define (main)
        (display "enter a string")
        (define str (read))
        (freq str)
        ;(printf "sorted frequencies =\n")
        (sortfreq freqs)
        (buildtree tuple)
        (set! tree (trimtree tuples))
        (printf "compression of given data\n")
        (assigncodes tree)
        (define l (encode str))
        (printf "~a\n" l)
        (display "if you want decompress data press y or n\n")
        (define q (read)) 
        (if (equal? q 'y)
            (decode tree l)
            (printf "thank you"))   
            
         )                  
;to run
;(main)   
