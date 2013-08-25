;;name:yuyongjiang
;;school:Buaa
;;Email:yuyongjianghit@163.com



(define (eq24? num)
  (cond ((number? num) (< (abs (- num 24)) 0.01))
        (else #f)))
                      

(define (operate list-input)
 (let ((add-o (calculate 
              (append (cons (+ (car list-input) (cadr list-input)) '())
                      (cddr list-input))))
   (sub-o (calculate 
              (append (cons (- (car list-input) (cadr list-input)) '())
                      (cddr list-input))))
   (mul-o (calculate 
              (append (cons (* (car list-input) (cadr list-input)) '())
                      (cddr list-input))))
   (div-o (if (> (cadr list-input) 0) 
              (calculate 
               (append (cons (/ (car list-input) (cadr list-input)) '())
                      (cddr list-input)))
              #f)))
   (cond ((eq24? add-o) add-o)
         
         ((eq24? sub-o) sub-o)
         
         ((eq24? mul-o) mul-o)
         
         ((eq24? div-o) div-o)
         (else #f))))

(define (extend-o input)
  (cond ((eq? (length input) 1) (cons input '()))
        ((eq? (length input) 2) (append
                                 (cons input '())
                                 (cons (reverse input) '())))
        ((eq? (length input) 3) (list (list (list-ref input 0) (list-ref input 1) (list-ref input 2))
                                      (list (list-ref input 1) (list-ref input 0) (list-ref input 2))
                                       (list (list-ref input 0) (list-ref input 2) (list-ref input 1))
                                       (list (list-ref input 2) (list-ref input 0) (list-ref input 1))
                                       (list (list-ref input 1) (list-ref input 2) (list-ref input 0))
                                       (list (list-ref input 2) (list-ref input 1) (list-ref input 0))))))
                                       
                       
(define (calculate-helper input-list)
  (if (null? input-list) #f
      (let ((answer (operate (car input-list))))
        (if (eq? answer #f)
            (calculate-helper (cdr input-list))
            answer))))

(define (calculate  input)
  (cond ((eq? (length input) 1) (if 
                                 (eq24? (car input))
                                 24
                                 #f))
        (else (calculate-helper (extend-o input)))))

  
;; input '(4 6 1 1)
;; output '( leftbracket 4 * 6 rightbracket * 1 * 1)
