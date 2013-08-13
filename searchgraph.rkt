;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname searchgraph) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;name:yuyongjiang
;;school:buaa
;;Email:yuyongjianghit@163.com

(define Graph
  '((A  (B E))
    (B  (E F))
    (C  (D))
    (D  ())
    (E  (C F))
    (F  (D G))))


;;neighbors: node G graph -> list 
(define (neighbors node graph)
  (if (null? graph) 
      '()
      (if (symbol=? (car (car graph)) node) 
           (car (cdr (car graph)))
           (neighbors node (cdr graph)))))
      



;; find-route: node node graph -> list or false
;; 从有向图中找到结点路径
(define (find-route origination destination G) 
  (cond [(symbol=? origination destination)  (list destination)]
        [else (local [(define possible-path 
                        (find-route/list (neighbors origination G) destination G))]
                
                (if (boolean?  possible-path)
                    false
                    (cons origination possible-path)
                    ))]))
                 
                                                 

;;find-route/list :list node graph -> list or false
(define (find-route/list list-origination destination G) 
  (if (null? list-origination)
      false
          (local [(define next (find-route (car list-origination) destination G))]
                (if (boolean? next)
                    (find-route/list (cdr list-origination)  destination G)
                    next
                    ))))



(find-route 'A 'G Graph) 

                
              



        
    