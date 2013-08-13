;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname quicksort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;name:yuyongjiang
;;school:buaa
;;Email:yuyongjianghit@163.com

;quick-sort use scheme

(define (quick-sort alon)
  (cond
    [(empty? alon) empty]
    [else (append (quick-sort (little alon (first alon)))
                  (list (first alon))
                  (quick-sort (bigger alon (first alon))))]))

(define (little alon num)
  (if (empty? alon)
      alon
      (if (< (first alon) num)
      (cons (first alon) (little (rest alon) num))
      (little (rest alon) num))))


(define (bigger alon num)
  (if (empty? alon)
      alon
      (if (> (first alon) num)
      (cons (first alon) (bigger (rest alon) num))
      (bigger (rest alon) num))))
      
  