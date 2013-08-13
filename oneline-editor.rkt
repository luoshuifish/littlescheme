;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname oneline-editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;name:yuyongjiang
;;school:Buaa
;;Email:yuyongjianghit@163.com

(require 2htdp/image)
(require 2htdp/universe)


(define WIDTH  200)
(define HEIGHT  20)

(define TEXT-SIZE  18)
(define TEXT-COLOR "BLACK")

(define CURSOR (rectangle 1 20 "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))

(define-struct editor (txt cp))


(define ED1 (make-editor ""       0))
(define ED2 (make-editor "abcdef" 0)) 
(define ED3 (make-editor "abcdef" 3)) 
(define ED4 (make-editor "abcdef" 6))



(define (main e)
  (big-bang e
            (to-draw    render)                 
            (on-key     handle-key)))   



(check-expect (render (make-editor "abcdef" 3))
              (overlay/align "left"
                             "middle"
                             (beside (text "abc" TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text "def" TEXT-SIZE TEXT-COLOR))
                             MTS))

(define (render-temp first-part second-part)
                (overlay/align "left"
                             "middle"
                             (beside (text first-part TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text second-part TEXT-SIZE TEXT-COLOR))
                             MTS))
  

(define (render e) 
  (render-temp
   (substring  (editor-txt e) 0  (editor-cp e))
   (substring  (editor-txt e)   (editor-cp e))
   ))
   

(define (insert-char str ch cp)
  (if (= 0 (string-length str))
      ch
      (string-append (substring  str 0  cp) ch (substring  str cp))))
      
  

(define (handle-key e key)
  (cond [(key=? key "left")        ( if (> (editor-cp e) 0) 
                                        (make-editor (editor-txt e) ( - (editor-cp e) 1))
                                        (make-editor (editor-txt e) (editor-cp e)))]
        [(key=? key "right")       (if (< (editor-cp e) (string-length (editor-txt e)))
                                       (make-editor (editor-txt e) ( + (editor-cp e) 1))
                                       (make-editor (editor-txt e) (editor-cp e )))]
        [(key=? key "\b")          ( if (> (editor-cp e) 0) 
                                        (make-editor (substring (editor-txt e) 0 ( - (editor-cp e) 1))  ( - (editor-cp e) 1))
                                        (make-editor (editor-txt e) (editor-cp e)))]        
        [(= (string-length key) 1) (make-editor (insert-char (editor-txt e) key (editor-cp e))
                                                ( + (editor-cp e) 1))]
        [else (make-editor (editor-txt e) (editor-cp e))]))



