;(load "../BookLib/Simply.scm")

(define (card-val hand_card)
		(cond ((equal? (butfirst hand_card) 'a) 4)
			  ((equal? (butfirst hand_card) 'k) 3)
			  ((equal? (butfirst hand_card) 'q) 2)
			  ((equal? (butfirst hand_card) 'j) 1)
			  (else 0)))

(define (high-card-points hand_card)
		(accumulate  + (every card-val hand_card)))


(define (count-suit colo hand_card)
		(let ((match (lambda (colo)
						(lambda (suit) 
			           		(if (equal? colo (first suit)) 1	0)))))	
		(accumulate  + (every (match colo) hand_card)))) 

(define (suit-counts hand_card)
		(se (count-suit 's hand_card) (count-suit 'h hand_card) (count-suit 'c hand_card) (count-suit 'd hand_card)))

(define (suit-dist-points num)
		(cond ((= num 0) 3)
			  ((= num 1) 2)
			  ((= num 2) 1)
			  (else 0)))

(define (hand-dist-points hand_card)
		(accumulate + (every suit-dist-points (suit-counts hand_card))))

(define (bridge-val hand_card)
		(+ (high-card-points hand_card) (hand-dist-points hand_card)))


(bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))
