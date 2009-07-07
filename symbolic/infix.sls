
(library

 (numerology symbolic infix)

 (export infix)
 
 (import (rnrs) (only (srfi :1) circular-list))

 (define precedence-table
   '((sentinel . 0)
     (=        . 1)
     (+        . 2)
     (-        . 2)
     (*        . 3)
     (/        . 3)
     (^        . 4)))

 (define (precedence item)
   (cdr (assq item precedence-table)))

 (define (right-associative? obj)
   (member obj '(^)))

 (define operators (cdr (map car precedence-table)))

 (define (operator? obj)
   (member obj operators))

 (define (shunting-yard expr operands operators)

   (define (apply-operator)
     (shunting-yard expr
                    (cons (list (car operators)
                                (list-ref operands 1)
                                (list-ref operands 0))
                          (cdr (cdr operands)))
                    (cdr operators)))

   (if (null? expr)
       
       (if (eq? (car operators) 'sentinel)
           (car operands)
           (apply-operator))

       (let ((elt (car expr)))
         
         (cond ((operator? elt)

                (if (or (> (precedence elt)
                           (precedence (car operators)))

                        (and (right-associative? elt)

                             (= (precedence elt)
                                (precedence (car operators)))))

                    (shunting-yard (cdr expr) operands (cons elt operators))

                    (apply-operator)))

               ((list? elt)
                (shunting-yard (cdr expr)
                               (cons (infix elt) operands)
                               operators))

               (else

                (shunting-yard (cdr expr) (cons elt operands) operators))))))
 
 (define (infix expr)
   (shunting-yard expr '() (circular-list 'sentinel)))

 )

;; Examples

;; (infix '(a + b - c * d / e = f))
;;
;; (= (- (+ a b) (/ (* c d) e)) f)

;; (infix '( (a - b) / (c + d) ))
;;
;; (/ (- a b) (+ c d))

;; ^ is right associative:
;;
;; > (infix '(a ^ b ^ c))
;;
;; (^ a (^ b c))

;; > (infix '(a + b + c))
;;
;; (+ (+ a b) c)