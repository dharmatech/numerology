
(library

 (numerology symbolic to-symbolic)

 (export sexp-to-symbolic)

 (import (rnrs) (numerology symbolic core))

 (define (sexp-to-symbolic expr)

   (cond ((and (list? expr)
               (not (null? expr)))

          (case (car expr)

            ((+) (make-add (sexp-to-symbolic (list-ref expr 1))
                           (sexp-to-symbolic (list-ref expr 2))))

            ((-) (make-sub (sexp-to-symbolic (list-ref expr 1))
                           (sexp-to-symbolic (list-ref expr 2))))

            ((*) (make-mul (sexp-to-symbolic (list-ref expr 1))
                           (sexp-to-symbolic (list-ref expr 2))))

            ((/) (make-div (sexp-to-symbolic (list-ref expr 1))
                           (sexp-to-symbolic (list-ref expr 2))))

            ((^) (make-pow (sexp-to-symbolic (list-ref expr 1))
                           (sexp-to-symbolic (list-ref expr 2))))

            ((=) (make-equation (sexp-to-symbolic (list-ref expr 1))
                                (sexp-to-symbolic (list-ref expr 2))))

            ((sin) (make-sine   (sexp-to-symbolic (list-ref expr 1))))
            ((cos) (make-cosine (sexp-to-symbolic (list-ref expr 1))))

            ))

         ((number? expr) (make-constant expr))

         (else (make-var expr))))
 )