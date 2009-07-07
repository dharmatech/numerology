
(library

 (numerology symbolic derivative)

 (export derivative)

 (import (rnrs) (numerology symbolic core))

 (define (derivative expr var)

   (define (deriv expr)
     (derivative expr var))

   (cond ((constant? expr) (make-constant 0))

         ((var? expr)
          (if (eq? (var-name expr) var)
              (make-constant 1)
              (make-constant 0)))

         ((add? expr)
          (make-add (deriv (add-f expr))
                    (deriv (add-g expr))))

         ((sub? expr)
          (make-sub (deriv (add-f expr))
                    (deriv (add-g expr))))

         ((mul? expr)
          (let ((f (mul-f expr))
                (g (mul-g expr)))
            (make-add (make-mul (deriv f) g)
                      (make-mul f (deriv g)))))

         ((div? expr)
          (let ((f (div-f expr))
                (g (div-g expr)))
            (make-div (make-sub (make-mul (deriv f) g)
                                (make-mul f (deriv g)))
                      (make-mul g g))))

         ((pow? expr)
          (let ((f (pow-f expr))
                (g (pow-g expr)))
            (make-mul (make-mul g (make-pow f (make-sub g (make-constant 1))))
                      (deriv f))))
         
         ((sine? expr)
          (let ((f (sine-f expr)))
            (make-mul (make-cosine f)
                      (deriv f))))

         ((cosine? expr)
          (let ((f (cosine-f expr)))
            (make-mul (make-neg (make-sine f))
                      (deriv f))))

         ((tangent? expr)
          (let ((f (tangent-f expr)))
            (make-mul (make-square (make-secant f))
                      (deriv f))))

         (else #f)))
 )