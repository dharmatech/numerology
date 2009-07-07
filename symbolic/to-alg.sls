
(library

 (numerology symbolic to-alg)

 (export to-alg)

 (import (rnrs) (numerology symbolic core))

 (define (to-alg expr)

   (cond ((constant? expr)
          (number->string (constant-value expr)))

         ((add? expr)
          (string-append "("
                         (to-alg (add-f expr)) " + " (to-alg (add-g expr))
                         ")"))

         ((sub? expr)
          (string-append "("
                         (to-alg (sub-f expr)) " - " (to-alg (sub-g expr))
                         ")"))

         ((mul? expr)
          (string-append "("
                         (to-alg (mul-f expr))
                         " * "
                         (to-alg (mul-g expr))
                         ")"))

         ((pow? expr)
          (string-append "("
                         (to-alg (pow-f expr))
                         " ^ "
                         (to-alg (pow-g expr))
                         ")"))

         ((var? expr)
          (symbol->string (var-name expr)))

         ((equation? expr)

          (string-append (to-alg (equation-a expr))
                         " = "
                         (to-alg (equation-b expr))))

         (else
          (call-with-string-output-port
           (lambda (port)
             (display expr port))))))

 )