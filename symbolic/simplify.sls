
(library

 (numerology symbolic simplify)

 (export simplify)

 (import (rnrs) (numerology symbolic core))
 
 (define (simplify-add expr)
   (let ((f (simplify (add-f expr)))
         (g (simplify (add-g expr))))
     (cond ((and (constant? f) (constant? g))
            (make-constant (+ (constant-value f)
                              (constant-value g))))
           ((and (constant? f) (= 0 (constant-value f))) g)
           ((and (constant? g) (= 0 (constant-value g))) f)
           (else (make-add f g)))))

 (define (simplify-mul expr)
   (let ((f (simplify (mul-f expr)))
         (g (simplify (mul-g expr))))
     (cond ((and (constant? f) (constant? g))
            (make-constant (* (constant-value f)
                              (constant-value g))))
           ((or (and (constant? f) (= 0 (constant-value f)))
                (and (constant? g) (= 0 (constant-value g))))
            (make-constant 0))
           ((and (constant? f) (= 1 (constant-value f))) g)
           ((and (constant? g) (= 1 (constant-value g))) f)
           (else (make-mul f g)))))

 (define (simplify-pow expr)
   (let ((f (simplify (pow-f expr)))
         (g (simplify (pow-g expr))))
     (cond ((and (constant? f) (constant? g))
            (make-constant (expt (constant-value f) (constant-value g))))
           ((and (constant? g) (= (constant-value g) 0)) (make-constant 1))
           ((and (constant? g) (= (constant-value g) 1)) f)
           (else (make-pow f g)))))

 (define (simplify-equation expr)
   (let ((a (simplify (equation-a expr)))
         (b (simplify (equation-b expr))))
     (make-equation a b)))

 (define (simplify expr)
   (cond ((add?      expr) (simplify-add      expr))
         ((mul?      expr) (simplify-mul      expr))
         ((pow?      expr) (simplify-pow      expr))
         ((equation? expr) (simplify-equation expr))
         (else expr)))

 )