
(library
 
 (numerology symbolic expand)

 (export expand)

 (import (rnrs)
         (numerology symbolic core))

 ;; (a + b) * c
 ;;
 ;; a c + b c

 (define (expand-mul-add-oth expr)

   (let ((a (add-f (mul-f expr)))
         (b (add-g (mul-f expr)))
         (c (mul-g expr)))

     (expand
      (make-add (expand (make-mul a c))
                (expand (make-mul b c))))))

 (define (expand-mul-oth-add expr)
   (expand-mul-add-oth
    (make-mul (mul-g expr)
              (mul-f expr))))

 ;; (a - b) * c
 ;;
 ;; a c - b c

 (define (expand-mul-sub-oth expr)

   (let ((a (sub-f (mul-f expr)))
         (b (sub-g (mul-f expr)))
         (c (mul-g expr)))

     (expand
      (make-sub (expand (make-mul a c))
                (expand (make-mul b c))))))

 (define (expand-mul-oth-sub expr)
   (expand-mul-sub-oth
    (make-mul (mul-g expr)
              (mul-f expr))))

 ;; (a + b) ^ n
 ;;
 ;; (a + b) * (a + b) ^ (n - 1)

 (define (expand-pow-add-or-sub expr)

   (let ((f (pow-f expr))
         (g (pow-g expr)))

     (cond ((and (constant? g) (= (constant-value g) 1)) f)

           ((and (constant? g) (>= (constant-value g) 2))
            (expand
             (make-mul f
                       (expand
                        (make-pow f
                                  (make-constant (- (constant-value g) 1)))))))

           (else expr))))
 
 (define (expand expr)
   
   (cond ((and (mul? expr)
               (add? (mul-f expr)))
          (expand-mul-add-oth expr))

         ((and (mul? expr)
               (add? (mul-g expr)))
          (expand-mul-oth-add expr))

         ((and (mul? expr)
               (sub? (mul-f expr)))
          (expand-mul-sub-oth expr))

         ((and (mul? expr)
               (sub? (mul-g expr)))
          (expand-mul-oth-sub expr))

         ((and (pow? expr)
               (or (add? (pow-f expr))
                   (sub? (pow-f expr)))
               (constant? (pow-g expr)))
          (expand-pow-add-or-sub expr))

         ((add? expr)
          (make-add (expand (add-f expr))
                    (expand (add-g expr))))

         ((sub? expr)
          (make-sub (expand (sub-f expr))
                    (expand (sub-g expr))))

         ((div? expr)
          (make-div (expand (div-f expr))
                    (expand (div-g expr))))

         (else expr)))

 )