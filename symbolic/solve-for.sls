
(library

 (numerology symbolic solve-for)

 (export solve-for)

 (import (rnrs) (numerology symbolic core))

 (define (contains? expr var)
   (or (and (var? expr)
            (eq? (var-name expr) var))

       (and (add? expr)
            (or (contains? (add-f expr) var)
                (contains? (add-g expr) var)))

       (and (mul? expr)
            (or (contains? (mul-f expr) var)
                (contains? (mul-g expr) var)))

       (and (pow? expr)
            (or (contains? (pow-f expr) var)
                (contains? (pow-g expr) var)))))

 (define (solve-for-add eq var)

   (let ((a (equation-a eq))
         (b (equation-b eq)))

     (let ((f (add-f a))
           (g (add-g a)))

       (cond ((contains? f var)
              (solve-for (make-equation f (make-sub b g)) var))

             ((contains? g var)
              (solve-for (make-equation g (make-sub b f))))))))

 (define (solve-for-mul eq var)

   (let ((a (equation-a eq))
         (b (equation-b eq)))

     (let ((f (mul-f a))
           (g (mul-g a)))

       (cond ((contains? f var)
              (solve-for (make-equation f (make-div b g)) var))

             ((contains? g var)
              (solve-for (make-equation g (make-div b f)) var))))))

 (define (solve-for-pow eq var)

   (let ((a (equation-a eq))
         (b (equation-b eq)))

     (let ((f (pow-f a))
           (g (pow-g a)))

       (cond ((contains? f var)
              (solve-for (make-equation f
                                        (make-pow b
                                                  (make-div (make-constant 1)
                                                            g)))
                         var))))))

 (define (solve-for eq var)

   (let ((a (equation-a eq))
         (b (equation-b eq)))

     (cond ((contains? b var)
            (solve-for (make-equation b a) var))

           ((and (var? a) (eq? (var-name a) var)) eq)

           ((add? a) (solve-for-add eq var))

           ((mul? a) (solve-for-mul eq var))

           ((pow? a) (solve-for-pow eq var))
           )))

 )

;; (solve-for (make-equation (make-add (make-add (make-var 'a)
;;                                               (make-var 'b))
;;                                     (make-var 'c))
;;                           (make-var 'd))
;;            'a)