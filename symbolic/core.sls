
(library

 (numerology symbolic core)

 (export make-constant constant? constant-value
         make-var var? var-name
         make-equation equation? equation-a equation-b
         make-add add? add-f add-g
         make-mul mul? mul-f mul-g
         make-pow pow? pow-f pow-g
         make-sub sub? sub-f sub-g
         make-div div? div-f div-g
         make-neg neg? neg-f
         make-sine sine? sine-f
         make-cosine cosine? cosine-f
         make-tangent tangent? tangent-f
         make-secant secant? secant-f
         make-cosecant cosecant? cosecant-f
         make-cotangent cotangent? cotangent-f
         make-square
         )
 
 (import (except (rnrs) div))

 ;; Misc forms
 
 (define-record-type constant (fields value))

 (define-record-type var (fields name))

 (define-record-type equation (fields a b))

 ;; Basic arithmetic forms

 (define-record-type add (fields f g))

 (define-record-type mul (fields f g))

 (define-record-type pow (fields f g))

 ;; Extended arithmetic forms

 (define-record-type sub (fields f g))

 (define-record-type div (fields f g))

 (define-record-type neg (fields f))

 ;; Basic trigonometric forms

 (define-record-type sine (fields f))

 (define-record-type cosine (fields f))

 (define-record-type tangent (fields f))

 ;; Extended trigonometric forms

 (define-record-type secant (fields f))

 (define-record-type cosecant (fields f))

 (define-record-type cotangent (fields f))

 ;; Derived forms
 
 (define (make-square f)
   (make-pow f 2))

 )