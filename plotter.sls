
(library

 (numerology plotter)

 (export plotter
         set-functions)

 (import (rnrs)
         (rnrs eval)
         (only (srfi :1) iota)
         (gl)
         (glut)
         (glamour window)
         (glamour misc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define x-min -10.0)
 (define x-max  10.0)
 (define y-min -10.0)
 (define y-max  10.0)

 (define (step-size)
   (/ (- x-max x-min) 100))

 (define functions '())

 (define (set-functions val)
   (set! functions val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (axis)

   (glColor4d 1.0 1.0 1.0 1.0)

   (gl-begin GL_LINE_STRIP
             (glVertex2d x-min 0.0)
             (glVertex2d x-max 0.0))

   (gl-begin GL_LINE_STRIP
             (glVertex2d 0.0 y-min)
             (glVertex2d 0.0 y-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (zoom-in)

   (let ((x-inc (* (- x-max x-min) 1/10))
         (y-inc (* (- y-max y-min) 1/10)))

     (set! x-min (+ x-min x-inc))
     (set! x-max (- x-max x-inc))

     (set! y-min (+ y-min y-inc))
     (set! y-max (- y-max y-inc))

     (glutPostRedisplay)))

 (define (zoom-out)

   (let ((x-inc (* (- x-max x-min) 1/10))
         (y-inc (* (- y-max y-min) 1/10)))

     (set! x-min (- x-min x-inc))
     (set! x-max (+ x-max x-inc))

     (set! y-min (- y-min y-inc))
     (set! y-max (+ y-max y-inc))

     (glutPostRedisplay)))

 (define (move-left)

   (let ((x-inc (* (- x-max x-min) 1/10)))

     (set! x-min (- x-min x-inc))
     (set! x-max (- x-max x-inc))

     (glutPostRedisplay)))

 (define (move-right)

   (let ((x-inc (* (- x-max x-min) 1/10)))

     (set! x-min (+ x-min x-inc))
     (set! x-max (+ x-max x-inc))

     (glutPostRedisplay)))

 (define (move-up)

   (let ((y-inc (* (- y-max y-min) 1/10)))

     (set! y-min (+ y-min y-inc))
     (set! y-max (+ y-max y-inc))

     (glutPostRedisplay)))

 (define (move-down)

   (let ((y-inc (* (- y-max y-min) 1/10)))

     (set! y-min (- y-min y-inc))
     (set! y-max (- y-max y-inc))

     (glutPostRedisplay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define point-a-x #f)
 (define point-a-y #f)
 (define point-b-x #f)
 (define point-b-y #f)

 (define (zoom-to-a-b)

   (if (and point-a-x point-a-y point-b-x point-b-y)

       (begin

         (set! x-min (inexact (min point-a-x point-b-x)))
         (set! x-max (inexact (max point-a-x point-b-x)))

         (set! y-min (inexact (min point-a-y point-b-y)))
         (set! y-max (inexact (max point-a-y point-b-y)))

         (set! point-a-x #f)
         (set! point-a-y #f)
         (set! point-b-x #f)
         (set! point-b-y #f)
         
         (glutPostRedisplay))))

 (define (set-a-b x y)

   (cond ( (not point-a-x)

           (set! point-a-x x)
           (set! point-a-y y) )

         ( (not point-b-x)

           (set! point-b-x x)
           (set! point-b-y y)

           (zoom-to-a-b) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define window-width  #f)
 (define window-height #f)

 (define (window->world-x x)

   (+ x-min

      (* (/ x window-width)

         (- x-max x-min))))

 (define (window->world-y y)

   (- y-max

      (* (/ y window-height)

         (- y-max y-min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (menu)

   (for-each (lambda (line) (display line) (newline))

             '(
               " a d w s - Move left right up down"
               ""
               " r f - Zoom in / out"
               ""
               " e - Specify corners of new view"
               ""
               " Any other key - Switch to repl"
               )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (plotter)

   (define no-op (initialize-glut))

   (window (size 400 400)
           (title "plotter")
           (reshape (width height)
                    (lambda (w h)
                      (set! window-width  w)
                      (set! window-height h))))

   (buffered-display-procedure
    (lambda ()

      (glMatrixMode GL_PROJECTION)

      (glLoadIdentity)
      
      (glOrtho x-min x-max y-min y-max -10.0 10.0)

      (glMatrixMode GL_MODELVIEW)

      (glLoadIdentity)

      (background 0.0)

      (axis)

      (glColor4d 1.0 0.0 0.0 1.0)
      
      (for-each (lambda (i f)

                  (glColor4d 1.0
                             (inexact (/ i (max (- (length functions) 1) 1)))
                             0.0
                             1.0)

                  (gl-begin
                   
                   GL_LINE_STRIP

                   (do ((x x-min (+ x (step-size))))
                       ((> x x-max))

                     (glVertex2d x (inexact (f x))))))

                (iota (length functions))
                functions)))
   
   (glutKeyboardFunc
    (lambda (key x y)

      (case (integer->char key)

        ((#\a) (move-left))
        ((#\d) (move-right))
        ((#\w) (move-up))
        ((#\s) (move-down))

        ((#\r) (zoom-in))
        ((#\f) (zoom-out))

        ((#\e) (set-a-b (window->world-x x)
                        (window->world-y y)))

        (else

         (let loop ()

           (display "plot> ")

           (let ((expr (read)))

             (if (eof-object? expr)
                 (begin
                   (display "\nplotting...\n")
                   (glutPostRedisplay))
                 (begin
                   (display (eval expr
                                  (environment '(rnrs)
                                               '(numerology plotter)
                                               )))
                   (newline)
                   (loop)))))))))

   (menu)
   
   (glutMainLoop))

 )
