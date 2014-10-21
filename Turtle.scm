;;;; Ryan Burnside
;;;; 2014 LOGO like implimentation for Gimp
;;;; If it blows up your computer or kills your cat don't come whinning to me.
;;;; No honestly, I mean if your computer reboots from a fine OS into Windows
;;;; it isn't my fault.


;; Turtle is just an associative list but we write some nice methods
;; To give the illusion of a modern class

(define (deg-to-rad degrees)
  (* degrees 0.0174532925))

(define (rad-to-deg radians)
  (* radians 57.2957795))


(define (angle-wrap angle)
  (let* ((num_times (truncate (/ angle 360.0)))
	 (whole-part (* num_times 360.0))
	 (reduced (- angle whole-part)))
    (if (< reduced 0.0)
	(+ 360.0 reduced)
	reduced)))

(define (alist-copy alist)
            (if (null? alist)
                '()
                (cons (cons (car (car alist)) (cdr (car alist)))
                      (alist-copy (cdr alist)))))

(define (draw-line x y x2 y2)
  (let ((layer (car (gimp-image-get-active-layer
		     (vector-ref (cadr (gimp-image-list)) 0))))
        (points (cons-array 4 'double)))
    (aset points 0 x)
    (aset points 1 y)
    (aset points 2 x2)
    (aset points 3 y2)
    (gimp-paintbrush-default layer 4 points)))
    ;(gimp-pencil layer 4 points)))

(define (Turtle-draw-line x y x2 y2 Turtle)
    (gimp-context-set-foreground (get-attribute 'color Turtle))
    (draw-line x y x2 y2))

(define (make-Turtle x y direction) 
  `((x ,x)
    (y ,y)
    (direction ,direction)
    (color "#009900") ; Hex color default is green for fun
    (drawing #t)
    (draw-command #f)))

;; Some nice wrappers which will be ... wrapped further
(define (get-attribute attrib list)
  (cadr (assq attrib list)))

(define (set-attribute! attrib value list)
  (set-car! (cdr (assq attrib list)) value))

;; Turtle specific functions

;; Relative movement commands
(define (color hex-string Turtle)
  (set-attribute! 'color hex-string Turtle))

(define (rt direction Turtle)
  (set-attribute! 'direction
		  (angle-wrap (- (get-attribute 'direction Turtle) direction))
		  Turtle))

(define (lt direction Turtle)
  (set-attribute! 'direction
		  (angle-wrap (+ (get-attribute 'direction Turtle) direction))
		  Turtle))

(define (fd steps Turtle)
  (let* ((direction (deg-to-rad (get-attribute 'direction Turtle)))
	 (last-x (get-attribute 'x Turtle))
	 (last-y (get-attribute 'y Turtle))
	 (new-x (+ last-x (* (cos direction) steps)))
	 (new-y (- last-y (* (sin direction) steps))))
    (set-attribute! 'x new-x Turtle)
    (set-attribute! 'y new-y Turtle)
    (if (get-attribute 'drawing Turtle)
	(Turtle-draw-line last-x last-y new-x new-y Turtle))))

(define (pu Turtle)
  (set-attribute! 'drawing #f Turtle))

(define (pd Turtle)
  (set-attribute! 'drawing #t Turtle))

;; Absolute movement commands
(define (mv x y Turtle)
  (set-attribute! 'x x Turtle)
  (set-attribute! 'y y Turtle))

(define (lk angle Turtle)
  (set-attribute! 'direction (angle-wrap angle) Turtle))

(define (direction x y x2 y2)
  (modulo (rad-to-deg (atan (- y2 y) (- x2 x))) 360))
    

(define (distance x y x2 y2)
  (let ((a (- x2 x))
	(b (- y2 y)))
    (sqrt (+ (pow a 2) (pow b 2)))))

