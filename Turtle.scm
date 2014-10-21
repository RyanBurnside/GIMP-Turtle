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
  "Forces angles to be [0 360)"
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

(define (get-current-image)
  "Return the image associated with the drawing"
  (car (gimp-image-list)))

(define (draw-line x y x2 y2)
  "Generic function to draw line in current brush"
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
  "Draw a line in the current brush and Turtle's color"
  (gimp-context-set-foreground (get-attribute 'color Turtle))
  (draw-line x y x2 y2))

(define (make-Turtle x y direction)
  "Make a fresh turtle with x y and direction"
  `((x ,x)
    (y ,y)
    (direction ,direction)
    (color "#009900") ; Hex color default is green for fun
    (drawing #t)
    (draw-command #f)))

;; Some nice wrappers which will be ... wrapped further
(define (get-attribute attrib list)
  "May be used to manually get attribute (should be wrapped for user)"
  (cadr (assq attrib list)))

(define (set-attribute! attrib value list)
  "May be used to manually set attribute (should be wrapped for user)"
  (set-car! (cdr (assq attrib list)) value))

;; Turtle specific functions
;; Relative movement commands

(define (color hex-string Turtle)
  "Set the color of the turtle ex #FFFFFF"
  (set-attribute! 'color hex-string Turtle))

(define (rt direction Turtle)
  "Rotate right current direction + direction"
  (set-attribute! 'direction
		  (angle-wrap (- (get-attribute 'direction Turtle) direction))
		  Turtle))

(define (lt direction Turtle)
  "Rotate left current direction + direction"
  (set-attribute! 'direction
		  (angle-wrap (+ (get-attribute 'direction Turtle) direction))
		  Turtle))

(define (fd steps Turtle)
  "Move steps in direction currently facing"
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
  "Pen up, disable drawing"
  (set-attribute! 'drawing #f Turtle))

(define (pd Turtle)
  "Pen down, enable drawing"
  (set-attribute! 'drawing #t Turtle))

;; Absolute movement commands
(define (mv x y Turtle)
  "Move to position"
  (set-attribute! 'x x Turtle)
  (set-attribute! 'y y Turtle))

(define (lk angle Turtle)
  "Look and face an angle"
  (set-attribute! 'direction (angle-wrap angle) Turtle))

(define (direction x y x2 y2)
  "Get direction between points"
  (modulo (rad-to-deg (atan (- y2 y) (- x2 x))) 360))

(define (distance x y x2 y2)
  "Preform a basic distance calculation"
  (let ((a (- x2 x))
	(b (- y2 y)))
    (sqrt (+ (pow a 2) (pow b 2)))))

(define (home Turtle)
  "Move the Turtle to the center of the image"
  (mv (/ (car (gimp-image-width (get-current-image))) 2.0)
      (/ (car (gimp-image-height (get-current-image))) 2.0)
      Turtle))


(define (fern length stop-length Turtle)
  "A little test, very memory intensive (fern 32 4 my-turtle) should be OK"
    (when (> length stop-length)
	  (fd length Turtle)
	  (let ((a
		 (make-Turtle (get-attribute 'x Turtle)
			      (get-attribute 'y Turtle)
			      (+ (get-attribute 'direction Turtle) 20.0)))
		(b
		 (make-Turtle (get-attribute 'x Turtle)
			      (get-attribute 'y Turtle)
			      (+ (get-attribute 'direction Turtle) 5.0)))

		(c
		 (make-Turtle (get-attribute 'x Turtle)
			      (get-attribute 'y Turtle)
			      (- (get-attribute 'direction Turtle) 20.0))))
	    (fd (* .48 length) a)
	    (fd (* .68 length) b)
	    (fd (* .54 length) c)
	    (fern (* .48 length) stop-length a)
	    (fern (* .68 length) stop-length b)
	    (fern (* .54 length) stop-length c))))

					;test
