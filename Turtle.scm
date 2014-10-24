;;;; Ryan Burnside
;;;; 2014 LOGO like implementation for Gimp
;;;; If it blows up your computer or kills your cat don't come whining to me.
;;;; No honestly, I mean if your computer reboots from a fine OS into Windows
;;;; it isn't my fault.

;; Turtle is just an associative list but we write some nice methods
;; To give the illusion of a modern class

;; TODO Add a with-state and with-undo macros 
   ;to save and restore state and undo

;; Utility functions
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

(define (direction x y x2 y2)
  "Get direction between points"
  (modulo (rad-to-deg (atan (- y2 y) (- x2 x))) 360))

(define (distance x y x2 y2)
  "Preform a basic distance calculation"
  (let ((a (- x2 x))
	(b (- y2 y)))
    (sqrt (+ (pow a 2) (pow b 2)))))

(define (get-current-image)
  "Return the image associated with the drawing"
  (car (gimp-image-list)))

(define (get-current-layer)
  "Return the current layer on focused image"
  (car (gimp-image-get-active-layer (vector-ref (cadr (gimp-image-list)) 0))))

(define (draw-line x y x2 y2)
  "Generic function to draw line in current brush"
  (let ((layer (get-current-layer))
	(points (cons-array 4 'double)))
    (aset points 0 x)
    (aset points 1 y)
    (aset points 2 x2)
    (aset points 3 y2)
    (gimp-paintbrush-default layer 4 points)))

(define (Turtle-draw-line x y x2 y2 Turtle)
  "Draw a line in the current brush and Turtle's color"
  (gimp-context-set-foreground (get-attribute 'color Turtle))
  (draw-line x y x2 y2))

(define (make-Turtle x y direction)
  "Make a fresh turtle with x y and direction"
  `((x ,x)
    (y ,y)
    (direction ,direction)
    (color "#000000") ; Hex color default is black
    (drawing #t)
    (draw-command #f)))

;; Some nice wrappers which will be ... wrapped further
(define (get-attribute attrib list)
  "May be used to manually get attribute (should be wrapped for user)"
  (cadr (assq attrib list)))

(define (set-attribute! attrib value list)
  "May be used to manually set attribute (should be wrapped for user)"
  (set-car! (cdr (assq attrib list)) value))

;;; Turtle specific functions

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

(define (bk steps Turtle)
  "Move steps backwards while keeping currently facing direction"
  (let* ((direction (deg-to-rad (+ 180 (get-attribute 'direction Turtle))))
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

(define (cs)
  "Clears all data on current image"
  (gimp-selection-all (get-current-image))
  (gimp-edit-clear (get-current-layer))
  (gimp-selection-none (get-current-image)))

(define (show Turtle)
  "This command shoves a turtle shape onto the canvas, undo to remove it"
  (print "Press undo to remove this turtle, it is drawn to canvas")
  ; Impliment some special turtle displaying layer later maybe...
  ; Also look into those state saving and undo macros to make it go away
  (let ((pen-state (get-attribute 'drawing Turtle))
	(angle-state (get-attribute 'direction Turtle))
	(x (get-attribute 'x Turtle))
	(y (get-attribute 'y Turtle))
	(scale 24.0))
    (gimp-image-undo-group-start (get-current-image))
    (pu Turtle)
    (fd (* scale .625) Turtle)
    (pd Turtle)
    (rt (+ 180.0 26.5651) Turtle)
    (fd (* scale 1.118) Turtle)
    (lt (- 296.565 180.0) Turtle)
    (fd scale Turtle)
    (lt (- 296.565 180.0) Turtle)
    (fd (* scale 1.118) Turtle)
    (gimp-image-undo-group-end (get-current-image))
    (set-attribute! 'x x Turtle)
    (set-attribute! 'y y Turtle)
    (set-attribute! 'drawing pen-state Turtle)
    (set-attribute! 'direction angle-state Turtle)))

;; Absolute movement commands
(define (mv x y Turtle)
  "Move to position"
  (set-attribute! 'x x Turtle)
  (set-attribute! 'y y Turtle))

(define (lk angle Turtle)
  "Look and face an angle"
  (set-attribute! 'direction (angle-wrap angle) Turtle))

(define (home Turtle)
  "Move the Turtle to the center of the image and face up"
  (mv (/ (car (gimp-image-width (get-current-image))) 2.0)
      (/ (car (gimp-image-height (get-current-image))) 2.0)
      Turtle)
  (lk 90 Turtle))
