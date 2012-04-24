(define (script-fu-white-border-sfd image use-default use-xtr
									black-width-x black-width-y
									white-width-x white-width-y
									shadow-width
									xtr-w-x xtr-w-y
									show-params)
  (let* ((img-w (car (gimp-image-width image))) ;; current image width
		 (img-h (car (gimp-image-height image))) ;; current image height 
		 (img-max (max img-w img-h)))
	(if (= use-default TRUE)
		(begin
		  (set! black-width-x
				(floor (+ (/ img-max 1000) (if (< img-max 1000) 1 0))))
		  (set! black-width-y
				(floor (+ (/ img-max 1000) (if (< img-max 1000) 1 0))))
		  (set! white-width-x (floor (* (/ img-max 100) 2)))
		  (set! white-width-y (floor (* (/ img-max 100) 2)))
		  (set! shadow-width (floor (/ img-max 100)))
		  (if (= use-xtr TRUE)
			  (begin
				(set! xtr-w-x (floor (* (/ img-max 100) 1.2)))
				(set! xtr-w-y (floor (* (/ img-max 100) 1.2))))
			  (begin
				(set! xtr-w-x 0)
				(set! xtr-w-y 0)))))
	(let* (
		   ;; no. of pixels to be added to the image (sum of all borders)
		   (img-supp-x (+ black-width-x white-width-x
						  shadow-width xtr-w-x))
		   (img-supp-y (+ black-width-y white-width-y
						  shadow-width xtr-w-y))
		   
		   ;; final image size
		   (img-w-final (+ img-w (* 2 img-supp-x)))
		   (img-h-final (+ img-h (* 2 img-supp-y)))
		   
		   ;; previous settings
		   (for-color (car (gimp-context-get-foreground)))
		   
		   ;; new layer (for borders)
		   (new-layer (car (gimp-layer-new image img-w-final img-h-final
										   RGB-IMAGE "border" 100 NORMAL-MODE)))
		   ) ; end init. variables (let*)

	  ;;Undo group start
	  (gimp-image-undo-group-start image)
	  
	  (gimp-image-resize image img-w-final img-h-final img-supp-x img-supp-y)
	  
	  (gimp-context-set-foreground '(255 255 255)) ; set white foreground
	  (gimp-drawable-fill new-layer FOREGROUND-FILL)
	  
	  (gimp-image-add-layer image new-layer
							(car (gimp-image-get-layers image)))
	  
	  ;; draw shadow
	  (let* ((small-shadow-width (ceiling (/ shadow-width 3))))
		(if (= show-params TRUE)
			(gimp-message (string-append "black-width-x="
										 (number->string black-width-x)
										 ", white-width-x="
										 (number->string white-width-x)
										 ", shadow-width="
										 (number->string shadow-width)
										 ", small-shadow-width="
										 (number->string small-shadow-width)
										 ", extra-width-x="
										 (number->string xtr-w-x))))
		;; fill with grey
		(gimp-context-set-foreground '(190 190 190))
		;; small shadow
		(gimp-rect-select image
						  (+ xtr-w-x (- shadow-width (/ small-shadow-width 2)))
						  (+ xtr-w-y (- shadow-width (/ small-shadow-width 2)))
						  (+ img-w (* 2 black-width-x) (* 2 white-width-x)
							 small-shadow-width)
						  (+ img-h (* 2 black-width-y) (* 2 white-width-y)
							 small-shadow-width)
						  CHANNEL-OP-REPLACE TRUE small-shadow-width)
		(gimp-edit-fill new-layer FOREGROUND-FILL)
		;; big shadow - slightly darker grey (only right and bottom)
		(gimp-context-set-foreground '(140 140 140))
		(gimp-rect-select image
						  (+ xtr-w-x shadow-width shadow-width)
						  (+ xtr-w-y shadow-width shadow-width)
						  (+ img-w (* 2 black-width-x) (* 2 white-width-x)
							 (- shadow-width) (/ shadow-width 2))
						  (+ img-h (* 2 black-width-y) (* 2 white-width-y)
							 (- shadow-width) (/ shadow-width 2))
						  CHANNEL-OP-REPLACE TRUE shadow-width)
		(gimp-edit-fill new-layer FOREGROUND-FILL)
		(gimp-selection-none image))
	  
	  ;; draw white
	  (gimp-context-set-foreground '(255 255 255))
	  (gimp-rect-select image
						(+ xtr-w-x shadow-width)
						(+ xtr-w-y shadow-width)
						(+ img-w (* 2 black-width-x) (* 2 white-width-x))
						(+ img-h (* 2 black-width-y) (* 2 white-width-y))
						CHANNEL-OP-REPLACE FALSE 0)
	  (gimp-edit-fill new-layer FOREGROUND-FILL)
	  (gimp-selection-none image)
	  
	  ;; draw black
	  (gimp-context-set-foreground '(0 0 0))
	  (gimp-rect-select image
						(+ xtr-w-x shadow-width white-width-x)
						(+ xtr-w-y shadow-width white-width-y)
						(+ img-w (* 2 black-width-x))
						(+ img-h (* 2 black-width-y))
						CHANNEL-OP-REPLACE FALSE 0)
	  (gimp-edit-fill new-layer FOREGROUND-FILL)
	  (gimp-selection-none image)
	  
	  ;; restore prev. settings, end undo, etc.
	  (gimp-context-set-foreground for-color)
	  (gimp-image-undo-group-end image)
	  (gimp-displays-flush))))


(script-fu-register
 "script-fu-white-border-sfd"
 "White border (SFD) v0.2"
 "White borders"
 "Tudor M. Pristavu"
 "Free for any purpose"
 "2011-06-28"
 "*"
 SF-IMAGE	"Input image"		 0
 SF-TOGGLE "Use default params." TRUE
 SF-TOGGLE "Use extra white border." FALSE
 SF-ADJUSTMENT "Black width x"  '(2 0 256 1 1 0)
 SF-ADJUSTMENT "Black width y"  '(2 0 256 1 1 0)
 SF-ADJUSTMENT "White width x"  '(20 0 256 1 1 0)
 SF-ADJUSTMENT "White width y"  '(20 0 256 1 1 0)
 SF-ADJUSTMENT "Shadow width"  '(16 0 256 1 1 0)
 SF-ADJUSTMENT "Extra width x"  '(0 0 256 1 1 0)
 SF-ADJUSTMENT "Extra width y"  '(0 0 256 1 1 0)
 SF-TOGGLE "Show params." FALSE)


(script-fu-menu-register
 "script-fu-white-border-sfd"
 _"<Image>/Filters/" )
