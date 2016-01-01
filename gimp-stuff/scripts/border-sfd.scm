(define (border-sfd-draw-border image layer border-color border-off-x
								border-off-y border-width border-height fuzzy)
  (let* ((img-w (car (gimp-image-width image)))
		 (img-h (car (gimp-image-height image)))
		 (fuzzy-size (+ (/ (+ border-width border-height) 4) 1)))
	;; feather size = half of the median value of border-width & border-height plus 1
	
	;; first selection
	(gimp-rect-select image border-off-x border-off-y
					  (- img-w (* 2 border-off-x))
					  (- img-h (* 2 border-off-y))
					  CHANNEL-OP-REPLACE FALSE 0)
	;; second selection
	(gimp-rect-select image (+ border-off-x border-width)
					  (+ border-off-y border-height)
					  (- img-w (* 2 (+ border-off-x border-width)))
					  (- img-h (* 2 (+ border-off-y border-height)))
					  CHANNEL-OP-SUBTRACT fuzzy fuzzy-size)
	
	(gimp-context-set-foreground border-color)
	(gimp-edit-fill layer FOREGROUND-FILL)
	(gimp-selection-none image)))


(define (script-fu-border-sfd image border1-color border1 border1-fuz
							  border2-color border2 border2-fuz
							  border3-color border3 border3-fuz
							  border4-color border4 border4-fuz)
  (let* ((border1-w border1) (border1-h border1)
		 (border2-w border2) (border2-h border2)
		 (border3-w border3) (border3-h border3)
		 (border4-w border4) (border4-h border4)
		 (border2-off-x border1-w)
		 (border2-off-y border1-h)
		 (border3-off-x (+ border2-off-x border2-w))
		 (border3-off-y (+ border2-off-y border2-h))
		 (border4-off-x (+ border3-off-x border3-w))
		 (border4-off-y (+ border3-off-y border3-h))

		 ;; current image size
		 (img-w (car (gimp-image-width image)))
		 (img-h (car (gimp-image-height image)))
		 
		 ;; no. of pixels to be added to the image (sum of all borders)
		 (img-supp-x (+ border1-w border2-w border3-w border4-w))
		 (img-supp-y (+ border1-h border2-h border3-h border4-h))
		 
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
	
	(gimp-context-set-foreground border1-color)
	(gimp-drawable-fill new-layer FOREGROUND-FILL)
	
	(gimp-image-add-layer image new-layer
						  (car (gimp-image-get-layers image)))
	
	(when (> border1 0)
		  (border-sfd-draw-border image new-layer border1-color 0 0
								  border1-w border1-h border1-fuz))
	(when (> border2 0)
		  (border-sfd-draw-border image new-layer border2-color border2-off-x
								  border2-off-y border2-w border2-h border2-fuz))
	(when (> border3 0)
		  (border-sfd-draw-border image new-layer border3-color border3-off-x
								  border3-off-y border3-w border3-h border3-fuz))
	(when (> border4 0)
		  (border-sfd-draw-border image new-layer border4-color border4-off-x
								  border4-off-y border4-w border4-h border4-fuz))
	
	;; restore prev. settings, end undo, etc.
	(gimp-context-set-foreground for-color)
	(gimp-image-undo-group-end image)
	(gimp-displays-flush)))


(script-fu-register
 "script-fu-border-sfd"
 "Borders (SFD) v0.2"
 "Borders"
 "Tudor M. Pristavu"
 "Free for any purpose"
 "2010-09-22"
 "*"
 SF-IMAGE	"Input image"		 0
 SF-COLOR	"Border 1 color"	 '(208 208 208)
 SF-ADJUSTMENT "Border 1 width"  '(12 0 256 1 1 0)
 SF-TOGGLE	"Border 1 fuzzy"	 FALSE
 SF-COLOR   "Border 2 color"     '(32 32 32) ; 202020
 SF-ADJUSTMENT "Border 2 width"  '(2 0 256 1 1 0)
 SF-TOGGLE	"Border 2 fuzzy"	 FALSE
 SF-COLOR	"Border 3 color"	 '(128 128 128) ; 808080
 SF-ADJUSTMENT "Border 3 width"  '(2 0 256 1 1 0)
 SF-TOGGLE	"Border 3 fuzzy"	 TRUE
 SF-COLOR	"Border 4 color"	 '(208 208 208)
 SF-ADJUSTMENT "Border 4 width"  '(2 0 256 1 1 0)
 SF-TOGGLE	"Border 4 fuzzy"	 FALSE
 )


(script-fu-menu-register
"script-fu-border-sfd"
_"<Image>/Filters/sfd/"
)
