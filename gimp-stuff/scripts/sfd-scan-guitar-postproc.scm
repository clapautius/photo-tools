;;; Perform a series of operations on scanned guitar documents.
;;; ver: 2020-01-30-0

(define RESIZE-PERCENT 66)
(define NO-COLORS 16)

(define (script-fu-sfd-scan-guitar-postproc image)

  ;;Undo group start
  ;; :debug: comment for debug
  ;;(when (= dont-group-undo FALSE)
  ;;      (gimp-image-undo-group-start image))

  ;; rotate right
  (gimp-image-rotate image ROTATE-90)

  ;; levels
  (if TRUE
      (gimp-message "foo")
      (let* ((clamp-input FALSE) (clamp-output FALSE)
         (image-drw (car (gimp-image-get-active-drawable image))))
    ;; VALUE
    (let* ((low-input 0.166667) (high-input 0.945946) (gamma 1.000000)
           (low-output 0.000000) (high-output 1.000000))
      (gimp-drawable-levels image-drw HISTOGRAM-VALUE low-input high-input clamp-input
                            gamma low-output high-output clamp-output))
    ;; RED
    (let* ((low-input 0.000000) (high-input 0.806020) (gamma 1.000000)
           (low-output 0.000000) (high-output 1.000000))
      (gimp-drawable-levels image-drw HISTOGRAM-RED low-input high-input clamp-input
                            gamma low-output high-output clamp-output))
    ;; GREEN
    (let* ((low-input 0.000000) (high-input 0.815545) (gamma 1.000000)
           (low-output 0.000000) (high-output 1.000000))
      (gimp-drawable-levels image-drw HISTOGRAM-GREEN low-input high-input clamp-input
                            gamma low-output high-output clamp-output))
    ;; BLUE
    (let* ((low-input 0.000000) (high-input 0.819231) (gamma 1.000000)
           (low-output 0.000000) (high-output 1.000000))
      (gimp-drawable-levels image-drw HISTOGRAM-BLUE low-input high-input clamp-input
                            gamma low-output high-output clamp-output))
    ;; ALPHA
    (let* ((low-input 0.000000) (high-input 1.000000) (gamma 1.000000)
           (low-output 0.000000) (high-output 1.000000))
      (gimp-drawable-levels image-drw HISTOGRAM-ALPHA low-input high-input clamp-input
                            gamma low-output high-output clamp-output))))

  #;(let* ((image-drw (car (gimp-image-get-active-drawable image))))
    (gimp-levels image-drw HISTOGRAM-VALUE 42 242 1.0 0 255)
    (gimp-levels image-drw HISTOGRAM-RED 0 206 1.0 0 255)
    (gimp-levels image-drw HISTOGRAM-GREEN 0 208 1.0 0 255)
    (gimp-levels image-drw HISTOGRAM-BLUE 0 209 1.0 0 255))

  ;;(gimp-image-convert-indexed image NO-DITHER WEB-PALETTE 0 FALSE TRUE ""))
  ;; dither methods: NO-DITHER, FS-DITHER, FSLOWBLEED-DITHER
  ;(gimp-image-convert-indexed image NO-DITHER MAKE-PALETTE 16 FALSE TRUE "")

  ;; :debug: comment for debug
  ;;(when (= dont-group-undo FALSE)
  ;;      (gimp-image-undo-group-end image))

  (gimp-displays-flush))


(script-fu-register
 "script-fu-sfd-scan-guitar-postproc"
 "Postproc. scanned guitar images - v0.1"
 "Export png"
 "Tudor M. Pristavu"
 "Free for any purpose"
 "2020-01-30"
 "*"
 SF-IMAGE	"Input image"		 0
)


(script-fu-menu-register
 "script-fu-sfd-scan-guitar-postproc"
 _"<Image>/Filters/sfd/" )
