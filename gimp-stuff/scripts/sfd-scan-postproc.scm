;;; A4
;;; 72 dpi (web) = 595 X 842 pixels
;;; 300 dpi (print) = 2480 X 3508 pixels (This is "A4" as I know it, i.e. "210mm X 297mm @ 300 dpi")

;;; ver: 2016-01-09-2

(define QUAL-BW-75DPI 0)
(define QUAL-GREY-150DPI 1)
(define QUAL-COLOR-150DPI 2)
(define QUAL-COLOR-300DPI 3)
(define QUAL-COLOR-75DPI 4)
(define QUAL-GREY-300DPI 5)

;; a4 for canon lide - so that the right and bottom margins are visible
;; :fixme: not good for other scanners? make a parameter?
(define A4-WIDTH 2488)
(define A4-HEIGHT 3504)


(define (75dpi? qual)
  (or (= qual QUAL-BW-75DPI) (= qual QUAL-COLOR-75DPI)))


(define (150dpi? qual)
  (or (= qual QUAL-GREY-150DPI) (= qual QUAL-COLOR-150DPI)))


(define (coloured? qual)
  (or (= qual QUAL-COLOR-75DPI) (= qual QUAL-COLOR-150DPI) (= qual QUAL-COLOR-300DPI)))


(define (script-fu-sfd-scan-postproc image crop-style quality)

  (let* ((new-width (if (75dpi? quality) 595 (if (150dpi? quality) 1240 0)))
         ;; it is computed from the current aspect ratio
         (new-height 0)
         (scanner-offset-x 4)
         (scanner-offset-y 4))

    ;;Undo group start
    ;; :debug: comment for debug
    (gimp-image-undo-group-start image)

    ;; crop
    ;; :fixme: check overflows
    (when (= crop-style 1) ; start at 20x20
          (set! scanner-offset-x 20)
          (set! scanner-offset-y 10))
    (when (not (= crop-style 0))
          (gimp-image-crop image A4-WIDTH A4-HEIGHT scanner-offset-x scanner-offset-y))

    (gimp-levels-stretch (car (gimp-image-get-active-drawable image)))
    (plug-in-unsharp-mask 1 image (car (gimp-image-get-active-drawable image)) 3 0.3 1)

    (when (or (75dpi? quality) (150dpi? quality))
          (let ((drawable (car (gimp-image-get-active-drawable image))))
            (set! new-height (/ (* new-width (car (gimp-drawable-height drawable))) (car (gimp-drawable-width drawable))))
            (gimp-image-scale image new-width new-height)))

    (when (not (coloured? quality))
          (gimp-image-convert-grayscale image))

    (when (and (coloured? quality) (not (= quality QUAL-COLOR-300DPI)))
          ;;(gimp-image-convert-indexed image NO-DITHER WEB-PALETTE 0 FALSE TRUE ""))
          ;; dither methods: NO-DITHER, FS-DITHER, FSLOWBLEED-DITHER
          (gimp-image-convert-indexed image NO-DITHER MAKE-PALETTE 256 FALSE TRUE ""))

    ;; :debug: comment for debug
    (gimp-image-undo-group-end image)

    (gimp-displays-flush)))


(script-fu-register
 "script-fu-sfd-scan-postproc"
 "Postproc. scanned images - v1.0"
 "Export jpeg 75% for first option; 85% for 150dpi"
 "Tudor M. Pristavu"
 "Free for any purpose"
 "2016-01-01"
 "*"
 SF-IMAGE	"Input image"		 0
 SF-OPTION "Crop style" '("no crop" "start 20x10")
 SF-OPTION  "Quality" '("black&white, 75dpi, archive"
                        "grey, 150dpi, email"
                        "color, 150dpi, email"
                        "color, 300dpi, preserve details"
                        "color, 75dpi, archive"
                        "grey, 300dpi, preserve details"))


(script-fu-menu-register
 "script-fu-sfd-scan-postproc"
 _"<Image>/Filters/sfd/" )
