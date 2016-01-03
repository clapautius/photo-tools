;;; A4
;;; 72 dpi (web) = 595 X 842 pixels
;;; 300 dpi (print) = 2480 X 3508 pixels (This is "A4" as I know it, i.e. "210mm X 297mm @ 300 dpi")

;;; ver: 2016-01-04-0

(define (script-fu-sfd-scan-postproc image crop-style quality)

  (let* (
         ;; a4 for canon lide - so that the right and bottom margins are visible
         ;; :fixme: not good for other scanners? make a parameter?
         (a4-width 2488) (a4-height 3504)
         (new-width (if (= quality 0) 595 (if (or (= quality 1) (= quality 2)) 1240 0)))
         ;; this is ignored - it is computed from the current aspect ratio
         (new-height (if (= quality 0) 842 (if (or (= quality 1) (= quality 2)) 1754 0)))
         (scanner-offset-x 4)
         (scanner-offset-y 4))

    ;;Undo group start
    ;; :debug: disabled for debug
    ;(gimp-image-undo-group-start image)

    ;; crop
    ;; :fixme: check overflows
    (when (= crop-style 1) ; start at 20x20
          (set! scanner-offset-x 20)
          (set! scanner-offset-y 10))
    (when (not (= crop-style 0))
          (gimp-image-crop image a4-width a4-height scanner-offset-x scanner-offset-y))

    (gimp-levels-stretch (car (gimp-image-get-active-drawable image)))
    (plug-in-unsharp-mask 1 image (car (gimp-image-get-active-drawable image)) 3 0.3 1)

    (when (or (= quality 0) (= quality 1) (= quality 2))
          (let ((drawable (car (gimp-image-get-active-drawable image))))
            (set! new-height (/ (* new-width (car (gimp-drawable-height drawable))) (car (gimp-drawable-width drawable))))
            (gimp-image-scale image new-width new-height)))

    (when (or (= quality 0) (= quality 1))
          (gimp-image-convert-grayscale image))

    (when (= quality 2)
          ;;(gimp-image-convert-indexed image NO-DITHER WEB-PALETTE 0 FALSE TRUE ""))
          ;; dither methods: NO-DITHER, FS-DITHER, FSLOWBLEED-DITHER
          (gimp-image-convert-indexed image NO-DITHER MAKE-PALETTE 256 FALSE TRUE ""))

    ;; :debug: disabled for debug
    ;(gimp-image-undo-group-end image)

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
 SF-OPTION  "Quality" '("black&white, 75dpi, archive" "grey, 150dpi, email" "color, 150dpi, email" "color, 300dpi, preserve details"))


(script-fu-menu-register
 "script-fu-sfd-scan-postproc"
 _"<Image>/Filters/sfd/" )
