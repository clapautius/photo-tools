;;; A4
;;; 72 dpi (web) = 595 X 842 pixels
;;; 300 dpi (print) = 2480 X 3508 pixels (This is "A4" as I know it, i.e. "210mm X 297mm @ 300 dpi")

(define (script-fu-sfd-scan-postproc image crop-style quality)

  (let* ((new-width (if (= quality 0) 595 (if (or (= quality 1) (= quality 2)) 1240 (if (= quality 3) 2480 0))))
         (new-height (if (= quality 0) 842 (if (or (= quality 1) (= quality 2)) 1754 (if (= quality 3) 3508 0))))
         (scanner-offset-x 4)
         (scanner-offset-y 4))

    ;;Undo group start
    (gimp-image-undo-group-start image)

    ;; :fixme: check overflows
    (when (not (= crop-style 0))
          (gimp-image-crop image 2480 3508 scanner-offset-x scanner-offset-y))

    (gimp-levels-stretch (car (gimp-image-get-active-drawable image)))
    (plug-in-unsharp-mask 1 image (car (gimp-image-get-active-drawable image)) 3 0.3 1)

    (when (or (= quality 0) (= quality 1) (= quality 2))
          (gimp-image-scale image new-width new-height))

    (when (or (= quality 0) (= quality 1))
          (gimp-image-convert-grayscale image))

    (when (= quality 2)
          (gimp-image-convert-indexed image NO-DITHER WEB-PALETTE 0 FALSE TRUE ""))

    (gimp-image-undo-group-end image)
    (gimp-displays-flush)))


(script-fu-register
 "script-fu-sfd-scan-postproc"
 "Postproc. scanned images - v1.0"
 "Export jpeg 75% for first option"
 "Tudor M. Pristavu"
 "Free for any purpose"
 "2016-01-01"
 "*"
 SF-IMAGE	"Input image"		 0
 SF-OPTION "Crop style" '("no crop" "canon lide specific crop")
 SF-OPTION  "Quality" '("black&white, 75dpi, archive" "grey, 150dpi, email" "color, 150dpi, email" "color, 300dpi, preserve details"))


(script-fu-menu-register
 "script-fu-sfd-scan-postproc"
 _"<Image>/Filters/sfd/" )
