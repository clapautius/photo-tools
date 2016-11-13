;; used to process scanned pages from journal (A5 copybook)

(gimp-message-set-handler 1) ; Messages to standard output

(define (postproc-scan-journal-img-and-save-jpeg filename outfile)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
         (drawable (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE))))
    (gimp-curves-spline drawable HISTOGRAM-VALUE 4 #(50 0 234 255))
    ;;(script-fu-sfd-scan-postproc image crop-style quality skip-autolevels dont-group-undo)
    ;; crop-style : 2 (off: 20x4)
    ;; skip-autolevels : TRUE
    ;; dont-group-undo : FALSE
    (script-fu-sfd-scan-postproc image 2 QUAL-GREY-300DPI TRUE FALSE)
    (gimp-image-rotate image ROTATE-90)
    ; orig. size: 2556x3543
    (gimp-image-crop image 3480 2472 24 0)
    ; scale - aspect = 1.4045307
    ; scale to 80%
    (gimp-image-scale image 2800 1994)
    (file-jpeg-save RUN-NONINTERACTIVE image drawable outfile outfile .80 0 0 0 " " 0 1 0 1)
    (gimp-image-delete image))) ; ... or the memory will explode
