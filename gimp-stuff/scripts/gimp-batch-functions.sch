; ver. 2017-04-27-0

(define (convert-xcf-to-jpeg filename outfile)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
	 (drawable (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE))))
    (file-jpeg-save RUN-NONINTERACTIVE image drawable outfile outfile .9 0 0 0 " " 0 1 0 1)
    (gimp-image-delete image))) ; ... or the memory will explode


(define (convert-xcf-to-png filename outfile)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
	 (drawable (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE))))
    (file-png-save RUN-NONINTERACTIVE image drawable outfile outfile 0 9 0 0 0 0 0)
    (gimp-image-delete image))) ; ... or the memory will explode


(define (convert-xcf-to-jpeg-resize filename outfile new-size)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
         (width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))
         (aspect (/ height width)))
    (gimp-context-set-interpolation INTERPOLATION-LANCZOS)
    (if (< width height)
        (begin
          (gimp-image-scale image (/ new-size aspect) new-size))
        (begin
          (gimp-image-scale image new-size (* new-size aspect))))
    (let ((drawable (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE))))
      (file-jpeg-save RUN-NONINTERACTIVE image drawable outfile outfile
                      .95; quality [0, 1]
                      0  ; smoothing [0, 1]
                      0  ; optimize - Optimization of entropy encoding parameters (0/1)
                      0  ; progressive - Enable progressive jpeg image loading (0/1)
                      " "; comment
                      0  ; subsampling - The subsampling option number
                      1  ; baseline - Force creation of a baseline JPEG
                         ;   (non-baseline JPEGs can't be read by all decoders) (0/1)
                      0  ; restart - Interval of restart markers
                         ;   (in MCU rows, 0 = no restart markers)
                      1  ; DCT algorithm to use (speed/quality tradeoff)
                      ))
    (gimp-image-delete image))) ; ... or the memory will explode


(gimp-message-set-handler 1) ; Messages to standard output


;;; Returns TRUE if OK, FALSE on error.
(define (postproc-scan-img-and-save-jpeg filename outfile rotate-right extra-crop quality)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
         (drawable (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE))))
    ;;(script-fu-sfd-scan-postproc image crop-style quality skip-autolevels dont-group-undo)
    ;; crop-style : 2 (off: 20x4)
    ;; skip-autolevels : TRUE
    ;; dont-group-undo : FALSE
    (script-fu-sfd-scan-postproc image 2 quality TRUE FALSE)
    (when extra-crop
          ;; crop 40x40px (e.g. notebook is smaller than A4)
          (let* ((orig-width (car (gimp-image-width image)))
                 (orig-height (car (gimp-image-height image))))
            (gimp-image-crop image (- orig-width 40) (- orig-height 40) 0 0)))
    (when rotate-right
          (gimp-image-rotate image ROTATE-90))
    (catch
     (begin (gimp-message "Error saving jpeg file!") FALSE)
     (file-jpeg-save RUN-NONINTERACTIVE image drawable outfile outfile .70 0 0 0 " " 0 1 0 1)
     (gimp-image-delete image) ; ... or the memory will explode
     TRUE)))
