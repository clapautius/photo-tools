;;; browser independent classes and functions
(in-package :digikam-db-tool)


;;; debug flag
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dk-debug* nil)
  (defparameter *dk-trace-sql* nil))


;;; bookmark class - browser independent
(defclass image ()
  ((fname :accessor fname
        :initarg :fname
        :initform (error "Must specify an URL"))

   (id :accessor id
       :initarg :id
       :initform nil)

   (comment :accessor comment
            :initarg :comment
            :initform nil)

   ;; list of tags
   (tags :accessor tags
         :initarg :tags
         :initform nil))

  (:documentation "Class containing basic elements for an image file"))


(defmethod print-object ((image image) stream)
  "Print the image object to the specified stream"
  (format stream "<img: ~a, id=~a~%  comm=~a~%>" (fname image) (id image)
          (comment image)))


(defun open-db (&optional (path "digikam4.db"))
  "Open an sqlite connection to the specified file."
  (clsql:connect (list path) :database-type :sqlite3 :if-exists :old))


(defun close-db ()
  "Close the sqlite connection"
  (clsql:disconnect))


(defun load-image-by-name (name)
  "Load image properties."
  (let ((query (format nil "select i.id, comment from Images i, ImageComments c
where i.id=c.imageid and name='~a'" name)))
    (when *dk-trace-sql*
      (format t "query: ~a~%" query))
    (let ((imgs (clsql:query query :field-names nil)))
      (when *dk-trace-sql*
        (format t "result: ~a~%" imgs))
      (cond
        ;; more then one image found - not good
        ((> (length imgs) 1)
         (error "Too many images with the same name. Check your database."))
        ;; exactly one image found - good
        ((= 1 (length imgs))
         (let ((img (first imgs)))
           (make-instance 'image :fname name :id (first img)
                          :comment (second img))))
        ;; return nil if no image found by name
        (t nil))))) 

(defun restore-comments-from-old-db (db-path imgs-file)
  "Read a list of image filenames, load the comments for the images from db-path
  database and print them to stdout. Use *standard-output* for redirection.
   This function is useful for restoring lost comments from backup db."
;;; to redirect stdout use:
;;;(with-open-file (*standard-output* "somefile.dat" :direction :output :if-exists :supersede)
;;;  ...)

  (open-db db-path)
  (with-open-file (stream imgs-file)
    (loop for line = (read-line stream nil 'done)
       until (eq line 'done)
       do (format t "~a ~a~%" line (comment (load-image-by-name line)))))
  (close-db))
