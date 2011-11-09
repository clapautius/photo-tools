;;; run from SLIME or make a shortcut for: 
;;;   (asdf:oos 'asdf:load-op 'digikam-db) or
;;;   (require 'digikam-db)

(defpackage #:digikam-db-system (:use #:cl #:asdf))
(in-package :digikam-db-system)

(require 'clsql-sqlite3)

(asdf:defsystem :digikam-db-tool
    :components ((:file "package")
                 (:file "digikam-db-tool"
                        :depends-on ("package"))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
