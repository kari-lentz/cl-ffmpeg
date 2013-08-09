(defpackage :cffi-helper
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :cffi :my-env :utility)
  (:export :define-cffi-return-types :defcfun*))

(defpackage :ring-buffer
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :cffi :bordeaux-threads :my-env :my-db :utility :cffi-helper)
  (:export :with-foreign-ring-buffer :run))

(defpackage :cl-ffmpeg
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :cffi :my-env :my-db :utility :ring-buffer :cffi-helper)
  (:export :run))

