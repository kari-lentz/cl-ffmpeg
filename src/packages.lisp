(defpackage :ring-buffer
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :cffi :bordeaux-threads :my-env :my-db :utility)
  (:export :with-foreign-ring-buffer :with-foreign-ring-buffer :run))

(defpackage :cl-ffmpeg
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :cffi :my-env :my-db :utility :ring-buffer)
  (:export :run))

