(asdf:defsystem #:cl-ffmpeg
  :description "wrapper for the ffmpeg libraries"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:asdf #:my-env #:my-db #:utility #:cffi #:bordeaux-threads)
  :components
  ((:module src
	    :components
	    ((:file "packages")
	     (:file "ring-buffer" :depends-on ("packages"))
	     (:file "ffmpeg" :depends-on ("ring-buffer"))))))
