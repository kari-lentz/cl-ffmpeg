(asdf:defsystem #:cl-ffmpeg
  :description "wrapper for the ffmpeg libraries"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:asdf #:swank #:my-env #:my-db #:utility #:cffi)
  :components
  ((:module src
	    :components
	    ((:file "packages")
	     (:file "ffmpeg" :depends-on ("packages"))))))
