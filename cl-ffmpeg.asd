(asdf:defsystem #:cl-ffmpeg
  :description "wrapper for the ffmpeg libraries"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:asdf #:my-env #:my-db #:utility #:cffi #:bordeaux-threads #:lispbuilder-sdl)
  :components
  ((:module src
	    :components
	    ((:file "packages")
	     (:file "media-time" :depends-on ("packages"))
	     (:file "cffi-helper" :depends-on ("packages"))
	     (:file "ring-buffer" :depends-on ("media-time" "cffi-helper" "packages"))
	     (:file "ffmpeg-cffi" :depends-on ("media-time" "cffi-helper" "packages"))
	     (:file "inherit" :depends-on ("packages"))
	     (:file "ffmpeg" :depends-on ("ring-buffer" "inherit" "ffmpeg-cffi"))
	     (:file "alsa" :depends-on ("ffmpeg" "inherit" "ffmpeg-cffi" "ring-buffer" "cffi-helper" "packages"))
	     (:file "keysym" :depends-on ("packages"))
	     (:file "sdl" :depends-on ("ffmpeg" "keysym" "inherit" "ffmpeg-cffi" "ring-buffer" "cffi-helper" "packages"))))))
