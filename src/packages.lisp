(defpackage :cffi-helper
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :cffi :my-env :utility)
  (:export :define-cffi-return-types :defcstruct* :defcfun* :zero-memory))

(defpackage :ring-buffer
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :cffi :bordeaux-threads :my-env :my-db :utility :cffi-helper)
  (:export :with-foreign-ring-buffer :user-eof :run))

(defpackage :cl-ffmpeg-inherit
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :my-env :utility)
  (:export :defstruct* :FFMPEG-ENV :EXPORT-P :RING-BUFFER :OUTPUT-BUFFER-SIZE :IN-DEVICE :OUT-DEVICE :MEDIA-TYPE :FFMPEG-ENV-EXPORT-P :FFMPEG-ENV-RING-BUFFER :FFMPEG-ENV-OUTPUT-BUFFER-SIZE :FFMPEG-ENV-IN-DEVICE :FFMPEG-ENV-OUT-DEVICE :FFMPEG-ENV-MEDIA-TYPE :AUDIO-PARAMS :SAMPLE-RATE :NUM-CHANNELS :AUDIO-PARAMS-SAMPLE-RATE :AUDIO-PARAMS-NUM-CHANNELS))

(defpackage :cl-ffmpeg
  (:documentation "front end for cl-ffmpeg")
  (:use :cl :cffi :my-env :my-db :utility :ring-buffer :cffi-helper :cl-ffmpeg-inherit)
  (:export :run-ffmpeg :run-ffmpeg-in :run-ffmpeg-out :with-audio-buffer :with-ffmpeg :run))

(defpackage :cl-alsa
  (:documentation "basic audio using the sdl mixer library (not available in lispbuilder)")
  (:use :cl :cffi :bordeaux-threads :lispbuilder-sdl :my-env :my-db :utility :ring-buffer :cffi-helper :cl-ffmpeg-inherit :cl-ffmpeg)
  (:export))

