(in-package :cl-sdl-audio)

(defmacro defctypes (type-specs)
  `(progn
     ,@(loop for (name base-type &optional documentation) in type-specs
	  collecting
	    `(defctype ,name ,base-type ,documentation))))

(define-foreign-library libSDL
  (:unix (:or "libSDL.so"))
  (t (:default "SDL.so")))

(use-foreign-library libSDL)

(defctypes ((uint16 :ushort)
	    (uint8 :uchar)
	    (uint32 :ulong)))

(defcfun ("SDL_GetError" sdl-get-error) :string)

(define-condition sdl-fault(error)
  ((msg :initarg :msg :reader msg)
   (code :initarg :code :reader code))
  (:report 
   (lambda(c stream)
     (let ((error-str (foreign-string-to-lisp (sdl-get-error))))
       (format stream (join " "  
			    (append (list "sdl" "lib:" (msg c)) 
				    (when (code c)
				      (list "return code:" (code c))) 
				    (list "error:" error-str))))))))
 			
(defmacro sdl-assert(lisp-name function &body args)
  (with-gensyms (ret)
    `(let ((,ret (,function ,@args)))
       (unless (= ,ret 0)
	 (error 'sdl-fault :msg (% "~a" (list ,(symbol-name lisp-name) ,@args)) :code ,ret))
       ,ret)))

(define-cffi-return-types
    ((:sdl-int :int sdl-assert)))

(defcenum sdl-audio-formats 
  (:audio-s16 #x8010))

(defcstruct* sdl-audio-spec
  (freq :int)
  (format uint16)
  (channels uint8)
  (silence uint8)
  (samples uint16)
  (size uint32)
  (callback :pointer)
  (userdata :pointer))
  
;int SDL_OpenAudio(SDL_AudioSpec *desired, SDL_AudioSpec *obtained);

(defcfun* ("SDL_OpenAudio" sdl-open-audio) :sdl-int 
  (desired-spec :pointer)
  (obtained-spec :pointer))

(defcfun ("SDL_CloseAudio" sdl-close-audio) :void)

(defcfun ("SDL_PauseAudio" sdl-pause-audio) :void
  (pause-on :int))

(defparameter *sdl-audio-devices* (make-hash-table))
(defparameter *sdl-audio-device-seq-num* 0)

(defstruct (sdl-audio-device 
	     (:constructor sdl-audio-device 
			   (&optional 
			    (audio-params (audio-params)) 
			    (done-waiter (make-condition-variable :name "sdl-audio-wait-done")) 
			    (wait-lock (make-lock "sdl-wait-lock"))))) 
  audio-params done-waiter wait-lock)

(defun register-sdl-audio-device(sdl-audio-device)
  (setf (gethash *sdl-audio-device-seq-num* *sdl-audio-devices*) sdl-audio-device)
  (post-fix
   *sdl-audio-device-seq-num*
   (incf *sdl-audio-device-seq-num*)))

(defun get-sdl-audio-device(seq-num)
  (aif (gethash seq-num *sdl-audio-devices*)
       it
       (error "SDL AUDIO SEQUENCE NUMBER ~a NOT FOUND~%" seq-num)))
       
(defcallback sdl-audio-player :void
    ((user-data :pointer)
     (stream :pointer)
     (len :int))
  (block audio-block
    (handler-bind
	((condition (lambda(c) (format t "~a~%" c) (return-from audio-block))))
      (format t "ASKING FOR ~a audio bytes~%" len)
      (with-slots (audio-params done-waiter wait-lock) (get-sdl-audio-device (mem-ref user-data :int))
	(with-slots (ring-buffer num-channels) audio-params
	  (let ((requested-samples (/ len num-channels 2)))
	    (let ((ret (funcall ring-buffer :read stream requested-samples)))
	      (when (< ret requested-samples)
		(with-lock-held (wait-lock)
		  (condition-notify done-waiter))))))))))

(defmethod run-ffmpeg-out(audio-params (device-out sdl-audio-device))
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (with-slots (sample-rate num-channels output-buffer-size) audio-params
      (with-foreign-objects ((seq-num :int)(audio-spec '(:struct sdl-audio-spec)))
	(with-foreign-slots ((freq format channels samples callback userdata) audio-spec (:struct sdl-audio-spec))
	  
	  (format t "HOLY COW!!:~a~%" sample-rate)

	  (setf freq sample-rate)
	  (setf format (foreign-enum-value 'sdl-audio-formats :audio-s16))
	  (setf channels num-channels)
	  (setf samples output-buffer-size)
	  (setf callback (callback sdl-audio-player))
	  (setf (mem-ref seq-num :int) (register-sdl-audio-device device-out))
	  (setf userdata seq-num)
	
	  (sdl-open-audio audio-spec (null-pointer))
	  (unwind-protect 
	       (progn
		 (sdl-pause-audio 0)
		 (with-slots (done-waiter wait-lock) device-out
		   (with-lock-held (wait-lock)
		     (condition-wait done-waiter wait-lock))))
	    (sdl-close-audio)))))))

(defun run-sdl(&optional (file-path "/mnt/MUSIC-THD/test.hd.mp4"))
  (run-ffmpeg (audio-params) (pathname file-path) (sdl-audio-device)))      
	       
