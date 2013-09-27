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
       
(defcallback !sdl-audio-player :void
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

(defcallback sdl-audio-player :void
    ((user-data :pointer)
     (stream :pointer)
     (len :int))
  (declare (ignore user-data))
  (declare (ignore stream))  
  (block audio-block
    (handler-bind
	((condition (lambda(c) (format t "TRAPPED ERROR:~a~%" c) (return-from audio-block))))
      (format t "CALLBACK REQUEST FOR ~a audio bytes~%" len))))

(defun make-thread (function &key name arguments ephemeral)
  #+sb-doc
  "Create a new thread of NAME that runs FUNCTION with the argument
list designator provided (defaults to no argument). Thread exits when
the function returns. The return values of FUNCTION are kept around
and can be retrieved by JOIN-THREAD.

Invoking the initial ABORT restart estabilished by MAKE-THREAD
terminates the thread.

See also: RETURN-FROM-THREAD, ABORT-THREAD."
  #-sb-thread (declare (ignore function name arguments ephemeral))
  #-sb-thread (error "Not supported in unithread builds.")
  #+sb-thread (assert (or (atom arguments)
                           (null (cdr (last arguments))))
                       (arguments)
                       "Argument passed to ~S, ~S, is an improper list."
                       'make-thread arguments)
  #+sb-thread
  (let ((thread (sb-thread::%make-thread :name name :%ephemeral-p ephemeral)))
     (sb-thread:with-mutex (sb-thread::*make-thread-lock*)
       (let* ((setup-sem (sb-thread:make-semaphore :name "Thread setup semaphore"))
              (real-function (coerce function 'function))
              (arguments     (if (listp arguments)
                                 arguments
                                 (list arguments)))
              (initial-function
               (sb-ext::named-lambda initial-thread-function ()
                 ;; As it is, this lambda must not cons until we are ready
                 ;; to run GC. Be very careful.
                 (sb-thread::initial-thread-function-trampoline
                  thread setup-sem real-function arguments nil nil nil))))
         ;; If the starting thread is stopped for gc before it signals the
         ;; semaphore then we'd be stuck.
         (assert (not sb-thread::*gc-inhibit*))
         ;; Keep INITIAL-FUNCTION pinned until the child thread is
         ;; initialized properly. Wrap the whole thing in
         ;; WITHOUT-INTERRUPTS because we pass INITIAL-FUNCTION to another
         ;; thread.
         (sb-sys:without-interrupts
           (sb-sys:with-pinned-objects (initial-function)
             (if (zerop
                  (sb-thread::%create-thread (sb-thread::get-lisp-obj-address initial-function)))
                 (setf thread nil)
                 (sb-thread:wait-on-semaphore setup-sem))))))
     (or thread (error "Could not create a new thread."))))

(defmethod run-ffmpeg-out(audio-params (device-out sdl-audio-device))
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (with-slots (sample-rate num-channels output-buffer-size) audio-params
      (with-foreign-objects ((seq-num :int)(audio-spec '(:struct sdl-audio-spec)))
	(with-foreign-slots ((freq format channels samples callback userdata) audio-spec (:struct sdl-audio-spec))
	  
	  (format t "HOLY COW!!:~a~%" sample-rate)
	  (zero-memory audio-spec '(:struct sdl-audio-spec)) 
	  (setf freq sample-rate)
	  (setf format (foreign-enum-value 'sdl-audio-formats :audio-s16))
	  (setf channels num-channels)
	  (setf samples output-buffer-size)
	  (setf callback (callback sdl-audio-player))
	  (setf (mem-ref seq-num :int) (register-sdl-audio-device device-out))
	  (setf userdata seq-num)

	  (sdl-open-audio audio-spec (null-pointer))
       
	  ;(sdl-open-audio audio-spec (null-pointer))
	  (unwind-protect 
	       (progn
		 (sdl-pause-audio 0)
		 (with-slots (done-waiter wait-lock) device-out
		   (with-lock-held (wait-lock)
		     (condition-wait done-waiter wait-lock))))
	    (sdl-close-audio)))))))

(defun run-sdl(&optional (file-path "/mnt/MUSIC-THD/test.hd.mp4"))
  (run-ffmpeg (audio-params) (pathname file-path) (sdl-audio-device)))      
	       
