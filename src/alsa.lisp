(in-package :cl-alsa)

(defmacro defctypes (type-specs)
  `(progn
     ,@(loop for (name base-type &optional documentation) in type-specs
	  collecting
	    `(defctype ,name ,base-type ,documentation))))

(define-foreign-library libasound
  (:unix (:or "libasound.so"))
  (t (:default "libasound.so")))

(use-foreign-library libasound)

(defctypes ((uint16 :ushort)
	    (uint8 :uchar)
	    (uint32 :ulong)))

(defcfun snd-strerror :string
  (err-num :int))

(define-condition alsa-fault(error)
  ((msg :initarg :msg :reader msg)
   (code :initarg :code :reader code))
  (:report 
   (lambda(c stream)
     (let ((error-str (snd-strerror (code c))))
       (format stream (join " "  
			    (append (list "sdl" "lib:" (msg c)) 
				    (when (code c)
				      (list "return code:" (code c))) 
				    (list "error:" error-str))))))))
 			
(defmacro alsa-assert(lisp-name function &body args)
  (with-gensyms (ret)
    `(let ((,ret (,function ,@args)))
       (unless (= ,ret 0)
	 (error 'alsa-fault :msg (% "~a" (list ,(symbol-name lisp-name) ,@args)) :code ,ret))
       ,ret)))

(defcfun snd-output-stdio-attach :int
  (outputp :pointer)
  (fp :pointer)
  (close-flag :int))

(define-cffi-return-types
    ((:alsa-int :int alsa-assert)))

(defcenum snd-pcm-stream-t
  (:snd-pcm-stream-playback 0)
  :snd-pcm-stream-capture)

(defcfun* snd-pcm-open :alsa-int
  (pcmp :pointer) 
  (name :string)
  (stream snd-pcm-stream-t)
  (mode :int))

(defcfun* snd-pcm-close :alsa-int
  (pcm :pointer))

(defcfun* snd-pcm-hw-params-malloc :alsa-int
  (double-ptr :pointer))

(defcfun* snd-pcm-hw-params-any :alsa-int
  (pcm :pointer)
  (params :pointer))

(defcenum snd-pcm-access-t 
  (:SND-PCM-ACCESS-MMAP-INTERLEAVED 0)
  :SND-PCM-ACCESS-MMAP-NONINTERLEAVED 
  :SND-PCM-ACCESS-MMAP-COMPLEX 
  :SND-PCM-ACCESS-RW-INTERLEAVED 
  :SND-PCM-ACCESS-RW-NONINTERLEAVED) 

(defcenum snd-pcm-format-t
  (:SND-PCM-FORMAT-UNKNOWN  -1) 
  (:SND-PCM-FORMAT-S8 0) 
  :SND-PCM-FORMAT-U8 
  :SND-PCM-FORMAT-S16-LE) 

(defcfun* snd-pcm-hw-params-set-access :alsa-int
  (pcm :pointer)
  (params :pointer)
  (access snd-pcm-access-t))

(defcfun* snd-pcm-hw-params-set-format :alsa-int
  (pcm :pointer)
  (params :pointer)
  (format snd-pcm-format-t))

(defcfun* snd-pcm-hw-params-set-rate :alsa-int
  (pcm :pointer)
  (params :pointer)
  (val :int)
  (dir :int))

(defcfun* snd-pcm-hw-params-set-channels :alsa-int
  (pcm :pointer)
  (params :pointer)
  (val :unsigned-int))

(defcfun* snd-pcm-hw-params-set-buffer-size-near :int
  (pcm :pointer)
  (params :pointer)
  (val :pointer))

(defcfun* snd-pcm-hw-params-set-period-size-near :alsa-int
  (pcm :pointer)
  (params :pointer)
  (val :pointer))
  
(defcfun* snd-pcm-hw-params :alsa-int
  (pcm :pointer)
  (params :pointer))

(defcfun snd-pcm-hw-params-free :void
  (obj :pointer))

(defcfun* snd-pcm-sw-params-malloc :alsa-int
  (double-pointer :pointer))

(defcfun* snd-pcm-sw-params-current :alsa-int
  (pcm :pointer)
  (params :pointer))

(defctype snd-pcm-uframes-t :unsigned-long)

(defcfun* snd-pcm-sw-params-set-start-threshold :alsa-int
  (pcm :pointer) 
  (params :pointer)
  (val :unsigned-long))

(defcfun* snd-pcm-sw-params-set-avail-min :alsa-int
  (pcm :pointer)
  (params :pointer)
  (val snd-pcm-uframes-t))

(defcfun* snd-pcm-sw-params :alsa-int
  (pcm :pointer)
  (params :pointer))

(defcfun snd-pcm-sw-params-free :void
  (obj :pointer))

(defmacro with-null-pointer(ptr &body body)
  `(with-foreign-object (,ptr :pointer)
     (setf (mem-ref ,ptr :pointer) (null-pointer))
     ,@body))

(defmacro with-open-alsa-device((pcm  &key (device-name "default")) &body body)
  (with-gensyms (ppcm)
    `(with-null-pointer ,ppcm
       (snd-pcm-open ,ppcm ,device-name :snd-pcm-stream-playback 0)
       (let ((,pcm (mem-ref ,ppcm :pointer)))
	 (unwind-protect
	      (progn
		,@body)
	   (snd-pcm-close ,pcm))))))

(defstruct* (alsa-params (:constructor alsa-params (&key (access :SND-PCM-ACCESS-RW-INTERLEAVED) (format :SND-PCM-FORMAT-S16-LE)(buffer-size 4096)(num-periods 4))) (:include audio-params)) 
    access format buffer-size num-periods)

(define-condition alsa-buffer-fault(alsa-fault)
  ((actual-buffer-size :initarg :actual-buffer-size :reader actual-buffer-size)
   (requested-buffer-size :initarg :requested-buffer-size :reader requested-buffer-size))
  (:report (lambda(c stream) (format stream "requested buffer size ~a but got size ~a" (requested-buffer-size c) (actual-buffer-size c))))) 

(defun set-params(pcm hw-params alsa-params)
  (macrolet ((hw-set(param &rest values)
	       `(,(.sym 'snd-pcm-hw-params-set- param) pcm hw-params ,@values)))
    (with-slots (access format sample-rate num-channels buffer-size) alsa-params
      (hw-set access access)
      (hw-set format format)
      (hw-set rate sample-rate 0)
      (hw-set channels num-channels)
      (tagbody try-buffer
	 (with-foreign-object (pbuffer-size 'snd-pcm-uframes-t)
	   (setf (mem-ref pbuffer-size 'snd-pcm-uframes-t) buffer-size)
	   (restart-case
	       (progn
		(hw-set buffer-size-near pbuffer-size)
		(let ((actual-buffer-size (mem-ref pbuffer-size 'snd-pcm-uframes-t)))
		  (unless (= actual-buffer-size buffer-size)
		    (error 'alsa-buffer-fault :requested-buffer-size buffer-size :actual-buffer-size actual-buffer-size))))
	       (accept-buffer())
	       (try-new-size(new-buffer-size)
		 (setf buffer-size new-buffer-size)
		 (go try-buffer)))))))))

(defmacro with-hw-params((pcm hw-params) &body body)
  (with-gensyms (phw-params)
    `(with-null-pointer ,phw-params
       (snd-pcm-hw-params-malloc ,phw-params)
       (let ((,hw-params (mem-ref ,phw-params :pointer)))
	 (unwind-protect
	      (progn
		(snd-pcm-hw-params-any ,pcm ,hw-params)
		,@body)
	   (snd-pcm-hw-params-free ,hw-params))))))

(defun test-alsa(&key (device-name "default"))
  (with-open-alsa-device (pcm :device-name device-name)
    (with-hw-params (pcm hw-params)
      (set-params pcm hw-params (alsa-params))
      (format t "ALSA:~a:~a~%" pcm hw-params))))    

;; (defmethod run-ffmpeg-out(audio-params (device-out sdl-audio-device))
;;   (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
;;     (with-slots (sample-rate num-channels output-buffer-size) audio-params
;;       (with-foreign-objects ((seq-num :int)(audio-spec '(:struct sdl-audio-spec)))
;; 	(with-foreign-slots ((freq format channels samples callback userdata) audio-spec (:struct sdl-audio-spec))
	  
;; 	  (format t "HOLY COW!!:~a~%" sample-rate)
;; 	  (zero-memory audio-spec '(:struct sdl-audio-spec)) 
;; 	  (setf freq sample-rate)
;; 	  (setf format (foreign-enum-value 'sdl-audio-formats :audio-s16))
;; 	  (setf channels num-channels)
;; 	  (setf samples output-buffer-size)
;; 	  (setf callback (callback sdl-audio-player))
;; 	  (setf (mem-ref seq-num :int) (register-sdl-audio-device device-out))
;; 	  (setf userdata seq-num)

;; 	  (sdl-open-audio audio-spec (null-pointer))
       
;; 	  (unwind-protect 
;; 	       (progn
;; 		 (sdl-pause-audio 0)
;; 		 (with-slots (done-waiter wait-lock) device-out
;; 		   (with-lock-held (wait-lock)
;; 		     (condition-wait done-waiter wait-lock))))
;; 	    (sdl-close-audio)))))))

;; (defun run-sdl(&optional (file-path "/mnt/MUSIC-THD/test.hd.mp4"))
;;   (run-ffmpeg (audio-params) (pathname file-path) (sdl-audio-device)))      
	       
