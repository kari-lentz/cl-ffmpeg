(in-package :cl-alsa)

(defmacro defeasycondition(name (&rest parent-types) (&rest slot-names) &body report-body)
  (with-gensyms (c stream)
    `(define-condition ,name (,@parent-types)
       ,(qmap (slot-name) `(,slot-name :initarg ,(to-keyword slot-name) :reader ,slot-name) slot-names)
       (:report (lambda(,c ,stream)
		  (with-slots (,@slot-names) ,c 
		    (format ,stream ,@report-body)))))))

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

(defeasycondition alsa-warning(warning)
    (msg)
  msg)

(defeasycondition alsa-fault(error)
  (msg code)
  (let ((error-str (snd-strerror code)))
    (join " "  
	  (append (list "sdl" "lib:" msg) 
		  (when code
		    (list "return code:" code)) 
		  (list "error:" error-str)))))
 			
(defmacro alsa-assert0(lisp-name function &body args)
  (with-gensyms (ret)
    `(let ((,ret (,function ,@args)))
       (unless (= ,ret 0)
	 (error 'alsa-fault :msg (% "~a" (list ,(symbol-name lisp-name) ,@args)) :code ,ret))
       ,ret)))

(defmacro alsa-assert+(lisp-name function &body args)
  (with-gensyms (ret)
    `(let ((,ret (,function ,@args)))
       (unless (>= ,ret 0)
	 (error 'alsa-fault :msg (% "~a" (list ,(symbol-name lisp-name) ,@args)) :code ,ret))
       ,ret)))

(defcfun snd-output-stdio-attach :int
  (outputp :pointer)
  (fp :pointer)
  (close-flag :int))

(define-cffi-return-types
    ((:alsa-int :int alsa-assert0)
     (:alsa-int0 :int alsa-assert0)
     (:alsa-int+ :int alsa-assert+)))

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
  (val :pointer)
  (dir :pointer))
  
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

(defcfun* snd-pcm-wait :alsa-int+
  (pcm :pointer)
  (timeout :int))

(defctype snd-pcm-sframes-t :int)

(defcfun* snd-pcm-avail-update :alsa-int+
  (pcm :pointer))

(defcfun* snd-pcm-prepare :alsa-int
  (pcm :pointer))

(defcfun snd-pcm-writei snd-pcm-sframes-t 
  (pcm :pointer)
  (pbuffer :pointer)
  (size snd-pcm-uframes-t))

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

(defstruct* (alsa-device (:constructor alsa-device (&key (device-name "hw:0,0") (access :SND-PCM-ACCESS-RW-INTERLEAVED) (format :SND-PCM-FORMAT-S16-LE)(buffer-size 4096)(num-periods 4)(timeout 2000)))) 
    device-name access format buffer-size num-periods timeout)

(defeasycondition alsa-buffer-fault (alsa-fault)
    (actual-buffer-size requested-buffer-size)
  "requested buffer size ~a but got size ~a" requested-buffer-size actual-buffer-size)

(defeasycondition alsa-period-fault (alsa-fault)
    (actual-period-size requested-period-size)
  "requested period size ~a but got size ~a" requested-period-size actual-period-size)

(defun test-cond()
  (macroexpand-1 '(defeasycondition alsa-buffer-fault (alsa-fault)
		   (actual-buffer-size requested-buffer-size)
		   "requested buffer size ~a but got size ~a" requested-buffer-size actual-buffer-size)))

(defun get-period-size(buffer-size num-periods)
  (/ buffer-size num-periods))

(defun set-params-hw(pcm hw-params audio-params alsa-device)
  (macrolet ((hw-set(param &rest values)
	       `(,(.sym 'snd-pcm-hw-params-set- param) pcm hw-params ,@values)))
    (with-slots (sample-rate num-channels) audio-params
      (with-slots (access format buffer-size num-periods) alsa-device
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
	       (accept())
	       (try-new-size(new-buffer-size)
		 (setf buffer-size new-buffer-size)
		 (go try-buffer)))))
	(tagbody try-num-periods
	   (let ((period-size (get-period-size buffer-size num-periods)))
	     (with-foreign-object (pperiod-size 'snd-pcm-uframes-t)
	       (setf (mem-ref pperiod-size 'snd-pcm-uframes-t) period-size)
	       (restart-case
		   (progn
		     (hw-set period-size-near pperiod-size (null-pointer))
		     (let ((actual-period-size (mem-ref pperiod-size 'snd-pcm-uframes-t)))
		       (unless (= actual-period-size period-size)
			 (error 'alsa-period-fault :requested-period-size period-size :actual-period-size actual-period-size))))
		 (accept())
		 (try-new-num-periods-num (new-num-periods)
		   (setf num-periods new-num-periods)
		   (go try-num-periods))))))
	(snd-pcm-hw-params pcm hw-params)))))

(defun set-params-sw(pcm sw-params alsa-device)
  (macrolet ((sw-set(param &rest values)
	       `(,(.sym 'snd-pcm-sw-params-set- param) pcm sw-params ,@values)))  
    (with-slots (buffer-size num-periods) alsa-device
      (let ((frames-per-period (get-period-size buffer-size num-periods)))
	(sw-set start-threshold 0)
	(sw-set avail-min frames-per-period)
	(snd-pcm-sw-params pcm sw-params)))))

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

(defmacro with-sw-params((pcm sw-params) &body body)
  (with-gensyms (psw-params)
    `(with-null-pointer ,psw-params
       (snd-pcm-sw-params-malloc ,psw-params)
       (let ((,sw-params (mem-ref ,psw-params :pointer)))
	 (unwind-protect
	      (progn
		(snd-pcm-sw-params-current ,pcm ,sw-params)
		,@body)
	   (snd-pcm-sw-params-free ,sw-params))))))

(defun run-alsa(pcm audio-params alsa-device)
  (snd-pcm-prepare pcm)
  (with-slots (ring-buffer) audio-params
    (with-slots (timeout buffer-size frames-per-period) alsa-device
      (with-foreign-object (buffer 'snd-pcm-sframes-t buffer-size)
	(block play-track
	  (loop
	     (when (= (snd-pcm-wait pcm timeout) 0)
	       (warn 'alsa-warning :msg "snd-pcm-wait timed out"))
	     (let ((max-frames (snd-pcm-avail-update pcm)))
	       (loop with total-frames = 0 while (< total-frames max-frames) do
		    (let ((ret (funcall ring-buffer :read buffer max-frames)))
		      (loop with playing-frames = 0 while (< playing-frames ret) do
			   (incf playing-frames (snd-pcm-writei pcm buffer ret)))
		      (unless (= ret max-frames) (return-from play-track)) 
		      (incf total-frames ret))))))))))

(defmethod run-ffmpeg-out(audio-params (device-out alsa-device))
  (let ((alsa-device (alsa-device)))
    (with-open-alsa-device (pcm :device-name (alsa-device-device-name alsa-device))
      (with-hw-params (pcm hw-params)
	(set-params-hw pcm hw-params audio-params alsa-device)
 	(with-sw-params (pcm sw-params)
 	  (set-params-sw pcm sw-params alsa-device)
 	  (run-alsa pcm audio-params alsa-device))))))
  
(defun test-alsa(&optional (file-path "/mnt/MUSIC-THD/test.hd.mp4"))
 (let ((audio-params (audio-params)))
   (run-ffmpeg audio-params (pathname file-path) (alsa-device))))     

(defun test-alsa-bare()
  (let ((audio-params (audio-params))(alsa-device (alsa-device)))
    (with-open-alsa-device (pcm :device-name (alsa-device-device-name alsa-device))
      (with-hw-params (pcm hw-params)
	(set-params-hw pcm hw-params audio-params alsa-device)
	(with-sw-params (pcm sw-params)
	  (set-params-sw pcm sw-params alsa-device)
	  (format t "ALSA:~a:~a:~a~%" pcm hw-params sw-params))))))	       
