(in-package :ffmpeg-test)

(define-foreign-library libavformat
  (:unix (:or "/usr/local/lib/libavformat.so"))
  (t (:default "libavformat")))

(use-foreign-library libavformat)

(define-foreign-library libavcodec
  (:unix (:or "/usr/local/lib/libavcodec.so"))
  (t (:default "libavcodec")))

(use-foreign-library libavcodec)

(defcenum AVDuration-Estimation-Method
  :AVFMT-DURATION-FROM-PTS 
  :AVFMT-DURATION-FROM-STREAM
  :AVFMT-DURATION_FROM-BITRATE)

(defcenum AVIO-Codec-ID
  :AVCODEC-ID-NONE)

(defcstruct AVIO-Interrupt-CB
	(callback :pointer)
	(opaque :pointer))

(defcstruct AVRational
  (num :int)
  (den :int))
 
(defcenum av-media-type
  (:avmedia-type-unknown -1)
  :avmedia-type-video
  :avmedia-type-audio)

(defcfun ("av_register_all" av-register-all) :void)

(defcfun ("avformat_open_input" avformat-open-input) :int
  (ps :pointer)
  (filename :string)
  (fmt :pointer)
  (options :pointer))

(defcfun ("avformat_find_stream_info" avformat-find-stream-info) :int
  (ic :pointer)
  (options :pointer))

(define-condition ffmpeg-fault(error)
  ((msg :initarg :msg :reader msg)
   (code :initform nil :initarg :code :reader code))
  (:report (lambda(c stream)(if (code c) 
				(format stream "ffmpeg lib:~a error-code:~a" (msg c) (code c))
				(format stream "ffmpeg lib:~a" (msg c))))))				

(defcfun ("av_find_best_stream" av-find-best-stream) :int
  (ic :pointer)
  (type av-media-type)
  (wanted_stream_nb :int)
  (related_stream :int)
  (decoder_ret :pointer)
  (flags :int))

(defcfun ("avcodec_alloc_context3" avcodec-alloc-context3) :pointer
  (codec :pointer))

(defcfun ("avcodec_close" avcodec-close) :int
  (avctx :pointer))

(defcfun ("avfreep" avfreep) :void
  (ptr :pointer))

(defcfun ("avcodec_open2" avcodec-open2) :int
  (avctx :pointer)
  (codec :pointer)
  (options :pointer))

(defcfun ("avformat_close_input" avformat-close-input) :void 
  (ss :pointer))
	
(defcfun ("av_frame_alloc" av-frame-alloc) :pointer)
(defcfun ("av_frame_free" av-frame-free) :void 
  (p-frame :pointer))

(defcenum AVPacket-Side-Data-Types
  (:unknown 0))

(defcstruct AVPacket-Side-Data
  (data :uint8)
  (size :int)
  (type AVPacket-Side-Data-Types))

(defcstruct AVPacket
  (buf :pointer)
  (pts :int64)
  (dts :int64)
  (data :uint8)
  (size :int)
  (stream-index :int)
  (flags :int)
  (side-data AvPacket-Side-Data)
  (side-data-elems :int)
  (duration :int)
  (pos :int64)
  (convergence-duration :int64))

(defcfun ("av_read_frame" av-read-frame) :int
    (format-context :pointer)
    (packet :pointer))

(defcfun ("av_init_packet" av-init-packet) :void
  (packet :pointer))

(defcfun ("av_free_packet" av-free-packet) :void
  (packet :pointer))

(defcfun ("avcodec_decode_audio4" avcodec-decode-audio4) :int
  (frame :pointer)
  (got_frame_ptr :pointer)
  (avpk :pointer))

(defcfun ("avcodec_decode_video2" avcodec-decode-video2) :int
  (picture :pointer)
  (got_frame_ptr :pointer)
  (avpk :pointer))

(defparameter *decoders* ({} (:av-media-type-audio (foreign-symbol-pointer "avcodec_decode_audio4"))(:av-media-type-video (foreign-symbol-pointer "avcodec_decode_video2"))))

(defmacro with-av-pointer(ptr allocator &body body)
  (with-gensyms (holder)
    `(let ((,ptr ,allocator))
       (unless (null-pointer-p ,ptr)
	 (unwind-protect
	      (progn
		,@body)
	   (with-foreign-object (,holder :pointer)
	     (setf (mem-ref ,holder :pointer) ,ptr)
	     (avfreep ,holder)))))))

  
(defmacro with-av-frame(av-frame &body body)
  (with-gensyms (holder)
    `(let ((,av-frame (av-frame-alloc)))
       (unwind-protect
	    (progn
	      ,@body)
	 (with-foreign-object (,holder :pointer)
	   (setf (mem-ref ,holder :pointer) ,av-frame)
	   (av-frame-free ,holder))))))

(defmacro with-av-packet(av-packet &body body)
  `(with-foreign-object (,av-packet 'AVPacket)
     (av-init-packet ,av-packet)
     (unwind-protect
	  (progn
	    ,@body)
       (av-free-packet ,av-packet))))

(defmacro in-frame-read-loop(format-context stream-idx packet &body body)
  `(loop
      (with-av-packet ,packet
	(unless (= (av-read-frame ,format-context packet) 0) 
	  (return))
	(if (= ,stream-idx (foreign-slot-value packet 'AVPacket 'stream-index))
	    ,@body))))

;; (defmacro with-decoded-frame(codec-context stream-type frame packet)
;;   `(with-foreign-object (p-got-frame-ptr :pointer) 
;;     (foreign-funcall-pointer ([] *decoders* stream-type) :pointer codec-context :pointer frame :pointer p-got-frame-ptr :pointer packet))
;;   (unless (= (mem-ref p-got-frame :int) 0)
;;     ,@body

(defmacro with-codec_context(context codec &body body)
    `(with-av-pointer ,context (avcodec-alloc-context3 ,codec)
       (unwind-protect
	(progn
	  ,@body)
	 (avcodec-close ,context))))

(defmacro with-open-input((p-format-context file-path ) &body body)
  (with-gensyms (pp-format-context c-file-path ret)
    `(with-foreign-string (,c-file-path ,file-path)
       (with-foreign-objects ((,pp-format-context :pointer))
	 (setf (mem-ref ,pp-format-context :pointer) (null-pointer))
	 (let ((,ret (avformat-open-input ,pp-format-context ,c-file-path (null-pointer) (null-pointer))))
	   (if (= ,ret 0)
	       (let ((,p-format-context (mem-ref ,pp-format-context :pointer)))
		 (if (not (null-pointer-p ,p-format-context))
		     (progn
		       (unwind-protect
			    (progn
			      ,@body)
			 (avformat-close-input ,pp-format-context)))))))))))
		 
(defmacro with-input-stream((p-format-context p-codec-context stream-idx file-path media-type) &body body)
  `(with-open-input (,p-format-context ,file-path)
     (avformat-find-stream-info ,p-format-context (null-pointer))
     (let ((,stream-idx (av-find-best-stream ,p-format-context (foreign-enum-value 'av-media-type ,media-type) -1 -1 (null-pointer) 0)))
       (let ((,p-codec-context (get-codec-context ,p-format-context ,stream-idx)))
	 ,@body))))))

(defcstruct (AVFormat-Context-Overlay)
  (av-class  :pointer)
  (iformat  :pointer)
  (oformat  :pointer)
  (priv-data  :pointer)
  (pb  :pointer)
  (ctx-flags  :int)
  (nb-streams  :unsigned-int)
  (streams  :pointer))

(defcstruct (AVStream-Overlay)
  (index :int)
  (id :int)
  (codec :pointer))

(defun get-codec-context(format-context stream-idx)
  (let ((nb-streams (foreign-slot-value format-context 'AVFormat-Context-Overlay 'nb-streams)))
    (cond ((>= stream-idx nb-streams)
	   (error 'ffmpeg-fault :msg (% "stream index:~a too high" stream-idx)))
	  ((< stream-idx 0)
	   (error 'ffmpeg-fault :msg (% "stream index below zero"))))
    (let ((streams (foreign-slot-value format-context 'AVFormat-Context-Overlay 'streams)))
      (let ((stream (mem-aref streams :pointer stream-idx)))
	(foreign-slot-value stream 'AVStream-Overlay 'codec)))))

(defun test-ffmpeg(&optional (stream-type :avmedia-type-audio))
  (av-register-all)
  (let ((frames 0))
    (with-input-stream (p-format-context p-codec-context stream-idx "/mnt/MUSIC-THD/test.hd.mp4" stream-type)
      (with-av-frame av-frame
	(in-frame-read-loop p-format-context stream-idx packet
	  (incf frames))
	(format t "frames-count:~a decode-context:~a~%" frames p-codec-context)))))

(defun run())
