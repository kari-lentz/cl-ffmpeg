(in-package :cl-ffmpeg)

(defparameter *AVFMT-GLOBALHEADER* #x40)
(defparameter *CODEC-FLAG-GLOBAL-HEADER* #x400000)

(define-foreign-library libavformat
  (:unix (:or "/usr/local/lib/libavformat.so"))
  (t (:default "libavformat")))

(use-foreign-library libavformat)

(define-foreign-library libavcodec
  (:unix (:or "/usr/local/lib/libavcodec.so"))
  (t (:default "libavcodec")))

(use-foreign-library libavcodec)

(define-foreign-library libswresample
  (:unix (:or "/usr/local/lib/libswresample.so"))
  (t (:default "libswresample")))

(use-foreign-library libswresample)

(define-foreign-library ffmpeg-wrapper
  (:unix (:or "/usr/local/lib/ffmpeg-wrapper.so"))
  (t (:default "ffmpeg-wrapper")))

(use-foreign-library ffmpeg-wrapper)

(defparameter *err-buffer-length* 1024)

(define-condition ffmpeg-fault(error)
  ((msg :initarg :msg :reader msg)
   (code :initform nil :initarg :code :reader code))
  (:report (lambda(c stream)(if (code c) 
				(with-foreign-object  (buffer :char *err-buffer-length*)
				  (av-strerror (code c) buffer *err-buffer-length*)
				  (format stream "ffmpeg lib:~a error-code:~a:~a" (msg c) (code c) (foreign-string-to-lisp buffer)))
				(format stream "ffmpeg lib:~a" (msg c))))))			

(defmacro ffmpeg-assert(lisp-name function &body args)
  (with-gensyms (ret)
    `(let ((,ret (,function ,@args)))
       (unless (= ,ret 0)
	 (error 'ffmpeg-fault :msg (% "~a" (list ,(symbol-name lisp-name) ,@args)) :code ,ret))
       ,ret)))

(define-cffi-return-types
    ((:ffmpeg-int :int ffmpeg-assert)))

(defmacro define-dummy-enums(&rest enums)
  `(progn
     ,@(loop for enum in enums collecting
	    `(defcenum ,enum
	       (,(to-keyword (.sym enum '-unknown)) -1)))))

(defmacro defcstruct*(name-and-options &body fields)
  (let ((type (get-first-atom name-and-options)))
    `(progn
       (defcstruct ,name-and-options ,@fields)
       ,@(loop for item in fields collecting 
	      (let ((slot-name (get-first-atom item)))
		`(defmacro ,(.sym (get-first-atom name-and-options) '- (get-first-atom item)) (ptr) 
		   `(foreign-slot-value ,ptr ,''(:struct ,type) ,'',slot-name)))))))

(defcenum AVDuration-Estimation-Method
  :AVFMT-DURATION-FROM-PTS 
  :AVFMT-DURATION-FROM-STREAM
  :AVFMT-DURATION_FROM-BITRATE)

(defcenum AVIO-Codec-ID
  :AVCODEC-ID-NONE)

(defcstruct* AVIO-Interrupt-CB
	(callback :pointer)
	(opaque :pointer))

(defcstruct* AVRational
  (num :int)
  (den :int))
 
(defcenum avmedia-type
  (:avmedia-type-unknown -1)
  :avmedia-type-video
  :avmedia-type-audio)

(defcfun ("av_register_all" av-register-all) :void)

(defcfun* ("avformat_open_input" avformat-open-input) :ffmpeg-int
  (ps :pointer)
  (filename :string)
  (fmt :pointer)
  (options :pointer))

(defcfun ("avformat_find_stream_info" avformat-find-stream-info) :int
  (ic :pointer)
  (options :pointer))

(defctype size-t :uint)

(defcfun ("av_strerror" av-strerror) :int
  (errnum :int)
  (errbuf :string)
  (errbuf-size size-t))

(define-condition ffmpeg-fault(error)
  ((msg :initarg :msg :reader msg)
   (code :initform nil :initarg :code :reader code))
  (:report (lambda(c stream)(if (code c) 
				(with-foreign-object  (buffer :char *err-buffer-length*)
				  (av-strerror (code c) buffer *err-buffer-length*)
				  (format stream "ffmpeg lib:~a error-code:~a:~a" (msg c) (code c) (foreign-string-to-lisp buffer)))
				  (format stream "ffmpeg lib:~a" (msg c))))))			
  
(defcfun ("av_find_best_stream" av-find-best-stream) :int
  (ic :pointer)
  (type avmedia-type)
  (wanted_stream_nb :int)
  (related_stream :int)
  (decoder_ret :pointer)
  (flags :int))

(defcfun ("avcodec_alloc_context3" avcodec-alloc-context3) :pointer
  (codec :pointer))

(defcfun ("av_freep" avfreep) :void
  (ptr :pointer))

(defcfun* ("avcodec_open2" avcodec-open2) :ffmpeg-int
  (avctx :pointer)
  (codec :pointer)
  (options :pointer))

(defcfun* ("avcodec_close" avcodec-close) :ffmpeg-int
  (avctx :pointer))

(defcfun ("avformat_close_input" avformat-close-input) :void 
  (ss :pointer))
	
(defcfun ("av_frame_alloc" av-frame-alloc) :pointer)
(defcfun ("av_frame_free" av-frame-free) :void 
  (p-frame :pointer))

(defcenum AVPacket-Side-Data-Types
  (:unknown 0))

(defcstruct* AVPacket-Side-Data
  (data :pointer)
  (size :int)
  (type AVPacket-Side-Data-Types))

(defcstruct* AVPacket
  (buf :pointer)
  (pts :int64)
  (dts :int64)
  (data :pointer)
  (size :int)
  (stream-index :int)
  (flags :int)
  (side-data (:struct AvPacket-Side-Data))
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
  (codec-context :pointer)
  (frame :pointer)
  (got-frame-ptr :pointer)
  (avpk :pointer))

(defcfun ("avcodec_decode_video2" avcodec-decode-video2) :int
  (codec-context :pointer)
  (picture :pointer)
  (got-frame-ptr :pointer)
  (avpk :pointer))

(defcenum AVSample-Format 
   (:AV-SAMPLE-FMT-NONE -1)
   :AV-SAMPLE-FMT-U8
   :AV-SAMPLE-FMT-S16
   :AV-SAMPLE-FMT-S32
   :AV-SAMPLE-FMT-FLT
   :AV-SAMPLE-FMT-DBL 
   :AV-SAMPLE-FMT-U8P
   :AV-SAMPLE-FMT-S16P
   :AV-SAMPLE-FMT-S32P
   :AV-SAMPLE-FMT-FLTP
   :AV-SAMPLE-FMT-DBLP 
   :AV-SAMPLE-FMT-NB)

(define-dummy-enums AVCodec-ID AVPixel-Format AVColor-Primaries AVColor-Transfer-Characteristic AVColor-Space AVColor-Range AVChroma-Location AVField-Order AVAudio-Service-Type) 

(defcfun ("avcodec_find_encoder" avcodec-find-encoder) :pointer
  (id avcodec-id))

(defcfun ("avcodec_find_encoder_by_name" avcodec-find-encoder-by-name) :pointer
  (name  :string))	
         
(defcfun ("av_get_channel_layout" av-get-channel-layout) :uint64	
  (name :string))

(defcfun ("av_get_channel_layout_nb_channels" av-get-channel-layout-nb-channels) :int
    (channel_layout :uint64))	

(defcfun ("av_get_default_channel_layout" av-get-default-channel-layout) :int64
  (nb-channels :int))

(defcfun ("my_test" my-test) :int
    (msg :string))

(defcfun* ("avformat_alloc_output_context2" avformat-alloc-output-context2) :ffmpeg-int
  (ctx :pointer)
  (oformat :pointer)
  (format_name :string)
  (filename :string))

(defcfun ("avformat_free_context" avformat-free-context) :void
  (format-context :pointer))

(defcfun ("avformat_new_stream" avformat-new-stream) :pointer
  (format-context :pointer)
  (codec :pointer))

(defcfun* ("avformat_write_header" avformat-write-header) :ffmpeg-int
  (format-context :pointer)
  (options :pointer))

(defcfun ("av_write_frame" av-write-frame) :int
  (format-context :pointer)
  (pkt :pointer))

(defcfun ("av_interleaved_write_frame" av-interleaved-write-frame) :int
  (format-context :pointer)
  (pkt :pointer))

(defcfun* ("av_write_trailer" av-write-trailer) :ffmpeg-int
    (format-context :pointer))

(defcfun* ("avio_open" avio-open) :ffmpeg-int
  (io-context :pointer)
  (url :string)
  (flags :int))

(defcfun* ("avio_close" avio-close) :ffmpeg-int
  (io-context :pointer))

(defcstruct* (AVFrame-Overlay)
  (data :pointer :count 8)
  (linesize :int :count 8)
  (extended-data :pointer)
  (width :int)
  (height :int)
  (nb-samples :int)
  (format :int))

(defcfun av-frame-get-channels :int
  (frame :pointer))

(defcfun av-frame-set-channels :void
  (frame :pointer)
  (channels :int))

(defcfun av-frame-get-sample-rate :int
  (frame :pointer))

(defcfun av-frame-set-sample-rate :void 
  (frame :pointer)
  (sample-rate :int))

(defcfun av-frame-get-channel-layout :uint64
  (frame :pointer))

(defcfun av-frame-set-channel-layout :void 
  (frame :pointer)
  (channel-layout :int))

(defcfun av-frame-get-buffer :int
  (frame :pointer)
  (align :int))

(defcfun swr-alloc :pointer)

(defcfun swr-alloc-set-opts :pointer
  (s :pointer)
  (out_ch_layout :int64)
  (out-sample-fmt avsample-format)
  (out-sample-rate :int)
  (in-ch-layout :int64)
  (in-sample-fmt avsample-format)
  (in-sample-rate :int)
  (log-offset :int)
  (log-ctx :pointer))

(defcfun swr-set-compensation :int
  (s :pointer)
  (sample-delta :int)
  (compensation-distance :int))

(defcfun swr-init :int
  (s :pointer))

(defcfun swr-convert :int
  (s :pointer)
  (out-buffers :pointer)
  (out-count :int)
  (in-buffers :pointer)
  (in-count :int))

(defcfun swr-free :void
  (s :pointer))

(defcfun avcodec-fill-audio-frame :int
  (frame :pointer)
  (nb-channels :int)
  (sample-format avsample-format)
  (buf :pointer)
  (buf-size :int)
  (align :int))

(defcenum av-lock-op
  :av-lock-create
  :av-lock-obtain
  :av-lock-release
  :av-lock-destroy)

(defcfun av-lockmgr-register :int
  (callback :pointer))

(defstruct (thread-control (:constructor thread-control (lisp-lock foreign-lock-hash-table sequence-num))) lisp-lock foreign-lock-hash-table sequence-num)
  
(defparameter *thread-locks* (make-hash-table))
(defparameter *thread-control* (thread-control (bordeaux-threads:make-lock "FFMPEG-LOCK") *thread-locks* 0))

(defmacro with-foreign-lock(thread-control &body body)
  `(with-slots (lisp-lock foreign-lock-hash-table sequence-num) ,thread-control
     (bordeaux-threads:with-lock-held (lisp-lock)
       ,@body)))
  
(defun create-lock(thread-control)
  (with-foreign-lock thread-control
    (setf (gethash sequence-num foreign-lock-hash-table) (bordeaux-threads:make-lock))
    (post-fix sequence-num
	      (incf sequence-num))))

(defun acquire-lock(thread-control lock-key)
  (with-foreign-lock thread-control
    (aif (gethash lock-key foreign-lock-hash-table)
	 (bordeaux-threads:acquire-lock it)
	 (error (% "ffmpeg acquire: lock not found for sequence~a" sequence-num)))))

(defun release-lock(thread-control lock-key)
  (with-foreign-lock thread-control
    (aif (gethash lock-key foreign-lock-hash-table)
	 (bordeaux-threads:release-lock it)
	 (error (% "ffmpeg release lock not found for sequence~a" sequence-num)))))

(defun destroy-lock(thread-control lock-key)
  (with-foreign-lock thread-control
    (remhash  lock-key foreign-lock-hash-table)))

(defparameter *debug-lock-mgr* nil)

(defcallback my-lock-mgr :int ((arg :pointer)(op av-lock-op))
  (macrolet ((log*(msg)
	       `(when *debug-lock-mgr*
		  (format *standard-output* "LOCK MGR:~a THREAD-CONTROL:~a~%" ,msg *thread-control*))))
    (handler-case
	(progn
	  (case op
	    (:av-lock-create
	     (log* "create")
	     (setf (mem-ref arg :int) (create-lock *thread-control*)))
	    (:av-lock-obtain
	     (log* "obtain")
	     (acquire-lock *thread-control* (mem-ref arg :int)))
	    (:av-lock-release
	     (log* "release")
	     (release-lock *thread-control* (mem-ref arg :int)))
	    (:av-lock-destroy
	     (log* "destroy")
	     (destroy-lock *thread-control* (mem-ref arg :int))))
	  0)
      (error()))))

(defmacro define-ffmpeg-wrappers(prefix &body name-types)
  `(progn
     ,@(reduce #'append
	       (loop for (name type) in name-types collecting
		    (progn
		      (unless type (error 'ffmpeg-fault :msg (% "No type for ~a-~a" prefix name)))  
		      (let ((accessor-name (.sym prefix '- name))(lisp-get-name (.sym prefix '-get- name))(lisp-set-name (.sym prefix '-set- name)))
			(flet ((lisp-to-c(lisp-name)
				 (string-downcase (cl-ppcre:regex-replace-all "\\-" (symbol-name lisp-name) "_"))))
			  (let ((c-get-name (lisp-to-c lisp-get-name))(c-set-name (lisp-to-c lisp-set-name)))
			    `((defcfun (,c-get-name ,accessor-name) ,type
				(,prefix :pointer))
			      (defcfun (,c-set-name ,lisp-set-name) :void
				(,prefix :pointer)
				(,name ,type))
			      (defsetf ,accessor-name ,lisp-set-name))))))))))

(define-ffmpeg-wrappers codec-context
  (channel-layout :uint64)
  (channels :int)
  (sample-fmt AVSample-Format)
  (sample-rate :int)
  (bit-rate :int)
  (flags :int))

(define-ffmpeg-wrappers format-context
  (oformat :pointer)
  (pb :pointer)
  (flags :int))

(define-ffmpeg-wrappers output-format
  (audio-codec avcodec-id)
  (video-codec avcodec-id)
  (flags :int))

(define-ffmpeg-wrappers stream
    (codec :pointer))

(with-full-eval
  (defun lisp-to-c(function-name)
    (string-downcase (cl-ppcre:regex-replace-all "\\-" (symbol-name function-name) "_"))))

(defmacro define-foreign-call-pointers(identifier &body pointers)
  `(defparameter ,identifier 
     ({}
      ,@(loop for (key pointer) in pointers collecting
	    `(,key (foreign-symbol-pointer ,(lisp-to-c pointer)))))))

(define-foreign-call-pointers *decoders* (:avmedia-type-audio avcodec-decode-audio4)(:avmedia-type-video avcodec-decode-video2))
(define-foreign-call-pointers *encoders* (:avmedia-type-audio avcodec-encode-audio2)(:avmedia-type-video avcodec-decode-video2))
(define-foreign-call-pointers *codecs* (:avmedia-type-audio output-format-get-audio-codec)(:avmedia-type-video output-format-get-video-codec))

(defcstruct* (AVFormat-Context-Overlay)
  (av-class  :pointer)
  (iformat  :pointer)
  (oformat  :pointer)
  (priv-data  :pointer)
  (pb  :pointer)
  (ctx-flags  :int)
  (nb-streams  :unsigned-int)
  (streams  :pointer))

(defcstruct* (AVStream-Overlay)
  (index :int)
  (id :int)
  (codec :pointer))

(defcstruct audio-frame
  (samples :ushort :count 2))

