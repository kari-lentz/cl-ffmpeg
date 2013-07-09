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

(defcfun ("avformat_open_input" avformat-open-input) :int
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

(defcfun ("avcodec_open2" avcodec-open2) :int
  (avctx :pointer)
  (codec :pointer)
  (options :pointer))

(defcfun ("avcodec_close" avcodec-close) :int
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

(defcfun ("avformat_alloc_output_context2" avformat-alloc-output-context2) :int
  (ctx :pointer)
  (oformat :pointer)
  (format_name :string)
  (filename :string))

(defcfun ("avformat_free_context" avformat-free-context) :void
  (format-context :pointer))

(defcfun ("avformat_new_stream" avformat-new-stream) :pointer
  (format-context :pointer)
  (codec :pointer))

(defcfun ("avformat_write_header" avformat-write-header) :int
  (format-context :pointer)
  (options :pointer))

(defcfun ("av_write_frame" av-write-frame) :int
  (format-context :pointer)
  (pkt :pointer))

(defcfun ("av_interleaved_write_frame" av-interleaved-write-frame) :int
  (format-context :pointer)
  (pkt :pointer))

(defcfun ("av_write_trailer" av-write-trailer) :int
    (format-context :pointer))

(defcfun ("avio_open" avio-open) :int
  (io-context :pointer)
  (url :string)
  (flags :int))

(defcfun ("avio_close" avio-close) :int
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
		  (format *standard-output* "LOCK MGR:~a~%" ,msg))))
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

(defcstruct* wav-header
  (chunk-id :int32)
  (chunk-size :int32)
  (format :int32)
  (subchunk-iid :int32)
  (subchunk-1-size :int32)
  (audio-format :int16)
  (num-channels :int16)
  (sample-rate :int32)
  (byte-rate :int32)
  (block-align :int16)
  (bits-per-sample :int16)
  (subchunk-2-id :int32)
  (subchunk-2-size :int32))

(defmacro with-setter((macrolet-name prefix object) &body body)
  `(macrolet((,macrolet-name(symbol value)
	       `(setf (,(.sym ',prefix '- symbol) ,',object) ,value)))
     ,@body))

(defmacro with-foreign-setter((setter var type &optional (count 1)) &body body)
  `(with-foreign-object (,var '(:struct ,type) ,count)
     (with-setter (,setter ,type ,var)
       ,@body)))

(defmacro with-wav-header((setter hdr num-channels sample-rate file-size) &body body)
  `(with-foreign-setter (,setter ,hdr wav-header)
     (,setter chunk-id #x52494646)
     (,setter chunk-size (+ 36 ,file-size))
     (,setter format #x57415654)
     (,setter subchunk-iid #x666d7420)
     (,setter subchunk-1-size 16)
     (,setter audio-format 1)
     (,setter num-channels ,num-channels)
     (,setter sample-rate ,sample-rate)
     (,setter byte-rate (* 2 ,num-channels 2))
     (,setter block-align (* ,num-channels))
     (,setter bits-per-sample 16)
     (,setter subchunk-2-id #x64617641)
     (,setter subchunk-2-size ,file-size)
     ,@body))

(defun test-setter()
  (with-foreign-setter (set* my-header wav-header)
    (set* format #x57415654)
    (set* audio-format 1) 
    (set* SAMPLE-RATE 44100)
    (format t "~a~%" (wav-header-audio-format my-header))
    (format t "~a~%" (wav-header-sample-rate my-header))))

(defmacro with-foreign-array-to-lisp((foreign-array foreign-type lisp-array) &body body)
  (with-once-only (foreign-array lisp-array)
    (with-gensyms (length idx)
      `(let ((,length (length ,lisp-array)))
	   (dotimes (,idx ,length)
	     (setf (aref ,lisp-array ,idx) (mem-aref ,foreign-array ,foreign-type ,idx)))
	   ,@body))))

(defmacro define-ffmpeg-wrappers(prefix &body name-types)
  `(progn
     ,@(with-collector 
	(!push)
	(loop for (name type) in name-types do
	     (unless type (error 'ffmpeg-fault :msg (% "No type for ~a-~a" prefix name)))  
	     (let ((accessor-name (.sym prefix '- name))(lisp-get-name (.sym prefix '-get- name))(lisp-set-name (.sym prefix '-set- name)))
	       (flet ((lisp-to-c(lisp-name)
			(string-downcase (cl-ppcre:regex-replace-all "\\-" (symbol-name lisp-name) "_"))))
		 (let ((c-get-name (lisp-to-c lisp-get-name))(c-set-name (lisp-to-c lisp-set-name)))
		   (!push `(defcfun (,c-get-name ,accessor-name) ,type
			     (,prefix :pointer)))
		   (!push `(defcfun (,c-set-name ,lisp-set-name) :void
			     (,prefix :pointer)
			     (,name ,type)))
		   (!push `(defsetf ,accessor-name ,lisp-set-name)))))))))

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

(defmacro free-av-double-pointer(ptr &optional (free-function 'avfreep))
  (with-gensyms (holder)
    `(with-foreign-object (,holder :pointer)
       (setf (mem-ref ,holder :pointer) ,ptr)
       (,free-function ,holder))))

(defmacro with-av-pointer((ptr allocator &optional (free-function 'avfreep)) &body body)
  `(let ((,ptr ,allocator))
     (unless (null-pointer-p ,ptr)
       (unwind-protect
	    (progn
	      ,@body)
	 (free-av-double-pointer ,ptr ,free-function)))))

(defmacro with-av-frame(av-frame &body body)
  (with-gensyms (holder)
    `(let ((,av-frame (av-frame-alloc)))
       (unwind-protect
	    (progn
	      ,@body)
	 (with-foreign-object (,holder :pointer)
	   (setf (mem-ref ,holder :pointer) ,av-frame)
	   (av-frame-free ,holder))))))

(defmacro with-av-frames((&rest av-frames) &body body)
  (with-gensyms (holder)
    `(let 
	 ,(loop for av-frame in av-frames collecting
	       `(,av-frame (av-frame-alloc)))
       (unwind-protect
	    (progn
	      ,@body)
	 ,@(loop for av-frame in av-frames collecting
	       `(with-foreign-object (,holder :pointer)
		  (setf (mem-ref ,holder :pointer) ,av-frame)
		  (av-frame-free ,holder)))))))

(defmacro with-av-packet(av-packet &body body)
  `(with-foreign-object(,av-packet '(:struct AVPacket))
     (av-init-packet ,av-packet)
     (unwind-protect
	  (progn
	    ,@body)
       (av-free-packet ,av-packet))))

(defmacro in-frame-read-loop(format-context stream-idx packet &body body)
  `(loop
      (with-av-packet ,packet
	(unless (= (av-read-frame ,format-context ,packet) 0) 
	  (return))
	(if (= ,stream-idx (AVPacket-stream-index ,packet))
	    (progn
	      ,@body)))))

(defmacro with-decoded-frame((codec-context stream-type frame packet) &body body)
  (with-gensyms (p-got-frame-ptr ret packet-size)
    `(with-foreign-object (,p-got-frame-ptr :int) 
       (let ((,packet-size (AVPacket-size ,packet)))
	 (let ((,ret (foreign-funcall-pointer ([] *decoders* ,stream-type) () :pointer ,codec-context :pointer ,frame :pointer ,p-got-frame-ptr :pointer ,packet :int)))
	   (unless (= ,ret ,packet-size) (error 'ffmpeg-fault :msg (% "decode fault -> decoded bytes:~a expected bytes~a" ,ret ,packet-size)))
	   (unless (= (mem-ref ,p-got-frame-ptr :int) 0)
	     ,@body))))))

(defmacro in-decoded-frame-read-loop((frame file-path &optional (stream-type :avmedia-type-audio)) &body body)
  (with-gensyms (p-format-context-in p-codec-context-in stream-idx packet-in)
    `(with-av-frame ,frame
       (with-input-stream (,p-format-context-in ,p-codec-context-in ,stream-idx ,file-path ,stream-type)
	 (in-frame-read-loop ,p-format-context-in ,stream-idx ,packet-in
	   (with-decoded-frame (,p-codec-context-in ,stream-type ,frame ,packet-in)
	     ,@body))))))

(defmacro with-open-input((p-format-context file-path ) &body body)
  (with-gensyms (pp-format-context ret)
    `(with-foreign-objects ((,pp-format-context :pointer))
       (setf (mem-ref ,pp-format-context :pointer) (null-pointer))
       (let ((,ret (avformat-open-input ,pp-format-context ,file-path (null-pointer) (null-pointer))))
	 (unless (= ,ret 0) (error 'ffmpeg-fault :msg (% "unable to open:~a" ,file-path) :code ,ret))
	 (let ((,p-format-context (mem-ref ,pp-format-context :pointer)))
	   (if (not (null-pointer-p ,p-format-context))
	       (progn
		 (unwind-protect
		      (progn
			,@body)
		   (avformat-close-input ,pp-format-context)))))))))

(defmacro with-open-codec((codec-context codec &optional name) &body body)
  (with-gensyms (ret)
    `(let ((,ret (avcodec-open2 ,codec-context ,codec (null-pointer))))
       (unless (= ,ret 0) (error 'ffmpeg-fault :msg (% "codec open fault:~a" (or ,name "unknown")) :code ,ret))
       (unwind-protect
	    (progn
	      ,@body)
	 (avcodec-close ,codec-context)))))

(defun open-codec-2(codec-context codec &optional name)
  (let ((ret (avcodec-open2 codec-context codec (null-pointer))))
       (unless (= ret 0) (error 'ffmpeg-fault :msg (% "codec open fault:~a" (or name "unknown")) :code ret))))
       		 
(defmacro with-input-stream((p-format-context p-codec-context stream-idx file-path media-type) &body body)
  (with-gensyms (pp-codec p-codec)
    `(with-open-input (,p-format-context ,file-path)
       (avformat-find-stream-info ,p-format-context (null-pointer))
       (with-foreign-object (,pp-codec :pointer)
	 (let ((,stream-idx (av-find-best-stream ,p-format-context (foreign-enum-value 'avmedia-type ,media-type) -1 -1 ,pp-codec 0)))
	   (let ((,p-codec-context (get-codec-context ,p-format-context ,stream-idx))(,p-codec (mem-ref ,pp-codec :pointer)))
	     (open-codec-2 ,p-codec-context ,p-codec)
	     ,@body))))))

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

(defun get-codec-context(format-context stream-idx)
  (let ((nb-streams (AVFormat-Context-Overlay-nb-streams format-context)))
    (cond ((>= stream-idx nb-streams)
	   (error 'ffmpeg-fault :msg (% "stream index:~a too high" stream-idx)))
	  ((< stream-idx 0)
	   (error 'ffmpeg-fault :msg (% "stream index below zero"))))
    (let ((streams (AVFormat-Context-Overlay-streams format-context)))
      (let ((stream (mem-aref streams :pointer stream-idx)))
	(AVStream-Overlay-codec stream)))))

(defun set-audio-params(codec-context num-channels sample-rate bit-rate)
  ;(setf (codec-context-channel-layout codec-context) (av-get-default-channel-layout num-channels))
  (setf (codec-context-channels codec-context) num-channels)
  (setf (codec-context-sample-rate codec-context) sample-rate)
  (setf (codec-context-sample-fmt codec-context) (foreign-enum-value 'AVSample-Format :AV-SAMPLE-FMT-S16))
  (when bit-rate (setf (codec-context-bit-rate codec-context) bit-rate)))

(defun get-codec(format-context stream-type)
  (let ((oformat (format-context-oformat format-context)))
    (when (null-pointer-p oformat) (error 'ffmpeg-fault :msg "null output format in format context"))
    (let ((codec-id (foreign-funcall-pointer ([] *codecs* stream-type) () :pointer oformat :int)))
      (let ((ret (avcodec-find-encoder codec-id))) 
	(when (null-pointer-p ret) (error 'ffmpeg-fault :msg "no encoder for format context")) 
	ret))))

(defmacro with-encoder((codec-context codec format-context stream-type) &body body)
  (with-gensyms (stream)
    `(let ((,codec (get-codec ,format-context ,stream-type)))
       (let ((,stream (avformat-new-stream ,format-context ,codec))) 
	 (let ((,codec-context (stream-codec ,stream)))
	   (if (> (logand (output-format-flags (format-context-oformat ,format-context)) *AVFMT-GLOBALHEADER*) 0)
	       (setf (codec-context-flags ,codec-context) (logior (codec-context-flags ,codec-context) *CODEC-FLAG-GLOBAL-HEADER*)))
	   ,@body)))))

(defmacro with-audio-encoder((codec-context format-context stream-type &key (sample-rate 44100) (num-channels 2) bit-rate) &body body)
  (with-gensyms (codec)
    `(with-encoder (,codec-context ,codec ,format-context ,stream-type)  
       (set-audio-params ,codec-context ,num-channels ,sample-rate ,bit-rate)
       (with-open-codec (,codec-context ,codec) 
	 ,@body))))

(defmacro with-new-encoder((codec-context codec name) &body body)
  `(let ((,codec (avcodec-find-encoder-by-name ,name)))
     (when (null-pointer-p ,codec) (error 'ffmpeg-fault :msg (% "codec ~a not found" ,name)))
     (with-av-pointer (,codec-context (avcodec-alloc-context3 ,codec))
       (when (null-pointer-p ,codec-context) (error 'ffmpeg-fault :msg (% "failed to open codec context:~a" ,name)))
       (unwind-protect
	    (progn
	      ,@body)
	 (avcodec-close ,codec-context)))))

(defmacro with-new-audio-encoder((codec-context name &key (sample-rate 44100) (num-channels 2) bit-rate) &body body)
  (with-gensyms (codec)
    `(with-new-encoder (,codec-context ,codec ,name)
       (set-audio-params ,codec-context ,num-channels ,sample-rate ,bit-rate)
       (when ,bit-rate (setf (codec-context-bit-rate ,codec-context) ,bit-rate))
       (with-open-codec (,codec-context ,codec ,name) 
	 ,@body))))

(defmacro with-encoded-packet((codec-context stream-type packet frame) &body body)
  (with-gensyms (p-got-packet-ptr ret)
    `(with-foreign-object (,p-got-packet-ptr :int)
       (with-av-packet ,packet 
	 (let ((,ret (foreign-funcall-pointer ([] *encoders* ,stream-type) () :pointer ,codec-context :pointer ,packet :pointer ,frame :pointer ,p-got-packet-ptr :int)))
	   (unless (= ,ret 0) (error 'ffmpeg-fault :msg (% "encode fault") :code ,ret))
	   (unless (= (mem-ref ,p-got-packet-ptr :int) 0)
	     ,@body))))))

(defun ensure-file-gone(file-path)
  (handler-case
      (progn
	(close (open file-path))
	(delete-file file-path))
    (sb-int:simple-file-error())))
				
(defmacro with-format-context((p-format-context file-path) &body body)
  (with-gensyms (pp-format-context ret)
    (with-once-only (file-path)
      `(with-foreign-objects ((,pp-format-context :pointer))
	 (setf (mem-ref ,pp-format-context :pointer) (null-pointer))
	 (let ((,ret (avformat-alloc-output-context2 ,pp-format-context (null-pointer) (null-pointer) ,file-path)))
	   (unless (= ,ret 0) (error 'ffmpeg-fault :msg (% "unable to open:~a" ,file-path) :code ,ret))
	   (let ((,p-format-context (mem-ref ,pp-format-context :pointer)))
	     (if (not (null-pointer-p ,p-format-context))
		 (unwind-protect
		      (progn
			,@body)
		   (avformat-free-context ,p-format-context)))))))))

(defmacro with-open-output-file((file-path format-context) &body body)
  (with-once-only (file-path)
    (with-gensyms (p-io-context ret)
      `(with-foreign-object (,p-io-context :pointer)
	 (ensure-file-gone ,file-path)
	 (let ((,ret (avio-open ,p-io-context ,file-path 2)))
	   (unless (= ,ret 0) (error 'ffmpeg-fault :msg (% "could not open ~a for writing" ,file-path) :code ,ret))
	   (setf (format-context-pb ,format-context) (mem-ref ,p-io-context :pointer))
	   (unwind-protect
		(progn
		  ,@body)
	     (avio-close (format-context-pb ,format-context))))))))

(defmacro with-output-sink((format-context file-path) &body body)
  `(with-format-context(,format-context ,file-path)
     (with-open-output-file (,file-path ,format-context)
       ,@body)))

(defparameter *raw-buffer-size* 4096)
(defparameter *raw-buffer* (foreign-alloc :uint16 :count *raw-buffer-size*))

(defmacro with-restarter((ptr reallocator allocator-function (&body changing-parameters) free-function double-pointer-free-p) &body body)
  (let ((old-vars (qmap (param) (gensym (string-upcase (symbol-name param))) changing-parameters)))
    `(let ((,ptr (,allocator-function ,@changing-parameters)))
       (macrolet ,reallocator ,changing-parameters
		 `(unless (and ,(qmap `(equalp ,param ,old-param) (list ,@changing-parameters) (list ,@old-vars)))
		   (if ,,double-pointer-free-p 
		       (free-av-double-pointer ,,ptr ,,free-function) 
		       (,,free-function ,,ptr))
		   (setf ,,ptr (,,allocator-function ,,@changing-parameters)))
		 (unwind-protect
		      (progn
			,@body)
		   (if ,double-pointer-free-p 
		       (free-av-double-pointer ,ptr ,free-function) 
		       (,free-function ,ptr)))))))

(defun test-realloc()
  (macroexpand-1
   `(with-restarter (swr-ctx swr-realloc swr-context (x y z a b c) swr-free t)
      (list x y z a b c))))

(defmacro run-once(ht params &body resource-make-code)
  (with-gensyms (resource)
    `(progn
       (unless ,ht (setf ,ht (make-hash-table :test 'equal)))
       (let ((,resource (gethash (list ,@params) ,ht)))
	 (if ,resource
	     ,resource
	     (let ((,resource (progn 
				,@resource-make-code)))
	       (setf (gethash (list ,@params) ,ht) ,resource)
	       ,resource))))))

(defun open-swr-context-mgr()
  (make-hash-table :test 'equal))

(defun acquire-swr-context(ctx-mgr out-channel-layout out-sample-rate in-channel-layout in-sample-format in-sample-rate)
  (run-once 
      ctx-mgr
      (out-channel-layout out-sample-rate in-channel-layout in-sample-format in-sample-rate)
    (let ((swr-ctx (swr-alloc-set-opts 
		    (null-pointer)
		    out-channel-layout
		    :av-sample-fmt-s16
		    out-sample-rate
		    in-channel-layout
		    in-sample-format 
		    in-sample-rate
		    0
		    (null-pointer))))
      
      (format t "RUNNING PROTECTED RESAMPLE at ~a:~a~%" out-sample-rate out-channel-layout)

      (let ((ret (swr-init swr-ctx)))
	(unless (= ret 0) (error 'ffmpeg-fault :msg "could not allocate resample context" :code ret))
	swr-ctx))))

(defun close-swr-context-mgr(ctx-mgr) 
  (dolist (param-set (hash-table-keys ctx-mgr))
    (with-foreign-object (holder :pointer)
      (setf (mem-ref holder :pointer) (gethash param-set ctx-mgr))
      (swr-free holder)
      (remhash param-set ctx-mgr))))

(defmacro with-swr-context-mgr(swr-ctx-mgr &body body)
  `(let ((,swr-ctx-mgr (open-swr-context-mgr)))
     (unwind-protect
	  (progn
	    ,@body)
       (close-swr-context-mgr ,swr-ctx-mgr))))
	     
(defmacro with-buffered-frame((frame-out nb-samples &optional (rate 44100) (num-channels 2)) &body body)
  (with-gensyms (ret)
    `(with-av-frame ,frame-out
       (setf (avframe-overlay-nb-samples ,frame-out) ,nb-samples)
       (av-frame-set-channel-layout ,frame-out (av-get-default-channel-layout ,num-channels))
       (av-frame-set-channels ,frame-out ,num-channels)
       (av-frame-set-sample-rate ,frame-out ,rate) 
       (setf (avframe-overlay-format ,frame-out) (foreign-enum-value 'avsample-format :av-sample-fmt-s16))
       (let ((,ret (av-frame-get-buffer ,frame-out 0)))
	 (unless (= ,ret 0) (error 'ffmpeg-fault :msg "could not allocate buffer" :code ,ret)))
       ,@body)))
    
(defmacro with-resampled-frame((frame-out swr-ctx-mgr frame-in &optional (rate 44100)) &body body)
  (with-gensyms(nb-samples swr-ctx num-channels)
    `(let ((,nb-samples (ceiling  (/ (* ,rate (avframe-overlay-nb-samples ,frame-in)) (av-frame-get-sample-rate ,frame-in))))(,num-channels (av-frame-get-channels frame-in)))
      (let ((,swr-ctx (acquire-swr-context ,swr-ctx-mgr (av-get-default-channel-layout ,num-channels) ,rate (av-frame-get-channel-layout ,frame-in) (AVFrame-Overlay-format ,frame-in) (av-frame-get-sample-rate ,frame-in))))
	(with-buffered-frame (,frame-out ,nb-samples ,rate ,num-channels)
	  (setf (avframe-overlay-nb-samples ,frame-out) (swr-convert ,swr-ctx (avframe-overlay-data ,frame-out) ,nb-samples (avframe-overlay-data ,frame-in) (avframe-overlay-nb-samples ,frame-in)))
	  ,@body)))))
 
(defun write-to-buffer(buffer frame)
  (funcall buffer :write (avframe-overlay-data frame) (avframe-overlay-nb-samples frame)))
 
(defun test-ffmpeg!(&optional (stream-type :avmedia-type-audio))
  (av-register-all)
  (let ((frames 0))
    (with-input-stream (p-format-context-in p-codec-context-in stream-idx "/mnt/MUSIC-THD/test.hd.mp4" stream-type)
      (with-new-audio-encoder (p-codec-context-out "libmp3lame" :num-channels 2 :bit-rate 128000) 
	(with-av-frame frame
	  (in-frame-read-loop p-format-context-in stream-idx packet-in
	    (with-decoded-frame (p-codec-context-in stream-type frame packet-in)
	      (with-encoded-packet (p-codec-context-out stream-type packet-out frame)
		(incf frames)
		(format t "frame-count:~a~%" frames))))
	  (format t "frames-count:~a decode-context:~a~%" frames p-codec-context-in))))))

(defun test-ffmpeg-serial(&optional (stream-type :avmedia-type-audio) (sample-rate 44100))
  (let ((output-file-path "/mnt/MUSIC-THD/dummy.wav"))
    (av-register-all)
    (let ((ret (av-lockmgr-register (callback my-lock-mgr))))
      (unless (= ret 0) (error 'ffmpeg-fault :code ret :msg "could not register lock manager")))
    (with-swr-context-mgr swr-ctx-mgr
      (let ((decoded-frames 0)(encoded-packets 0))
	(with-input-stream (p-format-context-in p-codec-context-in stream-idx "/mnt/MUSIC-THD/test.hd.mp4" stream-type)
	  (with-output-sink (p-format-context-out output-file-path)
	    (with-audio-encoder (p-codec-context-out p-format-context-out stream-type :num-channels 2 :sample-rate sample-rate)
	      (with-av-frame frame-in
		(avformat-write-header p-format-context-out (null-pointer)) 

		(in-frame-read-loop p-format-context-in stream-idx packet-in
		  (with-decoded-frame (p-codec-context-in stream-type frame-in packet-in)
		    (incf decoded-frames)
		    (with-resampled-frame (frame-out swr-ctx-mgr frame-in sample-rate) 
		      (with-encoded-packet (p-codec-context-out stream-type packet-out frame-out)
			(incf encoded-packets)
					;(format t "at frame:~a~%" encoded-packets) 
			(av-interleaved-write-frame p-format-context-out packet-out)))))
		(av-write-trailer p-format-context-out) 
		(format t "frames-count:~a: packet-count:~a codec-context-in -> channels:~a sample-rate:~a channel_layout:~a~%" decoded-frames encoded-packets (codec-context-channels p-codec-context-in) (codec-context-sample-rate p-codec-context-in)  (codec-context-channel-layout p-codec-context-in))))))))))

(defparameter *ring-buffer-size* 65536)
(defparameter *out-file-path* nil)

(defun file-write()
   ;(with-open-output-file (*out-file-path* p-format-context-out)
)

(defun ffmpeg-transcode(in-file-path output-file-path &optional (stream-type :avmedia-type-audio) (sample-rate 44100))
  (av-register-all)
  (let ((ret (av-lockmgr-register (callback my-lock-mgr))))
    (unless (= ret 0) (error 'ffmpeg-fault :code ret :msg "could not register lock manager")))
  (with-foreign-ring-buffer (buffer *ring-buffer-size* :element-type :uint)  
    (with-thread ("FFMPEG-READER" 
		  ((*out-file-path* output-file-path) *debug-lock-mgr* *thread-control* (*ring-buffer buffer) *ring-buffer-size*)
		  (file-write))
      (with-swr-context-mgr swr-ctx-mgr
	(let ((frames 0))
	  (in-decoded-frame-read-loop (frame-in in-file-path stream-type)
	    (incf frames)
	    (with-resampled-frame (frame-out swr-ctx-mgr frame-in sample-rate)
	      (write-to-buffer buffer frame-out)
	      (format t "frames-count:~a~%" frames))))))))

(defun test-ffmpeg()
  (ffmpeg-transcode "/mnt/MUSIC-THD/test.hd.mp4" "/mnt/MUSIC-THD/dummy.wav"))

(defun volume-test()
  (dotimes (x 20) (progn (test-ffmpeg-serial) (format t "time #~a~%" x))))

(defun run())

(defmacro with-users(&body body)
  (with-gensyms(stream)
    `(let ((,stream *standard-output*))
       (macrolet ((use-case(msg)
		    `(format ,',stream (% "~a~%" ,msg))))
	 (format ,stream "OPENING HERE~%")
	 (unwind-protect
	      (progn
		,@body)
	   (format ,stream "CLOSING DOWN#1~%")
	   (format ,stream "CLOSING DOWN#2~%"))))))

       
			       
	 
	  