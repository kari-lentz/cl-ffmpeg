(in-package :cl-ffmpeg)

(define-foreign-library libavformat
  (:unix (:or "/usr/local/lib/libavformat.so"))
  (t (:default "libavformat")))

(use-foreign-library libavformat)

(define-foreign-library libavcodec
  (:unix (:or "/usr/local/lib/libavcodec.so"))
  (t (:default "libavcodec")))

(use-foreign-library libavcodec)

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
		   `(foreign-slot-value ,ptr ,'',type ,'',slot-name)))))))

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

(define-condition ffmpeg-fault(error)
  ((msg :initarg :msg :reader msg)
   (code :initform nil :initarg :code :reader code))
  (:report (lambda(c stream)(if (code c) 
				(format stream "ffmpeg lib:~a error-code:~a" (msg c) (code c))
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

(defcfun ("avcodec_find_encoder_by_name" avcodec-find-encoder-by-name) :pointer
  (name  :string))	

(define-dummy-enums AVCodec-ID AVPixel-Format AVColor-Primaries AVColor-Transfer-Characteristic AVColor-Space AVColor-Range AVChroma-Location AVField-Order AVSample-Format AVAudio-Service-Type) 
     
(defcstruct* (AVCodec-Context-Overlay)
 (av-class :pointer)
 (log-level-offset :int)
 (codec-type avmedia-type)
 (codec :pointer)
 (codec-name :char :count 32)
 (codec-id AVCodec-ID)
 (codec-tag :unsigned-int)
 (stream-codec-tag :unsigned-int)
 (priv-data :pointer)
 (internal :pointer)
 (opaque :pointer)
 (bit-rate :int)
 (bit-rate-tolerance :int)
 (global-quality :int)
 (compression-level :int)
 (flags :int)
 (flags2 :int)
 (extradata :pointer)
 (extradata-size :int)
 (timebase AVRational)
 (ticks-per-frame :int)
 (delay :int)
 (width :int)
 (height :int)
 (coded-width :int) 
 (coded-height :int)
 (gop-size :int)
 (pix-fmt AVPixel-Format)
 (me-method :int)
 (draw-horiz-band :pointer)
 (get-format :pointer)
 (max-b-frames :int) 
 (b-quant-factor :float)
 (rc-strategy :int)
 (b-frame-strategy :int)
 (b-quant-offset :float)
 (has-b-frames :int)
 (mpeg-quant :int)
 (i-quant-factor :float)
 (i-quant-offset :float)
 (lumi-masking :float) 
 (temporal-cplx-masking :float)
 (spatial-cplx-masking :float)
 (p-masking :float)
 (dark-masking :float)
 (slice-count :int)
 (prediction-method :int)
 (slice-offset :pointer)
 (sample-aspect-ratio AVRational)
 (me-cmp :int)
 (me-sub-cmp :int)
 (mb-cmp :int)
 (ildct-cmp :int)
 (dia-size :int)
 (last-predictor-count :int)
 (pre-me :int)
 (me-pre-cmp :int)
 (pre-dia-size :int)
 (me-subpel-quality :int)
 (dtg-active-format :int)
 (me-range :int)
 (intra-quant-bias :int)
 (inter-quant-bias :int)
 (slice-flags :int) 
 (xvmc-acceleration :int)
 (mb-decision :int)
 (intra-matrix :pointer)
 (inter-matrix :pointer)
 (scenechange-threshold :int)
 (noise-reduction :int)
 (me-threshold :int)
 (mb-threshold :int) 
 (intra-dc-precision :int)
 (skip-top :int)
 (skip-bottom :int)
 (border-masking :float)
 (mb-lmin :int)
 (mb-lmax :int)
 (me-penalty-compensation :int)
 (bidir-refine :int)
 (brd-scale :int)
 (keyint-min :int)
 (refs :int)
 (chromaoffset :int)
 (scenechange-factor :int)
 (mv0-threshold :int)
 (b-sensitivity :int)
 (color-primaries AVColor-Primaries) 	
 (color-trc AVColor-Transfer-Characteristic) 	
 (colorspace AVColor-Space) 
 (color-range AVColor-Range) 
 (chroma-sample-location AVChroma-Location)
 (slices :int)
 (field-order AVField-Order)
 (sample-rate :int)
 (channels :int)
 (sample-fmt AVSample-Format) 
 (frame-size :int) 
 (frame-number :int)
 (block-align :int)
 (cutoff :int)
 (channel-layout :uint64)
 (request-channel-layout :uint64)
 (audio-service-type AVAudio-Service-Type) 	
 (request-sample-fmt AVSample-Format))

(defparameter *decoders* ({} (:avmedia-type-audio (foreign-symbol-pointer "avcodec_decode_audio4"))(:avmedia-type-video (foreign-symbol-pointer "avcodec_decode_video2"))))

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
  `(with-foreign-object(,av-packet 'AVPacket)
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
	(format t "stream-idx:~a:~a~%" ,stream-idx (AVPacket-stream-index ,packet))
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

(defmacro with-decoded-frame!((codec-context stream-type frame packet) &body body)
  (declare (ignore stream-type))
  (with-gensyms (p-got-frame-ptr ret packet-size)
    `(with-foreign-object (,p-got-frame-ptr :int) 
       (let ((,packet-size (AVPacket-size ,packet)))
	 (format t "packet-size:~a~%" ,packet-size)
	 (let ((,ret (avcodec-decode-audio4 ,codec-context ,frame ,p-got-frame-ptr ,packet)))
	   (unless (= ,ret ,packet-size) (error 'ffmpeg-fault :msg (% "decode fault -> decoded bytes:~a expected bytes~a" ,ret ,packet-size)))
	   (unless (= (mem-ref ,p-got-frame-ptr :int) 0)
	     ,@body))))))

;; (defmacro with-codec-context(context codec &body body)
;;     `(with-av-pointer ,context (avcodec-alloc-context3 ,codec)
;;        (unwind-protect
;; 	(progn
;; 	  ,@body)
;; 	 (avcodec-close ,context))))

(defmacro with-open-input((p-format-context file-path ) &body body)
  (with-gensyms (pp-format-context c-file-path ret)
    `(with-foreign-string (,c-file-path ,file-path)
       (with-foreign-objects ((,pp-format-context :pointer))
	 (setf (mem-ref ,pp-format-context :pointer) (null-pointer))
	 (let ((,ret (avformat-open-input ,pp-format-context ,c-file-path (null-pointer) (null-pointer))))
	   (unless (= ,ret 0) (error 'ffmpeg-fault :msg (% "unable to open:~a" ,file-path) :code ,ret))
	   (let ((,p-format-context (mem-ref ,pp-format-context :pointer)))
	     (if (not (null-pointer-p ,p-format-context))
		 (progn
		   (unwind-protect
			(progn
			  ,@body)
		     (avformat-close-input ,pp-format-context))))))))))

(defmacro with-open-codec((codec-context codec) &body body)
  (with-gensyms (ret)
    `(let ((,ret (avcodec-open2 ,codec-context ,codec (null-pointer))))
       (unless (= ,ret 0) (error 'ffmpeg-fault :msg "codec open fault" :code ,ret))
       (unwind-protect
	    (progn
	      ,@body)
	 (avcodec-close ,codec-context)))))

(defun open-codec-2(codec-context codec)
  (let ((ret (avcodec-open2 codec-context codec (null-pointer))))
       (unless (= ret 0) (error 'ffmpeg-fault :msg "codec open fault" :code ret))))
       		 
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

(defmacro with-encoder((codec-context name) &body body)
  (with-gensyms (codec)
    `(let ((,codec (avcodec-find-encoder-by-name ,name)))
       (when (null-pointer-p ,codec) (error 'ffmpeg-fault :msg (% "codec ~a not found" ,codec)))
       (with-av-pointer ,codec-context (avcodec-alloc-context3 ,codec)
	 (unwind-protect
	      (progn
		,@body)
	   (avcodec-close ,codec-context))))))

(defun set-audio-params(codec-context &optional (channels 2) (sample-rate 44100))
  (setf (AVCodec-Context-Overlay-channels  codec-context) channels)
  (setf (AVCodec-Context-Overlay-sample-rate codec-context) sample-rate))

(defun test-ffmpeg!(&optional (stream-type :avmedia-type-audio))
  (av-register-all)
  (let ((frames 0))
    (with-input-stream (p-format-context p-codec-context stream-idx "/mnt/MUSIC-THD/test.hd.mp4" stream-type)
      (with-av-frame frame
	(in-frame-read-loop p-format-context stream-idx packet
	  (incf frames)
	  (format t "frame-count:~a~%" frames))
	(format t "frames-count:~a decode-context:~a~%" frames p-codec-context)))))

(defun test-ffmpeg(&optional (stream-type :avmedia-type-audio))
  (av-register-all)
  (let ((frames 0))
    (with-input-stream (p-format-context-in p-codec-context-in stream-idx "/mnt/MUSIC-THD/test.hd.mp4" stream-type)
      (with-av-frame frame
	(in-frame-read-loop p-format-context-in stream-idx packet-in
	  (with-decoded-frame (p-codec-context-in stream-type frame packet-in)
	    (incf frames)
	    (format t "frame-count:~a~%" frames)))
	(format t "frames-count:~a decode-context:~a~%" frames p-codec-context-in)))))


(defun run())

(defmacro defcstruct*!(name-and-options &body fields)
  (let ((type (get-first-atom name-and-options)))
    `(progn
       ,(loop for item in fields collecting 
	     (let ((slot-name (get-first-atom item)))
	       `(defun ,(.sym (get-first-atom name-and-options) '- (get-first-atom item)) (ptr) 
		  (foreign-slot-value ptr (quote ,type) (quote ,slot-name))))))))   

(defun test-cstruct*()
  (macroexpand-1
   `(defcstruct* AVPacket
     (buf :pointer)
     (pts :int64)
     (dts :int64)
     (data :pointer)
     (size :int)
     (stream-index :int)
     (flags :int)
     (side-data AvPacket-Side-Data)
     (side-data-elems :int)
     (duration :int)
     (pos :int64)
     (convergence-duration :int64))))


