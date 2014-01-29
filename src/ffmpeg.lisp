(in-package :cl-ffmpeg)

(defmacro with-struct-readers((&rest slots) instance type &body body)
  (with-once-only (instance)
    `(let ,(loop for (slot-outer slot-inner) in (ensure-pairs slots) collecting
		`(,slot-outer (,(.sym type '- slot-inner) ,instance)))
       ,@body)))

(defmacro !with-struct-readers((&rest slots) instance type &body body)
  (with-once-only(instance)
    `(macrolet 
	 ,(loop for (slot-outer slot-inner) in (ensure-pairs slots) 
	     collecting
	       `(,slot-outer
		 ()
		 `(,(.sym ',type '- ',slot-inner) ,',instance)))
       ,@body)))
         
(defstruct (video-frame (:constructor video-frame (width height &key (mmtime (media-time)) played-p skipped-p (frame-data (foreign-alloc :unsigned-char :count (* 2 width height)))))) mmtime played-p skipped-p width height frame-data)

(defun destroy-video-frame(frame)
  (with-slots (frame-data) frame
    (foreign-free frame-data)
    (setf frame-data nil)))

(defmacro with-anchored-video-frame((data linesize) frame &body body)
  (with-gensyms (buffer width height av-picture)
    `(with-struct-readers ((,buffer frame-data) (,width width) (,height height)) ,frame video-frame 
       (with-foreign-object (,av-picture '(:struct av-picture)) 
	 (anchor-picture ,av-picture ,width ,height ,buffer)
	 (with-cffi-readers ((,data data) (,linesize linesize)) av-picture ,av-picture 
	   ,@body)))))

(defmacro with-foreign-array-to-lisp((foreign-array foreign-type lisp-array) &body body)
  (with-once-only (foreign-array lisp-array)
    (with-gensyms (length idx)
      `(let ((,length (length ,lisp-array)))
	   (dotimes (,idx ,length)
	     (setf (aref ,lisp-array ,idx) (mem-aref ,foreign-array ,foreign-type ,idx)))
	   ,@body))))

(defmacro with-lisp-to-foreign-array((lisp-array foreign-array foreign-type) &body body)
  (with-once-only (foreign-array lisp-array)
    (with-gensyms (length idx)
      `(let ((,length (length ,lisp-array)))
	   (dotimes (,idx ,length)
	     (setf (mem-aref ,foreign-array ,foreign-type ,idx) (aref ,lisp-array ,idx)))
	   ,@body))))
  
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
     (setf (AVPAcket-data ,av-packet) (null-pointer))
     (unwind-protect
	  (progn
	    ,@body)
       (av-free-packet ,av-packet))))

(defstruct (decode-context (:constructor decode-context (format-context stream-idx codec codec-context media-type))) format-context stream-idx codec codec-context media-type)

(defun run-read-frame-loop(decode-context packet-user)
  (with-struct-readers (format-context stream-idx) decode-context decode-context
    (loop
       (with-av-packet packet
	 (unless (= (av-read-frame format-context packet) 0) 
	   (return))
	 (if (= stream-idx (AVPacket-stream-index packet))
	     (funcall packet-user packet))))))

(defun run-decoded-frame(decode-context frame packet decoded-frame-user)
  (with-struct-readers (codec-context media-type) decode-context decode-context
    (with-foreign-object (p-got-frame-ptr :int) 
      (let ((packet-size (AVPacket-size packet)))
	(let ((ret (foreign-funcall-pointer ([] *decoders* media-type) () :pointer codec-context :pointer frame :pointer p-got-frame-ptr :pointer packet :int)))
	  (unless (= ret packet-size) (error 'ffmpeg-fault :msg (% "decode fault -> decoded bytes:~a expected bytes~a" ret packet-size)))
	  (unless (= (mem-ref p-got-frame-ptr :int) 0)
	    (funcall decoded-frame-user frame)))))))

(defmacro with-open-input((p-format-context file-path ) &body body)
  (with-gensyms (pp-format-context)
    `(with-foreign-objects ((,pp-format-context :pointer))
       (setf (mem-ref ,pp-format-context :pointer) (null-pointer))
       (avformat-open-input ,pp-format-context ,file-path (null-pointer) (null-pointer))
       (let ((,p-format-context (mem-ref ,pp-format-context :pointer)))
	 (if (not (null-pointer-p ,p-format-context))
	     (progn
	       (unwind-protect
		    (progn
		      ,@body)
		 (avformat-close-input ,pp-format-context))))))))

(defun run-input-stream(file-path media-type decode-context-user)
  (with-open-input (p-format-context file-path)
    (avformat-find-stream-info p-format-context (null-pointer))
    (with-foreign-object (pp-codec :pointer)
      (setf (mem-ref pp-codec :pointer) (null-pointer))
      (let ((stream-idx (av-find-best-stream p-format-context (foreign-enum-value 'avmedia-type media-type) -1 -1 pp-codec 0)))
	(let ((p-codec-context (get-codec-context p-format-context stream-idx))(p-codec (mem-ref pp-codec :pointer)))
	  (open-codec-2 p-codec-context p-codec)
	  (funcall decode-context-user (decode-context p-format-context stream-idx p-codec p-codec-context media-type)))))))

(defun run-decoded-frame-read-loop(file-path stream-type frame-user)
  (with-av-frame frame
    (run-input-stream 
      file-path 
      stream-type 
      (lambda(decode-context)		 
	(run-read-frame-loop 
	 decode-context
	 (lambda(packet)
	   (run-decoded-frame 
	    decode-context 
	    frame 
	    packet
	    (lambda (frame) (funcall frame-user frame)))))))))
    
(defmacro with-open-codec((codec-context codec) &body body)
  `(progn
     (avcodec-open2 ,codec-context ,codec (null-pointer))
     (unwind-protect
	  (progn
	    ,@body)
       (avcodec-close ,codec-context))))

(defun open-codec-2(codec-context codec)
  (avcodec-open2 codec-context codec (null-pointer)))
       		 
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
     (let ((,codec-context (avcodec-alloc-context3 ,codec)))
       (when (null-pointer-p ,codec-context) (error 'ffmpeg-fault :msg (% "failed to open codec context:~a" ,name)))
       (unwind-protect
	    (progn
	      ,@body)
	 (avcodec-close ,codec-context)
	 (avfreep ,codec-context))))) 

(defmacro with-new-audio-encoder((codec-context name &key (sample-rate 44100) (num-channels 2) bit-rate) &body body)
  (with-gensyms (codec)
    `(with-new-encoder (,codec-context ,codec ,name)
       (set-audio-params ,codec-context ,num-channels ,sample-rate ,bit-rate)
       (when ,bit-rate (setf (codec-context-bit-rate ,codec-context) ,bit-rate))
       (with-open-codec (,codec-context ,codec) 
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
  (with-gensyms (pp-format-context)
    (with-once-only (file-path)
      `(with-foreign-objects ((,pp-format-context :pointer))
	 (setf (mem-ref ,pp-format-context :pointer) (null-pointer))
	 (format t "avformat-alloc:~a~%" (avformat-alloc-output-context2 ,pp-format-context (null-pointer) (null-pointer) ,file-path))
	 (let ((,p-format-context (mem-ref ,pp-format-context :pointer)))
	   (if (not (null-pointer-p ,p-format-context))
	       (unwind-protect
		    (progn
		      ,@body)
		 (avformat-free-context ,p-format-context))))))))

(defmacro with-open-output-file((file-path format-context) &body body)
  (with-once-only (file-path)
    (with-gensyms (p-io-context)
      `(with-foreign-object (,p-io-context :pointer)
	 (ensure-file-gone ,file-path)
	 (setf (mem-ref ,p-io-context :pointer) (null-pointer))
	 (avio-open ,p-io-context ,file-path 2)
	 (setf (format-context-pb ,format-context) (mem-ref ,p-io-context :pointer))
	 (unwind-protect
	      (progn
		,@body)
	   (avio-close (format-context-pb ,format-context)))))))

(defmacro with-output-sink((format-context file-path) &body body)
  `(with-format-context(,format-context ,file-path)
     (with-open-output-file (,file-path ,format-context)
       ,@body)))

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
	     
(defmacro with-buffered-frame((frame-out nb-samples &key (rate 44100) (num-channels 2) user-data) &body body)
  `(with-av-frame ,frame-out
     (setf (avframe-overlay-nb-samples ,frame-out) ,nb-samples)
     (av-frame-set-channel-layout ,frame-out (av-get-default-channel-layout ,num-channels))
     (av-frame-set-channels ,frame-out ,num-channels)
     (av-frame-set-sample-rate ,frame-out ,rate) 
     (setf (avframe-overlay-format ,frame-out) (foreign-enum-value 'avsample-format :av-sample-fmt-s16))
     ,@(if user-data
	   `((setf (mem-ref (avframe-overlay-data ,frame-out) :pointer) ,user-data)
	     (setf (mem-ref (avframe-overlay-extended-data ,frame-out) :pointer) ,user-data))
	   `((av-frame-get-buffer ,frame-out 0)))
     ,@body))
 
(defmacro with-resampled-frame((frame-out swr-ctx-mgr frame-in &optional (rate 44100) (num-channels 2)) &body body)
  (with-gensyms(nb-samples swr-ctx)
    `(let ((,nb-samples (ceiling  (/ (* ,rate (avframe-overlay-nb-samples ,frame-in)) (av-frame-get-sample-rate ,frame-in)))))
      (let ((,swr-ctx (acquire-swr-context ,swr-ctx-mgr (av-get-default-channel-layout ,num-channels) ,rate (av-frame-get-channel-layout ,frame-in) (AVFrame-Overlay-format ,frame-in) (av-frame-get-sample-rate ,frame-in))))
	(with-buffered-frame (,frame-out ,nb-samples :rate ,rate :num-channels ,num-channels)
	  (setf (avframe-overlay-nb-samples ,frame-out) (swr-convert ,swr-ctx (avframe-overlay-data ,frame-out) ,nb-samples (avframe-overlay-data ,frame-in) (avframe-overlay-nb-samples ,frame-in)))
	  ,@body)))))

(defun scale-frame(frame-out sws-context frame-in)
  (with-cffi-readers ((in-width width) (in-height height) (in-data data) (in-linesize linesize) format) AVFrame-Overlay frame-in
     (with-struct-readers ((out-width width)(out-height height)) frame-out video-frame
       (with-anchored-video-frame (out-data out-linesize) frame-out
	 (let ((sws-context (sws-get-cached-context sws-context in-width in-height format out-width out-height :AV-PIX-FMT-YUV420P 0 (null-pointer) (null-pointer) 0)))
	   (sws-scale sws-context in-data in-linesize 0 in-height out-data out-linesize))))))

(defun write-to-buffer(ring-buffer frame)
  (funcall ring-buffer :write (mem-ref (avframe-overlay-data frame) '(:pointer (:struct audio-frame))) (avframe-overlay-nb-samples frame)))

(defun read-from-buffer(ring-buffer frame)
  (let ((ret (funcall ring-buffer :read (mem-ref (avframe-overlay-data frame) '(:pointer (:struct audio-frame))) (avframe-overlay-nb-samples frame))))
    (setf (avframe-overlay-nb-samples frame) ret)))

(defgeneric run-ffmpeg-in(audio-params in-device)) 
(defgeneric run-ffmpeg-out(audio-params out-device)) 

(defmethod run-ffmpeg-in((audio-params audio-params) (in-device pathname))
  (with-slots (sample-rate num-channels (stream-type media-type) (buffer ring-buffer)) audio-params
    (with-swr-context-mgr swr-ctx-mgr
      (let ((frames 0))
	(run-decoded-frame-read-loop 
	 (namestring in-device) 
	 stream-type
	 (lambda(frame-in)
	   (incf frames)
	   (with-resampled-frame (frame-out swr-ctx-mgr frame-in sample-rate num-channels)
	     (write-to-buffer buffer frame-out))))))))

(defmethod run-ffmpeg-in((video-params video-params) (in-device pathname))
  (with-slots ((stream-type media-type) (buffer ring-buffer)) video-params
    (let ((sws-ctx (null-pointer)))
      (let ((frames 0))
	(run-decoded-frame-read-loop 
	 (namestring in-device) 
	 stream-type
	 (lambda(frame-in)
	   (flet ((frame-writer(video-frames count)
		    (for-each-range (idx count)
		      (let ((video-frame (aref video-frames idx)))
			(scale-frame video-frame sws-ctx frame-in)
			(incf frames)))))
	     (funcall :write-period buffer #'frame-writer))))))))

(defun buffered-reader(p-format-context-out p-codec-context-out num-channels sample-rate stream-type encoded-packets)
  (lambda(buffer num-samples)
    (with-cffi-ptrs ((buffer '(:pointer (:struct audio-frame))))
      (with-buffered-frame (frame-out num-samples :rate sample-rate :num-channels num-channels :user-data (&buffer 0))
	(with-encoded-packet (p-codec-context-out stream-type packet-out frame-out)
	  (av-interleaved-write-frame p-format-context-out packet-out)
	    (incf encoded-packets)
	    num-samples)))))
      
(defmethod run-ffmpeg-out((audio-params audio-params) (out-device pathname))
  (let ((encoded-packets 0))
    (with-slots (ring-buffer sample-rate num-channels (stream-type media-type) num-periods output-buffer-size) audio-params
      (let ((period-size (/ output-buffer-size num-periods)))
	(with-output-sink (p-format-context-out (namestring out-device))
	  (with-audio-encoder (p-codec-context-out p-format-context-out stream-type :num-channels num-channels :sample-rate sample-rate)
	    (avformat-write-header p-format-context-out (null-pointer))
	    (flet ((buffered-reader(buffer num-samples)
		     (with-cffi-ptrs ((buffer '(:pointer (:struct audio-frame))))
		       (with-buffered-frame (frame-out num-samples :rate sample-rate :num-channels num-channels :user-data (&buffer 0))
			 (with-encoded-packet (p-codec-context-out stream-type packet-out frame-out)
			   (av-interleaved-write-frame p-format-context-out packet-out)
			   (incf encoded-packets)
			   num-samples)))))
	      (loop
		 (let ((ret (funcall ring-buffer :read-period #'buffered-reader)))
		   (unless (= ret period-size) (return))))
	      (format t "WRITING TRAILER!!!~%")
	      (av-write-trailer p-format-context-out))))))))

(defmacro with-audio-buffer((buffer &key (ring-buffer-size 65536) (num-periods 2)) &body body)
  `(with-foreign-ring-buffer (,buffer ,ring-buffer-size :num-periods ,num-periods :element-type '(:struct audio-frame))
     ,@body))

(defun run-audio-buffer(ffmpeg-env closure)
  (with-slots (ring-buffer (ring-buffer-size output-buffer-size) num-periods) ffmpeg-env
    (with-foreign-ring-buffer (buffer ring-buffer-size :num-periods num-periods :element-type '(:struct audio-frame))
      (funcall closure buffer))))

(defun run-video-buffer(ffmpeg-env closure)
  (with-slots (ring-buffer (ring-buffer-size output-buffer-size) num-periods width height) ffmpeg-env
    (with-ring-buffer (buffer ring-buffer-size :num-periods num-periods :element-type 'video-frame :element-factory (lambda()(video-frame width height)))
      (funcall closure buffer))))

(defparameter *run-buffer-functions* (hash-table-from-list `((:avmedia-type-audio ,#'run-audio-buffer)(:avmedia-type-video ,#'run-video-buffer)) 'eq))

(defmacro with-sdl-window((&key (width 320) (height 240) (event-type :wait)) &body event-body)
  (with-gensyms (window-block)
    `(block ,window-block
       (sdl:with-init (sdl:sdl-init-video)
	 (sdl:window ,width ,height)
	 (sdl:with-events (,event-type)
	   ,@event-body
	   (:quit-event () (return-from ,window-block)))))))

(defun sdl-blocker(&optional (width 320) (height 240))
  (with-sdl-window (:width width :height height)
    (:key-down-event (:key key :mod-key mod-keys)
		     (when 
			 (and 
			  (or (find :sdl-key-mod-lctrl mod-keys) (find :sdl-key-mod-rctrl mod-keys))
			  (eq key :sdl-key-x))
		       (sdl:push-quit-event)))))

(defun run-ffmpeg(ffmpeg-env in-device out-device)
  (with-slots (ring-buffer output-buffer-size num-periods media-type) ffmpeg-env
    (with-ffmpeg ()
      (funcall 
       ([] *run-buffer-functions* media-type) 
       ffmpeg-env
       (lambda(buffer)
	 (setf ring-buffer buffer)
	 (with-thread ("FFMPEG-WRITER"
		       ()
		       (with-thread ("FFMPEG-READER" 
				     ()
				     (block writer
				       (handler-bind
					   ((condition (lambda(c) (funcall buffer :set-error c) (return-from writer))))
					 (run-ffmpeg-out ffmpeg-env out-device))))
			 (handler-bind 
			     ((user-eof (lambda(c) (declare (ignore c)) (invoke-restart 'windup))))
			   (restart-case
			       (unwind-protect
				    (run-ffmpeg-in ffmpeg-env in-device)
				 (funcall buffer :set-eof)
				 (sdl:push-quit-event))
			     (windup()
			       (format t "USER TERMINATION~%"))))))
	   (sdl-blocker)
	   (funcall buffer :set-eof)))))))

(defun ffmpeg-transcode(ffmpeg-env in-file-path output-file-path)
  (with-ffmpeg ()
    (run-ffmpeg ffmpeg-env (pathname in-file-path) (pathname output-file-path)))) 

(defun test-ffmpeg(&optional (pf "/mnt/MUSIC-THD/test.hd.mp4"))
  (ffmpeg-transcode (audio-params) pf "/mnt/MUSIC-THD/dummy.wav"))

(defun volume-test(&optional (freq 20))
  (dotimes (x freq) (progn (test-ffmpeg) (format t "time #~a~%" x))))

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

(defun kill-ffmpeg()
  (loop for thread in (bordeaux-threads:all-threads) 
     when (~ "FFMPEG-" (bordeaux-threads:thread-name thread))
     do 
       (format t "~a~%" thread)
       (bordeaux-threads:destroy-thread thread)))

(defun test-cffi!()
  (macroexpand
   '(defcfun* ("avformat_open_input" avformat-open-input) :ffmpeg-int
     (ps :pointer)
     (filename :string)
     (fmt :pointer)
     (options :pointer))))

(defun test-cffi(&optional (file-name "/mnt/MUSIC-THD/test.hd.mp4"))
  (av-register-all)
  (with-foreign-objects ((pp-format-context :pointer))
    (setf (mem-ref pp-format-context :pointer) (null-pointer))
    (avformat-open-input pp-format-context file-name (null-pointer) (null-pointer)))) 
	 
(defun view-locks()
  (hash-table-contents (thread-control-foreign-lock-hash-table *thread-control*)))

(defun save-image()
  (sb-ext:save-lisp-and-die "/home/klentz/ffmpeg-server" :executable t))

(defun test-multiple()
  (loop for pf in (directory "/home/klentz/test/video/*.mp4") do
       (let ((pf (namestring pf)))
	 (format t "processing ~a" pf)
	 (test-ffmpeg pf) 
	 (format t "processed ~a" pf))))

  