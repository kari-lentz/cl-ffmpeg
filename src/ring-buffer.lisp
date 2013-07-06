(in-package :ring-buffer)

(defun make-ring-buffer(size &key (element-type '(usigned-byte 8)))
  (let ((!buffer (make-array size :element-type element-type))
	(!read-ptr 0)
	(!write-ptr 0)
	(!size size)
	(!full-p))
    (flet ((get-byte-count()
	     (if !full-p
		 !size
		 (mod (- !write-ptr !read-ptr) !size))))
      (dlambda 
	(:write (value) 
	       (unless (<= (- !size (get-byte-count)) 0)
		 (setf (aref !buffer !write-ptr) value)
		 (setf !write-ptr (mod (1+ !write-ptr) !size))
		 (unless (< (get-byte-count) !size) (setf !full-p t))))
	(:read ()
	      (unless (<= (get-byte-count) 0)
		(let ((ret (aref !buffer !read-ptr)))
		  (setf !read-ptr (mod (1+ !read-ptr) !size))
		  (setf !full-p nil)
		  ret)))))))

(defctype size-t :int)

(defcfun memcpy :pointer
  (dest :pointer)
  (src :pointer)
  (n size-t))

(defun memcpy-params(!size begin size)
  (with-collector (!push)
    (if (<= (+ begin size) !size)
	(!push `(,begin 0 ,size))
	(progn
	  (let ((delta (- !size begin)))
	    (!push `(,begin 0 ,delta))
	    (!push `(0 ,delta ,(- size delta))))))))

(defmacro memcpy-call(dest-buffer dest-idx src-buffer src-idx size)
  (flet ((foreign-buffer(buffer idx)
	   `(mem-aptr ,buffer :uint8 ,idx)))
  `(memcpy ,(foreign-buffer dest-buffer dest-idx) ,(foreign-buffer src-buffer src-idx) ,size)))

(defun foreign-byte-size(size element-type)
  (* (foreign-type-size element-type) size))

(defmacro loop-down((size segment control) &body body)
  (with-once-only (size segment)
    (with-gensyms (remaining)
      `(do ((,remaining ,size)) ((<= ,remaining 0))
	 (let ((,control (min ,remaining ,segment)))
	   (decf ,remaining ,control)
	   ,@body)))))

(defun make-foreign-ring-buffer(size &key (element-type :uint8))
  (let ((!buffer (foreign-alloc element-type :count size))
	(!read-ptr 0)
	(!write-ptr 0)
	(!size (foreign-byte-size size element-type))
	(!element-type element-type)
	(!full-p)
	(!error-p)
	(!eof-p)
	(!lock (make-lock))
	(!full-state (make-condition-variable))
	(!empty-state (make-condition-variable)))
    (flet ((get-fill-count()
	     (if !full-p
		 !size
		 (mod (- !write-ptr !read-ptr) !size))))
      (flet ((get-room-count()
	       (- !size (get-fill-count))))

	(flet ((buffer-write(buffer size)
		 (with-lock-held (!lock)
		   (loop while (> size (get-room-count)) do
			(when !error-p (error "Encounter buffer error in read process"))
			(condition-wait !full-state !lock)))
			  
		 (loop for (dest-idx src-idx size) in (memcpy-params !size !write-ptr size)
		    do
		      (memcpy-call !buffer dest-idx buffer src-idx size))
			  
		 (with-lock-held (!lock)
		   (setf !write-ptr (mod (+ !write-ptr size) !size))
		   (when (= !write-ptr !read-ptr)
		     (setf !full-p t))
		   (condition-notify !empty-state)))
	       (buffer-read(buffer size)
		 (with-lock-held (!lock) 
		   (loop while (> size (get-fill-count)) do
			(when !error-p (error "Encountered buffer error in read process"))
			(when !eof-p (error "Encountered eof in read process"))
			(condition-wait !empty-state !lock)))
		 
		 (loop for (src-idx dest-idx size) in (memcpy-params !size !read-ptr size)
		    do
		      (memcpy-call buffer dest-idx !buffer src-idx size))
			    
		 (with-lock-held (!lock)
		   (setf !read-ptr (mod (+ !read-ptr size) !size))
		   (setf !full-p nil)
		   (condition-notify !full-state))))
	       
	  (dlambda 
	       
	    (:write (buffer size)
		    (block process
		      (unless (> size 0)(return-from process))
		      (let ((size (foreign-byte-size size !element-type))(idx 0))
			(loop-down (size !size request-size)
			   (buffer-write (mem-aptr buffer :uint8 idx) request-size)
			   (incf idx request-size)))))
					      
	    (:read (buffer size)
		   (block process
		     (unless (> size 0) (return-from process))
		     (let ((size (foreign-byte-size size !element-type))(idx 0))
		       (loop-down (size !size request-size)
			  (buffer-read (mem-aptr buffer :uint8 idx) request-size)
			  (incf idx request-size)))))

	    (:set-eof ()
		      (with-lock-held (!lock)
			(setf !eof-p t)
			(condition-notify !full-state)
			(condition-notify !empty-state)))
	    (:set-error ()
			(with-lock-held (!lock)
			  (condition-notify !full-state)
			  (condition-notify !empty-state)
			  (setf !error-p t)))
	    (:destroy ()
		      (release-lock !lock)
		      (foreign-free !buffer))))))))

(defparameter *thread-id* 0)

(defun my-thread()
  (loop for idx from 1 to 500 do
       (format *standard-output* "THIS IS IT:~a:~a~%" idx *thread-id*)))

(defun foo()
  (macroexpand-1
   '(with-thread ("MY-NEXT-THREAD"
		  (*standard-output* (*thread-id* 2))
		  (my-thread))
     (format t "STARTED UP A THREADs~%"))))

(defun test-thread()
  (let ((*thread-id* 1))
    (with-thread ("MY-THREAD" 
		  (*thread-id*) 
		  (my-thread))
      (with-thread ("MY-NEXT-THREAD"
		    ((*thread-id* 2))
		    (my-thread))
	(format t "STARTED UP A THREADs~%")))))

(defun make-iter(&optional (init 0) (delta 1))
  (dlambda 
    (:inc ()
	  (post-fix init
		    (incf init delta)))))

(defmacro with-foreign-ring-buffer((buffer size &key (element-type :uint8)) &body body)
  `(let ((,buffer (make-foreign-ring-buffer ,size :element-type ,element-type)))
     ,@body
     (funcall ,buffer :destroy)))

(defun test-buffer(&key (freq 1) (message-length 4096) (buffer-length 1024) (delay-reader-p) (delay-writer-p))
  (with-foreign-ring-buffer (my-buffer buffer-length :element-type :int)
    (with-thread ("WRITE-PROCESS" 
		  () 
		  (let ((buffer-length message-length)(iter (make-iter)))
		    (with-foreign-object (buffer :int buffer-length)
		      (loop for n from 1 to freq do
			   (when delay-writer-p (sleep 3))
			   (for-each-range (idx buffer-length)
			     (setf (mem-aref buffer :int idx) (funcall iter :inc)))
			   (funcall my-buffer :write buffer buffer-length))
		      (funcall my-buffer :set-eof))))
      (let ((buffer-length message-length)) 
	(with-foreign-object (buffer :int buffer-length)
	  (loop for n from 1 to freq do
	       (when delay-reader-p (sleep 3))
	       (funcall my-buffer :read buffer buffer-length)
	       (for-each-range (idx buffer-length)
		 (format t "~a:~a~%" idx (mem-aref buffer :int idx)))))))))

(defun kill-write-processes()
  (loop for thread in (bordeaux-threads:all-threads) when (~ "WRITE-PROCESS" (thread-name thread))
       do 
       (format t "~a~%" thread)
       (destroy-thread thread)))
       
(defun run())
