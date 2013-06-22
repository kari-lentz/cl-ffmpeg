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
    (unwind-protect
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
			     (when !error-p (error "Encounter buffer error in write process"))
			     (when !eof-p (error "Encounter eof in write process"))
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
			       (setf !error-p t)))))))
      
      (release-lock !lock))))

(defparameter *thread-id* 0)

(defun my-thread()
  (loop for idx from 1 to 1000 do
       (format *standard-output* "THIS IS IT:~a:~a~%" idx *thread-id*)))

(defun test-thread!()
  (make-thread #'my-thread :initial-bindings `((*standard-output* . ,*standard-output*))))

(defmacro with-thread((name bindings thread-body) &body body) 
  (with-gensyms (thread)
    `(let ((,thread (make-thread (lambda() ,thread-body) :initial-bindings (list ,@(loop for (var value) in bindings collecting `(cons ,(list 'quote var) ,value))) :name ,name)))
       (unwind-protect
	    (progn
	      ,@body)
	 (join-thread ,thread)))))

(defun test-thread()
  (with-thread ("MY-THREAD" 
		((*standard-output* *standard-output*) (*thread-id* 1)) 
		(my-thread))
    (with-thread ("MY-NEXT-THREAD"
		  ((*standard-output* *standard-output*) (*thread-id* 2))
		  (my-thread))
    (format t "STARTED UP A THREADs~%"))))

(defparameter *my-buffer* (make-foreign-ring-buffer 1024 :element-type :int))

(defun make-iter(&optional (init 0) (delta 1))
  (dlambda 
    (:inc ()
	  (post-fix init
		    (incf init delta)))))

(defun test-buffer() 
  (With-thread ("WRITE-PROCESS" 
		((*standard-output* *standard-output*) (*my-buffer* *my-buffer*)) 
		(let ((buffer-length 1024)(iter (make-iter)))
		  (with-foreign-object (buffer :int buffer-length)
		    (for-each-range (idx buffer-length)
		      (setf (mem-aref buffer :int idx) (funcall iter :inc)))
		    (funcall *my-buffer* :write buffer buffer-length)
		    (funcall *my-buffer* :set-eof))))
    (let ((buffer-length 1024)) 
      (with-foreign-object (buffer :int buffer-length)
	(funcall *my-buffer* :read buffer buffer-length)
	(for-each-range (idx buffer-length)
	  (format t "~a:~a~%" idx (mem-aref buffer :int idx)))))))

(defun kill-write-processes()
  (loop for thread in (bordeaux-threads:all-threads) when (~ "WRITE-PROCESS" (thread-name thread))
       do (format t "~a~%" thread)))
       

(defun run())