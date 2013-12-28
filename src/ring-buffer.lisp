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

(defun foreign-byte-size(size element-type)
  (* (foreign-type-size element-type) size))

(defmacro loop-down((size segment control) &body body)
  (with-once-only (size segment)
    (with-gensyms (remaining)
      `(do ((,remaining ,size)) ((<= ,remaining 0))
	 (let ((,control (min ,remaining ,segment)))
	   (decf ,remaining ,control)
	   ,@body)))))

(define-condition ring-buffer-eof(error)
  ((remaining :initarg :remaining :reader remaining)))

(define-condition user-eof(condition)())

(with-full-eval
  (defun array-ptr(array idx)
    (let ((size (- (length array) idx)))
      (make-array size :displaced-to array :displaced-index-offset idx))))

(defmacro with-array-ptrs(arrays &body body)
  `(macrolet
       ,(loop for array in arrays collecting
	     `(,(.sym '* array) (index)
		`(aref ,',array ,index)))
     (macrolet
	 ,(loop for array in arrays collecting
	       `(,(.sym '& array) (index)
		  `(array-ptr ,',array ,index)))	 
       ,@body)))

(defun make-cffi-context(type &key (count 1))
  (with-cffi-ptrs ((!buffer type)(src type))
    (let ((!buffer (foreign-alloc type :count count)))
      (dlambda
	(:write(dest-idx src src-idx count)
	       (memcpy (&!buffer dest-idx) (&src src-idx) (* (foreign-type-size type) count)))
	(:read(dest-idx src src-idx count)
	      (memcpy (&src src-idx) (&!buffer dest-idx) (* (foreign-type-size type) count)))
	(:&(ptr offset)
	     (with-cffi-ptrs ((ptr type))
	       (&ptr offset)))
	(:free()
	      (foreign-free !buffer))))))

(defun test-array()
  (with-array-ptrs (buffer)
    (let ((size 10))
      (let ((buffer (make-array size :element-type 'integer)))
	(for-each-range (n size)
	  (setf (aref buffer n) (+ 100 n)))
	(let ((buffer-2 (&buffer 3)))
	  (for-each-range (n 3)
	    (setf (aref buffer-2 n) (+ 200 n)))
	  (values
	   buffer
	   buffer-2))))))

(defun make-foreign-ring-buffer(size &key (element-type :uint8) (num-periods 2))
  (let ((!type-context (make-cffi-context element-type :count size))
	(!period (/ size num-periods))
	(!read-ptr 0)
	(!write-ptr 0)
	(!size size)
	(!full-p)
	(!error)
	(!eof-p)
	(!lock (make-lock))
	(!full-state (make-condition-variable))
	(!empty-state (make-condition-variable))) 
    (flet ((get-fill-count()
	     (if !full-p
		 !size
		 (mod (- !write-ptr !read-ptr) !size))))
      (flet ((get-room-count()
	       (- !size (get-fill-count)))
	     (set-eof ()
	       (with-lock-held (!lock)
		 (setf !eof-p t)
		 (condition-notify !full-state) 
		 (condition-notify !empty-state)))
	     (set-error (error)
	       (with-lock-held (!lock)
		 (condition-notify !full-state)
		 (condition-notify !empty-state)
		 (setf !error error))))
	(labels ((buffer-write(buffer size)
		   (unless (> size 0)(return-from buffer-write 0))
		   (let ((size
			  (with-lock-held (!lock)
			    (loop for room = (get-room-count) until (> room 0) do
				 (when !error (error "Encounter following buffer error in read process:~a" !error))
				 (when !eof-p (error 'user-eof))
				 (condition-wait !full-state !lock)
			       finally (return (min size room))))))		  
		     (loop for (dest-idx src-idx size) in (memcpy-params !size !write-ptr size)
			do
			  (funcall !type-context :write dest-idx buffer src-idx size)) 	     
		     (with-lock-held (!lock)
		       (setf !write-ptr (mod (+ !write-ptr size) !size))
		       (when (= !write-ptr !read-ptr)
			 (setf !full-p t))
		       (condition-notify !empty-state))
		     size))
		 (buffer-read(buffer size)
		   (unless (> size 0)(return-from buffer-read 0))
		   (let ((size
			  (with-lock-held (!lock)       
			    (loop for fill-count = (get-fill-count) until (> fill-count 0) do
				 (when !error (error "Encountered buffer error in read process"))
				 (when !eof-p
				   (error 'ring-buffer-eof :remaining fill-count))
				 (condition-wait !empty-state !lock)
			       finally (return (min size fill-count))))))
		     
		     (loop for (src-idx dest-idx size) in (memcpy-params !size !read-ptr size)
			do
			  (funcall !type-context :read src-idx buffer dest-idx size))

		     (with-lock-held (!lock)
		       (setf !read-ptr (mod (+ !read-ptr size) !size))
		       (setf !full-p nil)
		       (condition-notify !full-state))
		     size)))
	  
	  (dlambda 
	    
	    (:write-period(closure)
			  (let ((closure-params
				 (with-lock-held (!lock)
				   (loop for room = (get-room-count) until (>= room !period) do
					(when !error (error "Encounter following buffer error in read process:~a" !error))
					(when !eof-p (error 'user-eof))
					(condition-wait !full-state !lock))
				   (memcpy-params !size !write-ptr !period))))
			    
			    (let  ((size
				    (loop for (idx src-idx size) in closure-params sum
					 (funcall closure !type-context size))))

			      (with-lock-held (!lock)
				(setf !write-ptr (mod (+ !write-ptr size) !size))
				(when (= !write-ptr !read-ptr)
				  (setf !full-p t))
				(condition-notify !empty-state))
			      size)))

	    (:write (buffer size)
		    (block process
		      (unless (> size 0)(return-from process))
		      (let ((idx 0))
			(loop-down (size !size request-size)
			   (loop with total-bytes = 0 until (>= total-bytes request-size) do
				(let ((ret (buffer-write (funcall !type-context :& buffer idx) (- request-size total-bytes))))
				  (incf idx ret)
				  (incf total-bytes ret))))
			idx)))
	    
	    (:read (buffer size)
		   (block process
		     (unless (> size 0) (return-from process 0))
		     (handler-bind ((ring-buffer-eof 
				     (lambda(c)
				       (invoke-restart 'flush (remaining c)))))
		       (let ((idx 0))
			 (block reader
			     (loop-down (size !size request-size)
				(loop with total-bytes = 0 with eof = nil until (>= total-bytes request-size) do
				     (let ((ret
					    (restart-case
						(buffer-read (funcall !type-context :& buffer idx) (- request-size total-bytes))
					      (flush(remaining)
						(setf eof t)
						(buffer-read (funcall !type-context :& buffer idx) remaining)))))
				       (incf idx ret)
				       (incf total-bytes ret)
				       (when eof (return-from reader))))))
			 idx))))

	    (:set-eof () (set-eof))
	    (:set-error (error) (set-error error))
	    (:destroy ()
		      (set-eof)
		      (release-lock !lock)
		      (funcall !type-context :free))))))))

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
     (unwind-protect
	  (progn
	    ,@body)
       (funcall ,buffer :destroy))))

(defun write-buffer(ring-buffer c-buffer iter num-samples)
  (for-each-range (idx num-samples)
    (setf (mem-aref c-buffer :int idx) (funcall iter :inc)))
  (funcall ring-buffer :write c-buffer num-samples))

(defun test-buffer(&key (freq 1) (message-length 4096) (buffer-length 1024) (delay-reader-p) (delay-writer-p) (randomnize-p))
  (with-foreign-ring-buffer (my-buffer buffer-length :element-type :int)
    (with-thread ("WRITE-PROCESS" 
		  () 
		  (let ((buffer-length message-length)(iter (make-iter)))
		    (with-foreign-object (buffer :int buffer-length)
		      (flet ((write-buffer(num-samples)
			       (write-buffer my-buffer buffer iter num-samples)))
			(write-buffer 3)
			(loop for n from 1 to freq do
			     (when delay-writer-p (sleep 3))
			     (write-buffer (if randomnize-p (floor (* (random 1.0) buffer-length)) buffer-length))) 
			;(write-buffer iter (ash buffer-length -1))
			(write-buffer 3)
			(funcall my-buffer :set-eof)))))
      (let ((buffer-length message-length)) 
	(with-foreign-object (buffer :int buffer-length)
	  (loop 
	     (when delay-reader-p (sleep 3))
	     (let ((num-samples (funcall my-buffer :read buffer buffer-length)))
	       (for-each-range (idx num-samples)
		 (format t "~a:~a~%" idx (mem-aref buffer :int idx)))
	       (when (< num-samples message-length)
		 (format t "NUM-SAMPLES:~a MESSAGE-LENGTH:~a~%" num-samples message-length)
		 (return)))))))))

(defun kill-write-processes()
  (loop for thread in (bordeaux-threads:all-threads) when (~ "WRITE-PROCESS" (thread-name thread))
       do 
       (format t "~a~%" thread)
       (destroy-thread thread)))
       
(defun run())

