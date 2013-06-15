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
	   `(mem-aref ,buffer :uint8 ,idx)))
  `(memcpy ,(foreign-buffer dest-buffer dest-idx) ,(foreign-buffer src-buffer src-idx) ,size)))

(defun foreign-byte-size(size element-type)
  (* (foreign-type-size element-type) size))

(defun make-foreign-ring-buffer(size &key (element-type :uint8))
  (let ((!buffer (foreign-alloc element-type))
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
	     (dlambda 

	       (:write (buffer size) 
		       (block process
			 (unless (> size 0)(return-from process))
			 (let ((size (foreign-byte-size size !element-type)))
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
			     (condition-notify !empty-state)))))

	       (:read (buffer size)
		      (block process
			(unless (> size 0) (return-from process))
			(let ((size (foreign-byte-size size !element-type)))
			  (with-lock-held (!lock) 
			    (loop while (< size (get-fill-count)) do
				 (when !error-p (error "Encounter buffer error in write process"))
				 (when !eof-p (error "Encounter eof in write process"))
				 (condition-wait !empty-state !lock)))

			  (loop for (src-idx dest-idx size) in (memcpy-params !size !read-ptr size)
			     do
			       (memcpy-call buffer dest-idx !buffer src-idx size))
			  
			  (with-lock-held (!lock)
			    (setf !read-ptr (mod (+ !read-ptr size) !size))
			    (setf !full-p nil)
			    (condition-notify !full-state)))))
	       (:set-eof ()
			 (with-lock-held (!lock)
			   (setf !eof-p t)))
	       (:set-error ()
			   (with-lock-held (!lock)
			     (setf !error-p t)))))))

    (release-lock !lock)))

(defun test()
  (let ((buffer (make-ring-buffer 4)))
    (funcall buffer :write 10)
    (funcall buffer :write 11)
    (funcall buffer :read)
    (funcall buffer :read)
    (funcall buffer :write 12)
    (funcall buffer :write 13)
    (funcall buffer :write 14)
    (funcall buffer :read2)))

(defun test-array-1()
  (let ((numbers (make-array 10 :element-type 'integer)))
    (loop for n in (range 10) do
	 (setf (aref numbers n) (+ (* n 10) n)))
    (setf (subseq numbers 5 8) #(100 200 300))
    numbers))

(defun run())