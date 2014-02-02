(in-package :cffi-helper)

(defmacro defcstruct*(name-and-options &body fields)
  (let ((type (get-first-atom name-and-options)))
    `(progn
       (defcstruct ,name-and-options ,@fields)
       ,@(loop for item in fields collecting 
	      (let ((slot-name (get-first-atom item)))
		`(defmacro ,(.sym (get-first-atom name-and-options) '- (get-first-atom item)) (ptr) 
		   `(foreign-slot-value ,ptr ,''(:struct ,type) ,'',slot-name)))))))

(with-full-eval
  (defun default-c-name(lisp-name)
    (string-downcase (ppcre:regex-replace-all "\\-" (symbol-name lisp-name) "_")))
  (defun make-function(name)
    (intern (symbol-name (gensym (symbol-name name))))))

(defmacro with-cffi-readers(slots type instance &body body)
  `(let (,@(loop for (slot-local slot-global) in (ensure-pairs slots) collecting `(,slot-local (,(.sym type '- slot-global) ,instance))))
     ,@body))

(with-full-eval
  (defstruct (cffi-return (:constructor cffi-return (type c-type assert-macro))) type c-type assert-macro)

  (defparameter *cffi-asserts* (make-hash-table)))

(defmacro define-cffi-return-types(return-types)
  (loop for (type c-type assert-macro) in return-types do
       (setf (gethash type *cffi-asserts*) (cffi-return type c-type assert-macro))))

(with-full-eval

  (defun wrap-quote(expr)
    (list 'quote expr))

  (defun get-c-type(type)
    (aif (gethash type *cffi-asserts*)
	 (cffi-return-c-type it)
	 type)))

(defmacro call-assert(ret-type lisp-name function &rest args)
  (aif (gethash ret-type *cffi-asserts*)
       `(,(cffi-return-assert-macro it) ,lisp-name ,function ,@args)
       `(,function ,@args)))

(defmacro defcfun*(name-and-options return-type &body args)

  (multiple-value-bind (c-name lisp-name new-lisp-name)
      (if (atom name-and-options)
	  (values (default-c-name name-and-options) name-and-options (make-function name-and-options))
	  (let ((c-name (car name-and-options))
		(lisp-name (car (cdr name-and-options))))
	    (let ((new-lisp-name (make-function lisp-name)))
	      (values c-name lisp-name new-lisp-name))))
    `(progn
       (defcfun (,c-name ,new-lisp-name) ,(get-c-type return-type)
	 ,@args)
       ,(let ((lambda-list (qmap (arg) (get-first-atom arg) args)))
	     `(defmacro ,lisp-name ,lambda-list
		`(call-assert ,',return-type ,',lisp-name ,',new-lisp-name ,,@lambda-list))))))
		  			     
(defctype size-t :int)

(defcfun memcpy :pointer
  (dest :pointer)
  (src :pointer)
  (n size-t))
	 
(defcfun memset :pointer
  (ptr :pointer)
  (value :int)
  (size :uint))

(defun zero-memory(cffi-pointer cffi-type)
  (memset cffi-pointer 0 (foreign-type-size cffi-type)))

(defmacro with-cffi-ptrs(specs &body body)
  `(macrolet(,@(loop for (key type) in specs collecting 
		    `(,(.sym '* key)(index)
		      `(mem-ref ,',key ,',type ,index))))
     (macrolet(,@(loop for (key type) in specs collecting 
		      `(,(.sym '& key)(index)
			 `(mem-aptr ,',key ,',type ,index))))
       ,@body)))

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
	       (memcpy (&!buffer dest-idx) (&src src-idx) (* (foreign-type-size type) count))
	       count)
	(:read(dest-idx src src-idx count)
	      (memcpy (&src src-idx) (&!buffer dest-idx) (* (foreign-type-size type) count))
	      count)
	(:&&(offset)
	       (&!buffer offset))
	(:&(ptr offset)
	     (with-cffi-ptrs ((ptr type))
	       (&ptr offset)))
	(:free()
	      (foreign-free !buffer))))))

(defmacro with-cffi-context((cffi-context type &key (count 1)) &body body)
    `(let ((,cffi-context (make-cffi-context ,type &key :count ,count)))
       (unwind-protect
	    (progn
	      ,@body)
	 (funcall ,cffi-context :free))))

(defun array-copy(dest-array src-array count)
  (for-each-range (idx count)
    (setf (aref dest-array idx) (aref src-array idx))))

(defun make-array-context(type &key (count 1) element-factory)
  (with-array-ptrs (!buffer src)
    (let ((!buffer (make-array count :element-type type)))
      (when element-factory
	(for-each-range (idx count)
	  (setf (aref !buffer idx) (funcall element-factory))))
      (dlambda
	(:write(dest-idx src src-idx count)
	       (array-copy (&!buffer dest-idx) (&src src-idx) count)
	       count)
	(:read(dest-idx src src-idx count)
	      (array-copy (&src src-idx) (&!buffer dest-idx) count)
	      count)
	(:&(ptr offset)
	     (with-array-ptrs (ptr)
	       (&ptr offset)))
	(:&&(offset)
	    (&!buffer offset))
	(:free())))))

(defmacro with-array-context((array-context type &key (count 1) element-factory) &body body)
    `(let ((,array-context (make-array-context ,type &key :count ,count :element-factory ,element-factory)))
       (unwind-protect
	    (progn
	      ,@body)
	 (funcall ,array-context :free))))
       
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

