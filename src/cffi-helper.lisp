(in-package :cffi-helper)

;; (define-cffi-return-types
;;     ((:ffmpeg-int :int ffmpeg-assert)))

(with-full-eval
  (defun default-c-name(lisp-name)
    (string-downcase (ppcre:regex-replace-all "\\-" (symbol-name lisp-name) "_")))
  (defun make-function(name)
    (intern (symbol-name (gensym (symbol-name name))))))

(with-full-eval
  (defstruct (cffi-return (:constructor cffi-return (type c-type assert-macro))) type c-type assert-macro)

  (defparameter *cffi-asserts* (make-hash-table)))

(defmacro !define-cffi-return-types(return-types)
  `(progn
     ,@(loop for (type c-type assert-macro) in return-types collecting 
	    `(setf (gethash ,type *cffi-asserts*) (cffi-return ,type ,c-type (quote ,assert-macro))))))

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
		  			     
	 
