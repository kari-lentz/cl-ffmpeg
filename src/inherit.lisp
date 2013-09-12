(in-package :cl-ffmpeg-inherit)

(defmacro defstruct*(name-and-options &rest slot-descriptions)
    (let ((name (get-first-atom name-and-options))(slots (mapcar #'get-first-atom slot-descriptions)))
      (let ((constructor-form (car (remove-if-not (lambda(form) (and (not (atom form))(eq (car form) :constructor))) name-and-options))))
	(let ((constructor
	       (if constructor-form
		   (progn
		     (unless (>= (length constructor-form) 2) (error "defstruct-exportable ~a has malformed constrcutor form" name-and-options))
		     (second constructor-form))
		   (progn
		     (.sym 'make '- name)))))
	  (let ((accesors (qmap (slot) (.sym name '- slot) slots)))
	    `(progn
	       (format t (% "COPY THIS:~%**********~%~a~%***********~%" ,(join " " (mapcar (lambda(item) (% ":~a" item))  (make-set (append (list name constructor) slots accesors))))))  	   
	       (defstruct ,name-and-options ,@slot-descriptions)))))))

(defstruct* (ffmpeg-env (:constructor ffmpeg-env (ring-buffer in-device out-device)))
    (:export-p t)
    ring-buffer (output-buffer-size 16384) in-device out-device media-type)

(defstruct* 
    (audio-params 
     (:constructor audio-params (&key (media-type :avmedia-type-audio) (sample-rate 44100)(num-channels 2) ring-buffer output-buffer-size))
     (:include ffmpeg-env))
    sample-rate num-channels)


