(in-package :cl-media-time)

(defstruct (media-time (:constructor media-time (&optional (dts (make-dts-now)) (ms 0))))
  dts ms)
  
(defun media-time+(media-time ms)
  (let ((total-ms (+ (media-time-ms media-time) ms)))
    (multiple-value-bind (total-secs frac) (floor (/ total-ms 1000))
      (media-time (dts+ (media-time-dts media-time) total-secs :seconds) (* 1000 frac)))))

(defmacro define-comparator(comp comp-hi comp-lo media-time-1 media-time-2)
  (with-gensyms (ut-1 ut-2)
    `(defun ,(.sym 'media-time comp) (,media-time-1 ,media-time-2)
       (with-slots ((dts-1 dts) (ms-1 ms)) ,media-time-1
	 (with-slots ((dts-2 dts) (ms-2 ms)) ,media-time-2
	   (let ((,ut-1 (dts-ut dts-1))(,ut-2 (dts-ut dts-2)))
	     (or (,comp-hi ,ut-1 ,ut-2) 
		 (if (= ,ut-1 ,ut-2)
		     (,comp-lo ms-1 ms-2)
		     nil))))))))

(defmacro define-comparators(&rest specs)
  `(progn
     ,@(loop for (comp comp-hi comp-lo) in specs collecting
	    `(define-comparator ,comp ,comp-hi ,comp-lo media-time-1 media-time-2))))

(define-comparators (> > >) (>= > >=) (< < <) (<= < <=))

(defparameter *operators* ({} ('> #'media-time>)('>= #'media-time>=)('< #'media-time<)('<= #'media-time<=)))

(defun test(operator secs-1 ms-1 secs-2 ms-2)
  (let ((mm1 (media-time (make-dts 2014 1 19 16 20 secs-1) ms-1))
	(mm2 (media-time (make-dts 2014 1 19 16 20 secs-2) ms-2)))    
    (funcall ([] *operators* operator) mm1 mm2)))