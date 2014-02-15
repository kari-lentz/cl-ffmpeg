(in-package :my-sdl)

(defmacro defeasycondition(name (&rest parent-types) (&rest slot-names) &body report-body)
  (with-gensyms (c stream)
    `(define-condition ,name (,@parent-types)
       ,(qmap (slot-name) `(,slot-name :initarg ,(to-keyword slot-name) :reader ,slot-name) slot-names)
       ,@(when report-body 
	     `((:report (lambda(,c ,stream)
			  (with-slots (,@slot-names) ,c 
			    (format ,stream ,@report-body)))))))))

(defeasycondition sdl-fault (error) (msg)
  "~a" msg)

(defeasycondition sdl-null-fault (sdl-fault)())

(defmacro cffi-null-assert(lisp-name function &body args)
  (with-gensyms (ret)
    `(let ((,ret (,function ,@args)))
       (when (null-pointer-p ,ret)
	 (error 'sdl-null-fault :msg (% "\"~a\" returned null" (list ,(symbol-name lisp-name) ,@args)))
       ,ret))))

; constant-specs simple (constant value &optioanl documentation)
;
(defmacro defconstants(constant-specs)
  `(progn
     ,@(map-rows 
       (name value &optional documentation)
       `(defconstant ,name ,value ,documentation)
       constant-specs)))

;; (defcstruct SDL-Keyboard-Event
;;   (type :unsigned-char)
;;   (which :unsigned-char)
;;   (state :unsigned-char)
;;   (keysym SDL-key-sym))

(defcstruct SDL-Rect
  (x :short)
  (y :short)
  (w :unsigned-short)
  (h :unsigned-short))

(defcstruct SDL-Surface
  (flags :unsigned-int)
  (format :pointer)
  (w :int)
  (h :int)
  (pitch :unsigned-short)
  (pixels :pointer)
  (offset :int)
  (hwdata :pointer)
  (clip-rect (:struct SDL-Rect))
  (unused1 :unsigned-int)
  (locked :unsigned-int)
  (map :pointer)
  (format-version :unsigned-int)
  (refcount :int))

(defcstruct SDL-Overlay
  (format :unsigned-int)
  (w :int)
  (h :int)
  (planes :int)
  (pitches :pointer)
  (pixels :pointer)
  (hwfuncs :pointer)
  (hwdata :pointer)
  (hw-overlay :unsigned-int)
  (UnusedBits :unsigned-int))

(defcfun ("SDL_SetVideoMode" sdl-set-video-mode) :pointer
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(defcfun ("SDL_CreateYUVOverlay" SDL-Create-YUV-Overlay) :pointer
  (width :int)
  (height :int)
  (format :unsigned-int)
  (display :pointer))

(defcfun ("SDL_LockYUVOverlay" SDL-Lock-YUV-Overlay) :int
  (overlay :pointer))

(defcfun ("SDL_UnlockYUVOverlay" SDL-Unlock-YUV-Overlay) :void
  (overlay :pointer))

(defcfun ("SDL_DisplayYUVOverlay" SDL-Display-YUV-Overlay) :int
  (overlay :pointer)
  (dstrect :pointer))

(defcfun ("SDL_FreeYUVOverlay" SDL-Free-YUV-Overlay) :void
  (overlay :pointer))

(defcfun ("SDL_FreeSurface" sdl-free-surface) :void
  (surface (:pointer (:struct sdl-surface))))

(defconstants 
    ((SDL-FULLSCREEN #x80000000)
     (SDL-RESIZABLE #x00000010)
     (SDL-YV12-OVERLAY #x32315659)))

(defcenum Event-Type
  (:NO-EVENT 0)
  :ACTIVE-EVENT
  :KEY-DOWN-EVENT
  :KEY-UP-EVENT
  :MOUSE-MOTION-EVENT
  :MOUSE-BUTTON-DOWN-EVENT
  :MOUSE-BUTTON-UP-EVENT
  :JOY-AXIS-MOTION-EVENT
  :JOY-BALL-MOTION-EVENT
  :JOY-HAT-MOTION-EVENT
  :JOY-BUTTON-DOWN-EVENT
  :JOY-BUTTON-UP-EVENT
  :QUIT-EVENT
  :SYS-WM-EVENT
  :EVENT-RESERVEDA
  :EVENT-RESERVEDB
  :VIDEO-RESIZE-EVENT
  :VIDEO-EXPOSE-EVENT
  :EVENT-RESERVED2
  :EVENT-RESERVED3
  :EVENT-RESERVED4
  :EVENT-RESERVED5
  :EVENT-RESERVED6
  :EVENT-RESERVED7
  (:USER-EVENT 24)
  (:NUM-EVENTS 32))

(defcenum Sdl-Event-Type
  (:SDL-NO-EVENT 0)
  :SDL-ACTIVE-EVENT
  :SDL-KEY-DOWN-EVENT
  :SDL-KEY-UP-EVENT
  :SDL-MOUSE-MOTION-EVENT
  :SDL-MOUSE-BUTTON-DOWN-EVENT
  :SDL-MOUSE-BUTTON-UP-EVENT
  :SDL-JOY-AXIS-MOTION-EVENT
  :SDL-JOY-BALL-MOTION-EVENT
  :SDL-JOY-HAT-MOTION-EVENT
  :SDL-JOY-BUTTON-DOWN-EVENT
  :SDL-JOY-BUTTON-UP-EVENT
  :SDL-QUIT-EVENT
  :SDL-SYS-WM-EVENT
  :SDL-EVENT-RESERVEDA
  :SDL-EVENT-RESERVEDB
  :SDL-VIDEO-RESIZE-EVENT
  :SDL-VIDEO-EXPOSE-EVENT
  :SDL-EVENT-RESERVED2
  :SDL-EVENT-RESERVED3
  :SDL-EVENT-RESERVED4
  :SDL-EVENT-RESERVED5
  :SDL-EVENT-RESERVED6
  :SDL-EVENT-RESERVED7
  (:SDL-USER-EVENT 24)
  (:SDL-NUM-EVENTS 32))

(defconstant SDL-ALL-EVENTS #xFFFFFFFF)

(defcstruct SDL-Active-Event
  (type :unsigned-char)
  (gain :unsigned-char)
  (state :unsigned-char))

(defcstruct SDL-Keyboard-Event
  (type :unsigned-char)
  (which :unsigned-char)
  (state :unsigned-char)
  (keysym (:struct SDL-key-sym)))

(defcstruct SDL-Mouse-Motion-Event
  (type :unsigned-char)
  (which :unsigned-char)
  (state :unsigned-char)
  (x :unsigned-short)
  (y :unsigned-short)
  (xrel :short)
  (yrel :short))

(defcstruct SDL-Mouse-Button-Event
  (type :unsigned-char)
  (which :unsigned-char)
  (button :unsigned-char)
  (state :unsigned-char)
  (x :unsigned-short)
  (y :unsigned-short))

(defcfun ("SDL_PollEvent" SDL-Poll-Event) :int
  (event :pointer))

(defcfun ("SDL_WaitEvent" SDL-Wait-Event) :int
  (event :pointer))

;(defun make-window(&key (width 320) (height240) (flags 0))
;  (let ((surface (SDL-set-video-mode width height 0 flags)
;	  (when (null-pointer-p surface)

(defun test-rows()
  (macroexpand `(define-constants ((+portia+ #x20)(+ellen+ #x21)))))