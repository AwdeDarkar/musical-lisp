;;;; Instrument system

(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-wave-file-writer")

;;; === FUNCTION OPERATIONS === ;;;

(defun meta-function-operator (meta-func)
  (lambda (fs)
    (lambda (tm) (funcall meta-func (loop for f in fs collect (funcall f tm))))
  )
)

(defun f* (fs) (funcall (meta-function-operator *) fs))
(defun f+ (fs) (funcall (meta-function-operator +) fs))

(defun fscale (f x)
  (lambda (tm) (* x (funcall f tm)))
)

(defun fshift (f x)
  (lambda (tm) (funcall f (* x tm)))
)

(defun fphase (f x)
  (lambda (tm) (funcall f (+ tm x)))
)

(defun fwrap (f arg0)
  (lambda (arg1) (funcall f arg0 arg1))
)

;;; === AUDIO SEGMENTS === ;;;

(defclass segment ()
  ((func
     :initarg :func
     :accessor func)
   (duration
     :initarg :duration
     :accessor duration)
   )
)

(defun generate-sample (segment volume sample-rate)
  (loop for tm from 0 to (* (slot-value segment 'duration) sample-rate)
    collect (* (funcall (slot-value segment 'func) (/ (slot-value segment 'duration) sample-rate)) volume)
  )
)

(defun create-segment (pitch-gen pulse-gen pitch duration)
  (make-instance 'segment :func (f* (list (funcall pitch-gen pitch) (funcall pulse-gen duration))) :duration duration)
)

;;; === CORE PITCH FUNCTIONS === ;;;

(defun fpitch (f pitch)
  (fshift f (* pitch 2 pi))
)

(defun fourier-sum (wave-gen pitch-gen amp-gen cutoff)
  (setq i 0)
  (f+ (loop while (> (funcall amp-gen i) cutoff) do (incf i) collect 
    (fscale (funcall amp-gen i) (funcall wave-gen (funcall pitch-gen i))))
  )
)

;;; === CORE PULSE FUNCTIONS === ;;;

(defun square (duration)
  (lambda (tm) (if (< tm duration) 1 0))
)

(defun gaussian-distribution (mean variance)
  (lambda (tm) (*
                 (/ 1 (* variance (sqrt (* 2 pi))))
                 (exp (* -0.5 (expt (/ (- tm mean) variance) 2)))
               )
  )
)

(defun gaussian (duration)
  (gaussian-distribution (/ duration 2) (/ duration 4))
)

;;; === Audio Play === ;;;
(defconstant +output-file+ "/home/darkar/bench/learning/lisp/musical-lisp/generated/output.wav")
(defconstant +sample-rate+ 44100)
(defconstant +base-volume+ 0.04)

(defun play-sample (sample sample-rate file)
  (let ((wave-writer (cl-wave-file-writer:make-writer
                       :filename file
                       :channel-count 1
                       :sample-width :16Bit
                       :sample-rate sample-rate)))
      ;; Open file
      (funcall (getf wave-writer :open-file))
      ;; Write samples
      (apply (getf wave-writer :write-sample) sample)
      ;; Close file
      (funcall (getf wave-writer :close-file))
  )
)

(defun play-segment (segment volume sample-rate file)
  (play-sample (generate-sample segment volume sample-rate) sample-rate file)
)

(defun play (segment)
  (play-segment segment +base-volume+ +sample-rate+ +output-file+)
)

(defconstant output (create-segment (fwrap 'fpitch 'sin) 'gaussian 440 1))

;(play output)
