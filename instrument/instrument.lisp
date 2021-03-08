;;;; Instrument system

(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-wave-file-writer")

;;; === FUNCTION OPERATIONS === ;;;
;; (these are pretty universal and should maybe go in a separate lib)

(defun fmulti (fs x)
  (loop for f in fs collect (funcall f x))
)

;; Takes a function which operates on numbers (like * or +) and returns a function that
;; wraps a list of functions that also operate on numbers.
;; So: MFA: (F: R -> R) -> (F: (list F: R -> R) -> (F: R -> R))
(defun meta-function-operator (meta-func)
  (lambda (fs)
    (lambda (tm) (apply meta-func (fmulti fs tm)))
  )
)

(defun f* (fs) (funcall (meta-function-operator '*) fs))
(defun f+ (fs) (funcall (meta-function-operator '+) fs))

(defun fscale (f x)
  (lambda (tm) (* x (funcall f tm)))
)

(defun fshift (f x)
  (lambda (tm) (funcall f (* x tm)))
)

(defun fo (f g)
  (lambda (tm) (funcall f (funcall g tm)))
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
    collect (* (funcall (slot-value segment 'func) (/ tm sample-rate)) volume)
  )
)

(defun create-segment (pitch-gen pulse-gen pitches duration)
  (make-instance 'segment :func
    (f* (list
      (f+
        (loop for pitch in pitches collect
          (funcall pitch-gen pitch)
        )
      )
      (funcall pulse-gen duration)
    )) :duration duration
  )
)

;;; === CORE PITCH FUNCTIONS === ;;;

(defun fpitch (f pitch)
  (fshift f (* pitch 2 pi))
)

(defun square (pitch)
  (lambda (tm) (if (> (sin (* tm pitch 2 pi)) 0) 1 -1))
)

(defun sine (pitch)
  (fpitch 'sin pitch)
)

(defun warp-sine (warp)
  (lambda (pitch) (fpitch (fo 'sin warp) pitch))
)

(defun fourier-sum (wave-gen pitch-gen amp-gen cutoff)
  (setq i 0)
  (f+ (loop while (> (funcall amp-gen i) cutoff) do (incf i) collect 
    (fscale (funcall wave-gen (funcall pitch-gen i)) (funcall amp-gen i)))
  )
)

(defun overtone-sequence (base-pitch)
  (lambda (n) (* base-pitch n))
)

(defun detuned-overtone (base-pitch tune-shift)
  (lambda (n) (+ (* base-pitch n) (funcall tune-shift n)))
)

(defun inst-harmonic (pitch) (fourier-sum 'sine (overtone-sequence pitch) (lambda (n) (/ 1 (+ n 1))) (/ 1 20)))
(defun inst-harmonic-tunedown (pitch) (fourier-sum 'sine (detuned-overtone pitch (lambda (n) (* n n))) (lambda (n) (/ 1 (+ n 1))) (/ 1 20)))

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

(defun gaussian-pulse (sharpness)
  (lambda (duration)
    (gaussian-distribution (/ duration 2) (/ duration sharpness))
  )
)

;;; === Audio Play === ;;;
(defconstant +output-file+ "/home/darkar/bench/learning/lisp/musical-lisp/generated/output.wav")
(defconstant +sample-rate+ 44100)
(defconstant +base-volume+ (expt 2 -6.5))

(defun play-sample (sample sample-rate file)
  (let ((wave-writer (cl-wave-file-writer:make-writer
                       :filename file
                       :channel-count 1
                       :sample-width :16Bit
                       :sample-rate sample-rate)))
      ;; Open file
      (funcall (getf wave-writer :open-file))
      ;; Write samples
      (loop for val in sample do (funcall (getf wave-writer :write-sample) val))
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

(defconstant output (create-segment 'inst-harmonic (gaussian-pulse 2.2) '(330 440 660 880) 0.5))

(play output)
