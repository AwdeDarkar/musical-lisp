#!/usr/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-wave-file-writer")

(defconstant +output-file+ "/home/darkar/bench/learning/lisp/musical-lisp/generated/output.wav")
(defconstant +sample-rate+ 44100)

(defun play-function (func duration volume wave-writer) 
  (dotimes (i (* duration +sample-rate+))
      (funcall (getf wave-writer :write-sample) (* volume (funcall func (/ i +sample-rate+))))
      (funcall (getf wave-writer :write-sample) (* volume (funcall func (/ i +sample-rate+))))
  )
 )

(defun make-sine-tone (frequency offset volume)
  (lambda (tm)
    (* volume (sin (+ (* 2 PI frequency tm) offset)))
  )
 )

(defun play-sine-tone (duration frequency volume wave-writer)
  (play-function (make-sine-tone frequency 0 1) duration volume wave-writer)
  )

(defclass playable-tone ()
  ((frequency
     :initarg :frequency
     :accessor frequency)
   (duration
     :initarg :duration
     :accessor duration))
  )

(defmethod play-tone (playable volume wave-writer)
  (play-sine-tone (slot-value playable 'duration) (slot-value playable 'frequency) volume wave-writer)
  )

(defun main()
  ;; Instantiate writer
  (let ((wave-writer (cl-wave-file-writer:make-writer
                       :filename +output-file+
                       :channel-count 2
                       :sample-width :16Bit
                       :sample-rate +sample-rate+)))
      ;; Open file
      (funcall (getf wave-writer :open-file))
      ;; Write samples
      (play-tone
        (make-instance 'playable-tone :frequency 440 :duration 2)
        0.04
        wave-writer
        )
      ;; Close file
      (funcall (getf wave-writer :close-file))
   )
 )

(main)
