#!/usr/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-wave-file-writer")

(defconstant +output-file+ "/home/darkar/bench/learning/lisp/musical-lisp/generated/output.wav")
(defconstant +sample-rate+ 44100)

(defun make-phase-generator (sample-rate)
    (let ((phi 0.0))
        (lambda (frequency)
            (setf phi (rem (+ phi (/ (* 2 PI frequency) sample-rate)) (* 2 PI)))
            phi
        )
    )
)

(defun sine-tone (duration frequency volume wave-writer phase-generator)
  (dotimes (i (* duration +sample-rate+))
    (let ((phase (funcall phase-generator frequency)))
      ;; Channel 1
      (funcall (getf wave-writer :write-sample) (* volume (sin phase)))
      ;; Channel 2
      (funcall (getf wave-writer :write-sample)  (* volume (cos phase)))
      )
    )
  )

(defun example()
  (let* ((sample-rate +sample-rate+)
         (phase-generator (make-phase-generator sample-rate)))
    ;; Instantiate writer
    (let ((wave-writer (cl-wave-file-writer:make-writer
                        :filename +output-file+
                        :channel-count 2
                        :sample-width :16Bit
                        :sample-rate sample-rate)))
      ;; Open file
      (funcall (getf wave-writer :open-file))
      ;; Write samples
      (sine-tone 2 640 0.04 wave-writer phase-generator)
      ;; Close file
      (funcall (getf wave-writer :close-file))))
  "DONE")

(example)
