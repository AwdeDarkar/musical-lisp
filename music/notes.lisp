;;;; Core notes system

(load "../pitch/pitch.lisp")
(load "../rhythm/rhythm.lisp")
(load "../instrument/instrument.lisp")

(defclass note
  ((pitches
     :initarg :pitches
     :accessor pitches)
   (sections
     :initarg :sections
     :accessor sections)
   (instrument
     :initarg :instrument
     :accessor instrument)
   )
)
