;;;; Rhythm system

(defclass division ()
  ((order ; Number of elements in the division
     :initarg :order
     :accessor order)
   (distribution ; List of floats of length 'order that sums to 1
     :initarg :distribution
     :accessor distribution)
   (weights ; List of floats of length 'order in [0, 1]
     :initarg :weights
     :accessor weights))
)

(defclass section ()
   ((super-section ; Super-section (if any)
     :initarg :super-section
     :accessor super-section)
   (super-position ; Position within the super-section
     :initarg :super-position
     :accessor super-position)
   (sub-divisions ; Links to the sub-divisions
     :initarg :subs
     :accessor subs)
   (division ; How this section is divided into sub-divisions
     :initarg :division
     :accessor division)
   (comp-beat ; < 0 if the section is a lesser time division than a beat, > 0 if it is greater, = 0 if it's a beat
     :initarg :comp-beat
     :accessor comp-beat))
)

(defun get-duration-at-bpm (sect bpm)
  (if (= (slot-value sect 'comp-beat) 0) (/ 60 bpm)
      (if (> (slot-value sect 'comp-beat) 0)
          (apply '+ (loop for sub in (slot-value sect 'sub-divisions) collect (get-duration-at-bpm sub bpm)))
          (* (nth (slot-value sect 'super-position) (slot-value (slot-value sect 'super-section) 'distribution))
             (get-duration-at-bpm (slot-value sect 'super-section) bpm))
      )
  )
)

(defun equal-division (n)
  (make-instance 'division :order n :distribution (loop for i from 0 to (- n 1) collect (/ 1 n)))
)

(defun make-beat (measure ordinal)
  (make-instance 'section :super-section measure :super-position ordinal :comp-beat 0)
)

(defun make-measure (num-beats)
  (setq measure (make-instance 'section :division (equal-division num-beats) :comp-beat 1))
  (setf (slot-value measure 'sub-divisions) (loop for i from 0 to (- num-beats 1) collect (make-beat measure i)))
  measure
)

(defconstant output
  (make-measure 4)
)
