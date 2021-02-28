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
     :initarg :super
     :accessor super)
   (super-position ; Position within the super-section
     :initarg :pos
     :accessor pos)
   (sub-divisions ; Links to the sub-divisions
     :initarg :subs
     :accessor subs)
   (division ; How this section is divided into sub-divisions
     :initarg :division
     :accessor division)
   (comp-beat ; < 0 if the section is a lesser time division than a beat, > 0 if it is greater, = 0 if it's a beat
     :initarg :is-measure
     :accessor is-measure))
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
  (make-instance 'division :order n :distribution (loop for i from 0 to n (collect (/ 1 n))))
)
