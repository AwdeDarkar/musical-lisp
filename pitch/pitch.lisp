;;;; Pitch system

(defgeneric get-note (note-collection ordinal)
  (:documentation "Get the ordinal note of the tuning system")
)

(defclass tuning ()
  ((base
     :initarg :base
     :accessor base)
  )
)

(defclass equal-tuning (tuning)
  ((tune-ratio
     :initarg :tune-ratio
     :accessor tune-ratio)
  )
)

(defconstant +12-tet+ (make-instance 'equal-tuning :base 440 :tune-ratio 1.059463094))

(defmethod get-note ((tn equal-tuning) (ordinal integer))
  (* (slot-value tn 'base) (expt (slot-value tn 'tune-ratio) ordinal))
)

(defclass chord ()
  ((super
     :initarg :super
     :accessor super)
   (ordinals ; List of ordinals in super-chord (or tuning)
     :initarg :ordinals
     :accessor ordinals)
  )
)

(defmethod get-note ((chrd chord) (ordinal integer))
  (get-note (slot-value chrd 'super) (nth ordinal (slot-value chrd 'ordinals)))
)

(defun sum (lst i end)
  (if (eq i end) 0
      (+ (first lst) (sum (rest lst) (+ i 1) end)))
)

(defun make-scale (tn increments)
  (make-instance 'chord :super tn
    :ordinals (loop for i from 0 to (length increments)
                collect (sum increments 0 i))
  )
)

(defconstant +major-scale+ (list 2 1 2 2 1 2 2))
(defconstant +a-major+ (make-scale +12-tet+ +major-scale+))
