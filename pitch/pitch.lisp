;;;; Pitch system

(defgeneric get-pitch (note-collection ordinal)
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

(defmethod get-pitch ((tn equal-tuning) (ordinal integer))
  (* (slot-value tn 'base) (expt (slot-value tn 'tune-ratio) ordinal))
)

(defun from-pitch (tn pitch)
  (round (log (/ pitch (slot-value tn 'base)) (slot-value tn 'tune-ratio)))
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

(defmethod get-pitch ((chrd chord) (ordinal integer))
  (get-pitch (slot-value chrd 'super) (nth ordinal (slot-value chrd 'ordinals)))
)

(defmethod get-pitches (chrd)
  (loop for i from 0 to (- (length (slot-value chrd 'ordinals)) 1) collect (get-pitch chrd i))
)

(defun sum (lst i end)
  (if (eq i end) 0
      (+ (first lst) (sum (rest lst) (+ i 1) end)))
)

(defun rotate (lst i)
  (append (subseq lst i (length lst)) (subseq lst 0 i))
)

(defun modnth (lst i)
  (nth (mod i (length lst)) lst)
)

(defun substitute-nth (lst n new)
  (if (< (length lst) 3) lst
      (append (subseq lst 0 n) (list new) (subseq lst (+ n 1) (length lst))))
)

(defun add-nth (lst n change)
  (substitute-nth lst n (+ (nth n lst) change))
)

(defun in-range (x rng)
  (and (>= x (first rng)) (<= x (second rng)))
)

(defun float-eq (x y)
  (< (abs (- x y)) 0.05)
)

(defun make-scale (tn increments base-note)
  (make-instance 'chord :super tn
    :ordinals (loop for i from 0 to (length increments)
                collect (+ (sum increments 0 i) base-note))
  )
)

(defconstant +major-scale+ (list 2 2 1 2 2 2 1))
(defconstant +harmonic-minor-scale+ (list 2 1 2 2 1 3 1))
(defconstant +a-major+ (make-scale +12-tet+ +major-scale+ 0))

;; 0 - Ionian (major), 1 - Dorian, 2 - Phrygian, 3 - Lydian,
;; 4 - Mixolydian, 5 - Aeolian (Natural Minor), 6 - Locrian
(defun make-major-mode (degree base-note)
  (make-scale +12-tet+ (rotate +major-scale+ degree) base-note)
)

;; Builds a triad off of the scale/chord from the given degree, but keeps the
;; tuning as the super-chord to make modification easier
(defun make-triad (scale degree)
  (make-instance 'chord :super (slot-value scale 'super)
      :ordinals (loop for x in '(0 2 4) collect (modnth (slot-value scale 'ordinals) (+ x degree))))
)

;;; Labelling (pitch -> note value) assumes everything is 12-TET for now
(defconstant +note-names+ (list "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

(defun get-pitch-from-name (note-name octave)
  (get-pitch +12-tet+ (+ (- (position note-name +note-names+ :test (lambda (a b) (string= a b))) 9) (* 12 (- octave 4))))
)

(defun get-pitch-from-note (note-name)
  (if (find #\# note-name) (get-pitch-from-name (subseq note-name 0 2) (parse-integer (subseq note-name 2)))
       (get-pitch-from-name (subseq note-name 0 1) (parse-integer (subseq note-name 1))))
)

(defconstant +octave-ranges+ (loop for oct from 0 to 8 collect (list
                                                                 (- (get-pitch-from-name "C" oct) 0.01)
                                                                 (+ (get-pitch-from-name "B" oct) 0.01)))
)

(defun get-pitch-octave (pitch)
  (position pitch +octave-ranges+ :test (lambda (p rng) (in-range p rng)))
)

(defun get-pitch-note (pitch)
    (find pitch +note-names+ :test (lambda (p note) (float-eq p (get-pitch-from-name note (get-pitch-octave pitch)))))
)

(defun get-pitch-name (pitch)
  (list (get-pitch-note pitch) (get-pitch-octave pitch))
)

(defun label-pitch (pitch)
  (concatenate 'string (get-pitch-note pitch) (write-to-string (get-pitch-octave pitch)))
)

(defun label-chord (chrd)
  (loop for pitch in (get-pitches chrd) collect (label-pitch pitch))
)

;;; Intervals
(defconstant +interval-names+ (list "P1" "m2" "M2" "m3" "M3" "P4" "A4" "P5" "m6" "M6" "m7" "M7" "P8"))

(defun label-interval (semitones)
  (if (< semitones 12) (nth semitones +interval-names+)
      (concatenate 'string (label-interval (mod semitones 12)) "+" (write-to-string (floor semitones 12))))
)

(defun interval-semitones (name)
  (position name +interval-names+ :test (lambda (a b) (string= a b)))
)

(defun pitch-interval (p0 p1)
  (label-interval (- (from-pitch +12-tet+ (max p0 p1)) (from-pitch +12-tet+ (min p0 p1))))
)

(defun note-interval (n0 n1)
  (pitch-interval (get-pitch-from-note n0) (get-pitch-from-note n1))
)

;;; Chord Modifications
(defun trans-semitone (chord ordinal semitone)
  (make-instance 'chord :super (slot-value chord 'super)
                 :ordinals (add-nth (slot-value chord 'ordinals) ordinal semitone))
)

(defun trans-semitone-all (chord semitone)
  (make-instance 'chord :super (slot-value chord 'super)
                 :ordinals (loop for ord in (slot-value chord 'ordinals) collect (+ ord semitone)))
)

(defun trans-octave (chord ordinal octave)
  (trans-semitone chord ordinal (* 12 octave))
)

(defun trans-octave-all (chord octave)
  (trans-semitone-all chord (* 12 octave))
)

(defun chord-add-semitone (chord semitone)
  (make-instance 'chord :super (slot-value chord 'super)
                 :ordinals (append (slot-value chord 'ordinals) (list semitone)))
)

(defun chord-add-base-offset (chord offset)
  (chord-add-semitone chord (+ offset (first (slot-value chord 'ordinals))))
)

(defconstant output
  (label-chord (chord-add-base-offset (make-triad +a-major+ 0) 10))
)
