(cl-defstruct (card (:type list))
  rank
  suit)

(setq 3-hearts (make-card :rank 3 :suit 'hearts))

(card-rank 3-hearts)

(defun print-something ()
  (message "This is some text."))

