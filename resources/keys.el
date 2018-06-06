;;; %F --- %[Brief description of package: %]

(defun next-hidden-page ()
  (interactive)
  (widen)
  (forward-page)
  (narrow-to-page))

(defun previous-hidden-page ()
  (interactive)
  (widen)
  ;; Going backwards is slightly tricker;
  (backward-page 2)			; must skip the one we're on
  (narrow-to-page)
  (goto-char (point-min))) ; And reset ourselves to the top.

(define-key clojure-mode-map (kbd "M-n") 'next-hidden-page)
(define-key clojure-mode-map (kbd "M-p") 'previous-hidden-page)
