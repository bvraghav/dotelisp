;; (global-set-key (kbd "M-#") #'fill-line)

(defcustom fill-line-fill-char ?-
  "Default fill character to fill line")

(defun fill-line (arg)
  (interactive "P")
  (let* ((c (and arg (read-char "Fill Char: "))))
    (insert (fill-line-get-filler c))))

(defun fill-line-get-filler (&optional fill-char prefix)
  (let* ((n (- (+ (pos-bol) fill-column) (point)))
         (c (or fill-char fill-line-fill-char))
         (p (or prefix "")))
    (string-pad p n c)))


(provide 'fill-line)
