;; (global-set-key (kbd "M-#") #'fill-line)

(defcustom fill-line-fill-char ?-
  "Default fill character to fill line")

(defun fill-line (arg)
  (interactive "P")
  (let* ((c (if arg (read-char "Fill Char: ")
              fill-line-fill-char))
         (n (max 0 (- (+ (pos-bol) fill-column) (point))))
         (s (string-pad "" n c)))
    (message "%s %s %s" c n s)
    (insert s)))



(provide 'fill-line)
