
(defun get-dpi ()
  (or (get-dpi-connected) 0))

(defun get-dpi-connected ()
  (with-temp-buffer
    
    (insert (shell-command-to-string "xrandr"))
    (keep-lines " connected" (point-min) (point-max))
    (when (< 0 (first (wc-count)))
      (gd--compute-dpi))))

(defun gd--compute-dpi ()
  (let* ((line (buffer-substring-no-properties (point-min)
                                               (point-max)))
         (words (split-string line " "))
         (n (max (or (cl-position "connected" words :test #'equal) 0)
                 (or (cl-position "primary" words :test #'equal) 0)))
         (sz (nth (1+ n) words))
         (sx (first (split-string sz "[x+]" )))
         (sy (second (split-string sz "[x+]" )))
         (lx (caddr (reverse words)))
         (ly (first (reverse words))))
    (pcase-let ((`(,sx ,sy ,lx ,ly) (mapcar #'string-to-number 
                                            `(,sx ,sy ,lx ,ly))))
      (* 0.5 25.4 (+ (/ sx lx 1.0) (/ sy ly 1.0))))
    ))

(defun insert-dpi ()
  (interactive)
  (insert (number-to-string (get-dpi))))

(provide 'get-dpi)
