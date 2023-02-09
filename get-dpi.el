(defun get-dpi ()
  (with-temp-buffer
    
    (insert (shell-command-to-string "xrandr"))
    (keep-lines " connected" (point-min) (point-max))
    (let* ((line (buffer-substring-no-properties (point-min)
                                                 (point-max)))
           (words (split-string line " "))
           (sz (nth 3 words))
           (sx (first (split-string sz "x")))
           (sy (second (split-string sz "x")))
           (sy (first (split-string sy "+")))
           (lx (caddr (reverse words)))
           (ly (first (reverse words))))
      (pcase-let ((`(,sx ,sy ,lx ,ly) (mapcar #'string-to-number 
                                              `(,sx ,sy ,lx ,ly))))
        (* 0.5 25.4 (+ (/ sx lx 1.0) (/ sy ly 1.0))))
      )
    ))

(defun insert-dpi ()
  (interactive)
  (insert (number-to-string (get-dpi))))

(provide 'get-dpi)
