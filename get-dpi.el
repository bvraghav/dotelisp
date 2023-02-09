(defun get-dpi ()
  (with-temp-buffer 
    (insert (shell-command-to-string "xrandr"))
    (keep-lines " connected" (point-min) (point-max))
    (let* ((words (split-string
                   (buffer-substring-no-properties (point-min)
                                                   (point-max))
                   " "))
           (sz (nth 3 words))
           (sx (string-to-number
                (first (split-string sz "x"))))
           (sy (string-to-number
                (first (split-string
                        (second (split-string sz "x"))
                        "+"))))
           (lx (/ (string-to-number (nth 2 (reverse words)))
                  25.4))
           (ly (/ (string-to-number (first (reverse words)))
                  25.4)))
      (* 0.5 (+ (/ sx lx) (/ sy ly))))))

(defun insert-dpi ()
  (interactive)
  (insert (number-to-string (get-dpi))))

(provide 'get-dpi)
