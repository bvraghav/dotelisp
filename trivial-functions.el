;; Taken from here: https://emacs.stackexchange.com/a/38159
(defun bvr-reverse-region (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

(provide 'trivial-functions)
