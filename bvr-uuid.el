;;; UUID --- Insert UUID as number at point
;;;
;;; Commentary:
;;; 

;;; Code:
(require 'uuidgen)

(defun bvr/random-seed-from-uuidgen nil
  "Insert random seed int at poing from uuidgen."
  (interactive)
  (let* ((snum (uuidgen-4))
         (snum (split-string snum "-"))
         (snum (seq-take snum 2))
         (snum (string-join snum))
         (snum (string-to-number snum 16)))
    (insert (number-to-string snum))))

(provide 'bvr-uuid)
;;; bvr_uuid.el ends here
