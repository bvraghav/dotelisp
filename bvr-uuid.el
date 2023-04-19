;;; UUID --- Insert UUID as number at point
;;;
;;; Commentary:
;;; 

;;; Code:
(require 'uuidgen)

(use-package bvr-utils)

(defgroup bvr-uuid nil
  "Properties of 'bvr-uuid package."
  :group 'bvr-utils)

(defcustom bvr/random-uuid-string-length 8
  "String length of uuid string to insert."
  :type '(integer)
  :local t
  :group 'bvr-uuid)

(defun bvr/random-seed-from-uuidgen nil
  "Insert random seed int at poing from uuidgen."
  (interactive)
  (let* ((snum (uuidgen-4))
         (snum (split-string snum "-"))
         (snum (seq-take snum 2))
         (snum (string-join snum))
         (snum (string-to-number snum 16)))
    (insert (number-to-string snum))))

(defun bvr/random-uuid-string (arg)
  "Insert random string from uuid.

Prompt for string length if prefix ARG is given."
  (interactive "P")
  (let* ((slen bvr/random-uuid-string-length)
         (slen (if (and slen (not arg)) slen
                 (read-integer "String Length: ")))
         (snum (uuidgen-4))
         (snum (split-string snum "-"))
         (snum (string-join snum))
         (snum (substring snum 0 slen)))
    (insert snum)))

(provide 'bvr-uuid)
;;; bvr-uuid.el ends here
