;;; chellakutty.el ---  -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.0.1
;; Package-Requires: (org)
;; Homepage: https://github.com/bvraghav/dotelisp
;; Keywords: miscellaneous

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; 

;;; Code:

(require 'org)

(defun chellakutty () 
  (interactive)
  (message "--- Chellakutty ---")
  (pcase-dolist (`(,dt ,w0) '(("2025-03-09 IST" 2430)
                              ("2025-05-06 IST" 4390)))
    (let* ((d (abs(org-time-stamp-to-now dt)))
           (nwi (/ d 7))
           (nwr (% d 7))
           (nwf (/ d 7.0))
           (dw 170)
           (ew (+ w0 (* nwf dw))))
      (message "WRT D0: %s" dt)
      (message "%s weeks %s days since D0" nwi nwr)
      (message "On D0, w: %s g" w0)
      (message "Today Expected w: %s g" ew))
    (message "--- Chellakutty ---")))


(provide 'chellakutty)

;;; chellakutty.el ends here
