;;; bvr-utils.el --- Random utils (BVR). -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: (cl-lib)
;; Homepage: https://github.com/bvraghav/dotelisp.git
;; Keywords: utils,elisp


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

;; Random utils collected by BVR from all over the place.

;;; Code:
(require 'cl-lib)

(defgroup bvr-utils nil "Propertie of BVR Utils.")

(defun group-p (symbol)
  "Whether SYMBOL exists and is a group."
  (and (symbolp symbol)
       (or (and (get symbol 'custom-loads)
                (not (get symbol 'custom-autoload)))
           (get symbol 'custom-group))))

(defun read-integer (&rest args)
  "Wrapper around built-in `read-number' function to read integer.

The ARGS are forwarded as is to `read-number'.

Inspired by: Emanuel Berg
https://lists.gnu.org/archive/html//help-gnu-emacs/2021-11/msg00186.html"
  (cl-loop with sentinel = "9£9dkt,d"
	   with args = (or args '("Integer: "))
	   for n = sentinel
	   then (apply #'read-number args)
	   until (or (integerp n)
		     (unless (equal sentinel n)
		       (message "Please enter an integer.")
		       (sit-for 1)
		       nil))
	   finally (cl-return n)))

(defun camel-to-title-case (txt)
  "Convert TXT from `camelCase' to `Title Case'."
  (let ((case-fold-search nil))
    (capitalize
     (replace-regexp-in-string
      "\\([A-Z]\\)"
      " \\1"
      txt
      t))))


(provide 'bvr-utils)

;;; bvr-utils.el ends here
