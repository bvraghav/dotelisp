;;; insert-gitignore.el --- Insert at point, a gitignore template. -*- lexical-binding: t -*-

;; Author: Raghav B. Venkataramaiyer
;; Maintainer: Raghav B. Venkataramaiyer
;; Version: 0.0.1
;; Package-Requires: (f)
;; Homepage: https://github.com/bvraghav/dotelisp
;; Keywords: gitignore,template


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

;; Insert at point, a gitignore template.

;;; Code:

(require 'f)

(defcustom bvr/ig/gitignores-dir "~/code/gitignore"
  "Location of `.gitignore' file templates collection."
  :type '(directory)
  :safe t)

(defgroup bvr/ig
  '((bvr/ig/gitignores-dir custom-variable))
  "Insert Gitignores Customisation.")

(defun bvr/ig/insert-gitignore ()
  "Insert at point, a \".gitignore\" template.

Read a filename from templates collection `bvr/ig/gitignores-git'; and
insert its content at point."
  (interactive)
  (let ((fname))
    (setq fname (read-file-name "Select Template: "
                                bvr/ig/gitignores-dir
                                nil t ".gitignore"))
    (unless (bvr/ig/gitignore? fname)
      (user-error "Not a `.gitignore': `%s'" fname))
    (insert (f-read fname))))

(defun bvr/ig/gitignore? (fname)
  "If FNAME is a gitignore template."
  
  (s-ends-with? ".gitignore" fname t))

(provide 'insert-gitignore)

;;; insert-gitignore.el ends here
