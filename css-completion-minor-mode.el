;;; css-helpers.el --- Browse info index with helm -*- lexical-binding: t -*-

;; Copyright (C) 2020 B.V. Raghav <r@bvraghav.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl)

;; (defun bvr/css/complete-class ()
;;   (interactive)
;;   (let ((new-word (completing-read "CSS Class: "
;;                                    (bvr/css/class-list) 
;;                                    nil nil (word-at-point t))))
;;     (cl-destructuring-bind (beg . end)
;;         (bounds-of-thing-at-point 'word)
;;       ;; (message "beg: %s end: %s" beg end)
;;       (delete-region beg end))
;;     (insert new-word)))

(defgroup bvr/css nil "BVR's CSS Helpers group")
(defcustom bvr/css/valid-modes '(mhtml-mode)
  "Set of applicable modes for css class completion-at-point"
  :group 'bvr/css
  :type '(repeat symbol))

(defcustom bvr/css/completion-attributes
  '("class")
  "Attribute values to apply completion for"
  :group 'bvr/css
  :type '(repeat string))

(defun bvr/css/class-list ()
  (with-current-buffer "yabuf" (erase-buffer))
  (call-process-shell-command
   (format "rg -oINr '$1' --type-add 'css:css' -t 'css' '%s' %s | sort -u"
           "\\.([A-Za-z][-_:A-Z0-9a-z]{2,})[, {]"
           (projectile-ensure-project (projectile-project-root)))
   nil "yabuf")
  (with-current-buffer "yabuf"
    (split-string (buffer-string) "\n")))

(defun bvr/css/complete-class ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'symbol)
    (completion-in-region beg end (bvr/css/class-list))))

;; (defun bvr/css/completion-at-point ()
;;   (when (bvr/css/valid-completion-context-p)
;;     (cl-destructuring-bind (beg . end)
;;         (bounds-of-thing-at-point 'symbol)
;;       (list beg end (bvr/css/complete-class)))))

;; (defun bvr/css/valid-completion-context-p ()
;;   (letrec ((or-sc (lambda (predicate el &rest seq)
;;                     (or (predicate el)
;;                         (and seq (or-sc seq))))))
;;    (or (member major-mode bvr/css/valid-modes)
;;        (or-sc #'derived-mode-p bvr/css/valid-modes))))

;; (add-hook 'completion-at-point-functions
;;           #'bvr/css/completion-at-point)

(defun bvr/css/point-at-attr-p (attr)
  (looking-back (format "\\<%s=\"[^\"]+" attr)))

(defun bvr/css/any-attr-p (attr &rest attrs)
  (or (bvr/css/point-at-attr-p attr)
      (and (apply #'bvr/css/any-attr-p attrs)
           attrs)))

(defun bvr/css/completable-p ()
  (apply #'bvr/css/any-attr-p
         bvr/css/completion-attributes))

(defun bvr/css/completion-filter (cmd)
  (when (bvr/css/completable-p) cmd))

(defconst bvr/css/complete-maybe
  '(menu-item "" bvr/css/complete-class
              :filter bvr/css/completion-filter)
  "Optional CSS class completion based on context")

(defconst bvr/css/keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)]     bvr/css/complete-maybe)
    (define-key map (kbd "TAB") bvr/css/complete-maybe)
    (define-key map (kbd "<tab>") bvr/css/complete-maybe)
    map)
  "Keymap for CSS completion minor mode")

(define-minor-mode css-completion-minor-mode
  "Complete css classes in the project"
  :keymap bvr/css/keymap))

(provide 'css-completion-minor-mode)
