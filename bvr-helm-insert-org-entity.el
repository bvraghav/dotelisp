;;; bvr-helm-insert-org-entity.el --- Insert Unicode as Org Entity -*- lexical-binding: t -*-

;; Author: Raghav B. Venkataramaiyer
;; Maintainer: Raghav B. Venkataramaiyer
;; Version: v0.0.1
;; Package-Requires: (s f dash)
;; Homepage: https://github.com/bvraghav/dotelisp.git
;; Keywords: helm,unicode,utf8,completion


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

;; Insert Unicode as Org Entity.

;; Usage:
;; (helm :sources (bvr/hioe/build-sources-all))

;; Courtesy: Refactored from Kitchin Lab's code
;; https://kitchingroup.cheme.cmu.edu/blog/2015/11/21/Insert-org-entities-into-org-mode-with-helm/

;;; Code:


(require 's)
(require 'f)
(require 'dash)

;; (char-from-name "NON-BREAKING SPACE")
;; (char-to-name ? )
;; (char-to-name (string-to-char " "))

(defun bvr/helm-insert-org-entity (_)
  "Helm interface to insert an entity from `org-entities'.

Insert org-entity by default.

F1...F5 inserts other code (use TAB in helm buffer to view details)"
  (interactive "i")
  (helm :sources (bvr/hioe/build-sources-all)))

(cl-defstruct
    (bvr/hioe/struct-candidate
     (:conc-name bvr/hioe/candidate/)
     ;; (:constructor nil)
     (:constructor
      bvr/hioe/candidate
      (org-name latex latex-mathp html ascii latin1 utf8
                &aux
                (utf8-name (if (and (stringp utf8) (s-present? utf8))
                               (char-to-name (string-to-char utf8))
                             ""))
                (display (format "%-45s \\%-7s %-8s %s"
                                 (format "`%s' %s" utf8 utf8-name)
                                 org-name latex html)))))
  (org-name "" :documentation "Name.

As a string, without the leading backslash.")
  (latex "" :documentation "LaTeX replacement.

In ready LaTeX, no further processing will take place.")
  (latex-mathp "" :documentation "LaTeX mathp.

Either t or nil.  When t this entity needs to be in math mode.")
  (html "" :documentation "HTML replacement.

In ready HTML, no further processing will take place.  Usually this will be an &...; entity.")
  (ascii "" :documentation "ASCII replacement.

Plain ASCII, no extensions.")
  (latin1 "" :documentation "Latin1 replacement.

Use the special characters available in latin1.")
  (utf8 "" :documentation "UTF-8 replacement.

Use the special characters available in utf-8.")
  (utf8-name "" :documentation "UTF-8 char name as string.")
  (display "" :documentation "Helm display for candidate."))

(defun bvr/hioe/action ()
  (helm-make-actions "Insert org-entity"      #'bvr/hioe/candidate/insert/org-name
                     "Insert UTF-8"           #'bvr/hioe/candidate/insert/utf8
                     "Insert LaTeX"           #'bvr/hioe/candidate/insert/latex
                     "Insert HTML"            #'bvr/hioe/candidate/insert/html
                     "Insert Character Name"  #'bvr/hioe/candidate/insert/utf8-name))

(defun bvr/hioe/candidate/insert/org-name (candidate)
  (message "Selected Org Name: %s" (bvr/hioe/candidate/org-name candidate))
  ;; (unless (bvr/hioe/struct-candidate-p candidate)
  ;;   (error "Not a candidate: %s" candidate))
  (insert (format "\\%s{}" (bvr/hioe/candidate/org-name candidate))))
(defun bvr/hioe/candidate/insert/utf8 (candidate)
  (insert (bvr/hioe/candidate/utf8 candidate)))
(defun bvr/hioe/candidate/insert/latex (candidate)
  (insert (bvr/hioe/candidate/latex candidate)))
(defun bvr/hioe/candidate/insert/html (candidate)
  (insert (bvr/hioe/candidate/html candidate)))
(defun bvr/hioe/candidate/insert/name (candidate)
  (insert (bvr/hioe/candidate/name candidate)))

(defun bvr/hioe/get-entities ()
  (append '("* User" "** User entities")
          org-entities-user
          org-entities))

(defun bvr/hioe/is-source-start? (item)
  (and (stringp item)
       (or (s-starts-with? "\* " item)
           (s-starts-with? "\*\* " item))))

;; (setq --lst (bvr/hioe/get-entities-as-pairs))

;; (setq --src (nth 1 --lst))

;; (setq --helm-src (bvr/hioe/build-source --src))

(setq bvr/hioe/--entities-pair nil)
(defun bvr/hioe/get-entities-as-pairs ()
  (or bvr/hioe/--entities-pair
      (setq bvr/hioe/--entities-pair
            (-partition 2 (-partition-by #'bvr/hioe/is-source-start?
                                         (bvr/hioe/get-entities))))))

(defun bvr/hioe/build-sources-all ()
  (let (h1
        (sources '())
        (msg ""))
   (dolist (pair (bvr/hioe/get-entities-as-pairs) sources)
     (pcase-let ((`(,h1 ,h2 ,source)
                  (bvr/hioe/build-source pair h1)))
       (add-to-list 'sources source))
     (setq msg (format "%s %d" msg (length sources))))
   (message "%s" msg)
   (reverse sources))
  ;; For some reason cl-loop hangs
  ;; (cl-loop for pair in (bvr/hioe/get-entities-as-pairs)
  ;;          and h1 = nil
  ;;          collect (pcase-let ((`(,h1 ,h2 ,source) (bvr/hioe/build-source pair h1)))
  ;;                    source))
  )

(defun bvr/hioe/build-source (&optional pair h1)
  "PAIR ≡ (raw-titles raw-candidates).

RAW-TITLES ≡ ([\"* h1\" \"** h2\"] ... \"** H2\")
RAW-CANDIDATES ≡ ORG-ENTITIES candidates.

Returns:
(H1 H2 SOURCE)
"
  (pcase-let* ((`(,raw-titles ,raw-candidates) pair)
               (`(,h1 ,h2 ,name) (bvr/hioe/build-source/get-title raw-titles h1))
               (candidates (--map (apply #'bvr/hioe/candidate it) raw-candidates))
               (candidates (bvr/hioe/build-source/get-candidates candidates)))
    ;; (user-error "Raw Candidates: %s" raw-candidates)
    ;; (user-error "Typeof candidate: car:%s cdar:%s"
    ;;             (type-of (caar candidates))
    ;;             (type-of (cdar candidates)))
    (list h1 h2 (helm-build-sync-source name
                  :candidates candidates
                  :action (bvr/hioe/action)))))

;; (setq --helm-src (bvr/hioe/build-source --src))

(defun bvr/hioe/build-source/get-title (raw-titles &optional h1)
  "RAW-TITLES ≡ ([\"* h1\" \"** h2\"] ... \"** H2\").

Returns:
(H1 H2 NAME)"
  (pcase-let ((`(,h1 ,h2) (-take-last 2 (nconc `(,h1) raw-titles))))
    (setq h2 (s-chop-left 2 h2))
    (list h1 h2 (concat h1 " - " h2))))

(defun bvr/hioe/build-source/get-candidates (candidates)
  "CANDIDATES ≡ (CANDIDATE ...).

Returns:
((DISPLAY . CANDIDATE) ...)"
  (-zip-pair (-map #'bvr/hioe/candidate/display candidates)
              candidates))

;; ----------------------------------------------------
;; DEBUG
;; ----------------------------------------------------

;; (setq --lst (bvr/hioe/get-entities-as-pairs))

;; (setq --src (nth 0 --lst))

;; (bvr/hioe/build-source/get-title (car --src))

;; (setq --helm-src (bvr/hioe/build-source --src))

;; (helm :sources (bvr/hioe/build-sources-all))


(provide 'bvr-helm-insert-org-entity)

;;; bvr-helm-insert-org-entity.el ends here
