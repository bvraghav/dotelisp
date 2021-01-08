;; The MIT License (MIT)
;; Copyright (c) 2021 B.V. Raghav

;; Permission is hereby granted, free of charge, to any
;; person obtaining a copy of this software and associated
;; documentation files (the ``Software''), to deal in the
;; Software without restriction, including without
;; limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the
;; following conditions:

;; The above copyright notice and this permission notice
;; shall be included in all copies or substantial portions
;; of the Software.

;; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF
;; ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
;; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
;; ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

(require 'seq)

(defgroup screen-capture nil "Screen Capture Group")

(defcustom screen-capture-binary "~/.local/bin/screen-capture"
  "Location of screen capture binary."
  :type '(file :must-match t)
  :group 'screen-capture)

(defcustom screen-capture-destination "~/.screen-capture/dest"
  "Path to screen capture destination folder."
  :type 'directory
  :group 'screen-capture)

(defcustom screen-capture-destinations
  ;; '(("www" . "~/public_html/images")
  ;;   ("org" . "~/org/images")
  ;;   ("roam" . "~/org-roam/images")
  ;;   ("tmp" . "~/tmp/screen-capture"))
  nil
  "List of screen capture destinations."
  :type '(alist :key-type string
                :value-type directory)
  :group 'screen-capture)

(defcustom screen-capture-styles
  '("complete" "active" "window" "patch")
  "List of capture styles"
  :type '(list string))

(defvar screen-capture--binary
  (expand-file-name screen-capture-binary))

(defun screen-capture (arg)
  (interactive
   (list (completing-read "Screen Capture: "
                          (screen-capture--options)
                          nil t)))
  (if (member arg (screen-capture--destination-names))
      (progn (screen-capture--set-dest arg)
             (call-interactively #'screen-capture))
    (and (message "Calling process: %s %s" screen-capture--binary arg)
         (call-process screen-capture--binary nil nil nil arg)
         ;; (kill-new (screen-captured-last))
         )))

(defun screen-captured-last ()
  (interactive)
  (let* ((asc-modif #'screen-capture--time-ascending)
         (default-directory
           (file-name-as-directory screen-capture-destination))
         (last-capture
          (caar (reverse (sort (screen-captured-all) asc-modif)))))
    (file-truename last-capture)))

(defun screen-captured-all ()
  (interactive)
  (directory-files-and-attributes default-directory))

(defun screen-capture--destination-names ()
  (mapcar #'car (screen-capture--get-destinations)))

(defun screen-capture--get-destinations ()
  (or screen-capture-destinations
      (let* ((default-directory (file-name-directory screen-capture-destination))
             (dest (file-truename screen-capture-destination))
             (get-dest (lambda (x) (file-truename (car x))))
             (dest-filter (lambda (x)
                            (and (stringp (nth 1 x))
                                 (not (string= dest (get-dest x))))))
             (dest-siblings (directory-files-and-attributes default-directory))
             (dest-candidates (seq-filter #'dest-filter dest-siblings))
             (candidates-to-alist (lambda (x) (cons (car x) (cadr x))))
             (dest-alist (mapcar #'candidates-to-alist dest-candidates)))
        dest-alist)))

(defun screen-capture--options ()
  (concatenate 'list
               (screen-capture--destination-names)
               screen-capture-styles))

(defun screen-capture--set-dest (location)
  (let* ((dest screen-capture-destination)
         (true-loc
          (expand-file-name location
                            (file-name-directory dest)))
         (true-dest
          (file-truename
           (cdr (assoc location (screen-capture--get-destinations))))))
    (make-directory true-dest t)
    (make-symbolic-link true-dest true-loc t)
    (message "screen-capture--set-dest: %s -> %s" screen-capture-destination location)
    (make-symbolic-link location screen-capture-destination t)))

(defun screen-capture--time-ascending (x y)
  (time-less-p (nth 6 x) (nth 6 y)))

(provide 'screen-capture)
