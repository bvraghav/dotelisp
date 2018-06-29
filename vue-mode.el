(require 'sgml-mode)

(defvar vue-constants nil)

(defvar vue-keywords
  '("template" "script" "style"))

;; I'd probably put in a default that you want, as opposed to nil
(defvar vue-tab-width nil "Width of a tab for VUE mode")

;; Two small edits.
;; First is to put an extra set of parens () around the list
;; which is the format that font-lock-defaults wants
;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; you were very close
(defvar vue-font-lock-defaults
  `((
     ;; stuff between double quotes
     ("\"\\.\\*\\?" . font-lock-string-face)
     ;; ; : , ; { } =>  @ $ = are all special elements
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ( ,(regexp-opt vue-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt vue-constants 'words) . font-lock-constant-face)
     )))

(defvar vue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map html-mode-map)
    map))

(define-derived-mode vue-mode html-mode "V"
  "VUE mode is a major mode for editing VUE files"
  ;; you again used quote when you had '((vue-hilite))
  ;; I just updated the variable to have the proper nesting (as noted above)
  ;; and use the value directly here
  (setq font-lock-defaults vue-font-lock-defaults)
  
  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when vue-tab-width
    (setq tab-width vue-tab-width))
  
  ;; for comments
  ;; ;; overriding these vars gets you what (I think) you want
  ;; ;; they're made buffer local when you set them
  ;; (setq comment-start "#")
  ;; (setq comment-end "")
  
  (modify-syntax-entry ?# "< b" vue-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" vue-mode-syntax-table)
  
  ;; Note that there's no need to manually call `vue-mode-hook'; `define-derived-mode'
  ;; will define `vue-mode' to call it properly right before it exits
  )

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(defun vue-part-end (part-name)
  (save-excursion
    (goto-char (point-max))
    (search-backward
     (format "</%s>" part-name))
    (backward-char)
    (point)))

(defun vue-part-beg (part-name)
  (save-excursion
    (goto-char (point-min))
    (search-forward
     (format "<%s" part-name))
    (search-forward ">")
    (forward-char)
    (point)))

(defun vue-part-buffer (part-name)
  (format "*VUE %s: %s*"
	  (capitalize part-name)
	  (buffer-name (current-buffer))))

(defun vue-edit-part (part-name)
  (let ((current-point (point))
	(part-beg (vue-part-beg part-name))
	(part-end (vue-part-end part-name))
	(part-buf (vue-part-buffer part-name)))
    (edit-indirect-region part-beg part-end 't)
    (rename-buffer part-buf)

    ;; TODO Try to restore point without this dirty hack
    (goto-char (- (1+ current-point) part-beg)))
  (call-interactively 'delete-window))

(defun vue-edit-template ()
  (interactive)
  (vue-edit-part "template")
  (html-mode))

(defun vue-edit-script ()
  (interactive)
  (vue-edit-part "script")
  (js2-mode))

(defun vue-edit-css ()
  (interactive)
  (vue-edit-part "style")
  (css-mode))

;; ;; Does not work as desired.. Shunned for now
;; (defun vue-kill-after-commit ()
;;   (make-local-variable 'edit-indirect-after-commit-functions)
;;   (add-to-list 'edit-indirect-after-commit-functions
;; 	       #'delete-windows-on))
;; (add-hook 'vue-mode-hook #'vue-kill-after-commit)

;; Key Bindings
(define-prefix-command 'vue-edit-map)
(define-key vue-mode-map (kbd "C-c C-e") 'vue-edit-map)
(define-key vue-edit-map (kbd "C-t") #'vue-edit-template)
(define-key vue-edit-map (kbd "t") #'vue-edit-template)
(define-key vue-edit-map (kbd "C-s") #'vue-edit-script)
(define-key vue-edit-map (kbd "s") #'vue-edit-script)
(define-key vue-edit-map (kbd "C-c") #'vue-edit-css)
(define-key vue-edit-map (kbd "c") #'vue-edit-css)

(provide 'vue-mode)
