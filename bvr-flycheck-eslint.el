;;; bvr-flycheck-eslint --- Flycheck eslint adaptor
;;; ---------------------------------------------------

;;; Usage:
;;; ---------------------------------------------------
;;; Include this in your load-path, and either, with
;;; use-package :
;;;
;;; (use-package bvr-flycheck-eslint)
;;;
;;; or otherwise simply
;;;
;;; (require 'bvr-flycheck-eslint)

;;; Commentary:
;;; ---------------------------------------------------
;;; This code is merely an adaptation for flycheck
;;; eslint to use `source' instead of `source-original'
;;; as the argument for commandline.
;;;
;;; For more details, ref:
;;; https://github.com/flycheck/flycheck/issues/2011

;;; Code:
;;; ---------------------------------------------------
(defun bvr-flycheck-eslint--find-working-directory (_checker)
  "Look for a working directory to run ESLint CHECKER in.

This will be the directory that contains the `node_modules'
directory.  If no such directory is found in the directory
hierarchy, it looks first for `.eslintignore' and then for
`.eslintrc' files to detect the project root."
  (let* ((file (or (buffer-file-name) default-directory))
         (file (when file
                 (or (locate-dominating-file file "node_modules")
                     (locate-dominating-file file ".eslintignore")
                     (locate-dominating-file file ".eslintrc")
                     (locate-dominating-file file ".eslintrc.js")
                     (locate-dominating-file file ".eslintrc.json")
                     (locate-dominating-file file ".eslintrc.yaml")
                     (locate-dominating-file file ".eslintrc.yml")))))
    ;; (when file
    ;;   (message "Found working directory: %s" file))
    file))


(flycheck-def-executable-var bvr-javascript-eslint "eslint")
(flycheck-define-command-checker 'bvr-javascript-eslint
  "A Javascript syntax and style checker using eslint.

See URL `https://eslint.org/'."
  :command '("eslint" "--format=json"
             (option-list "--rulesdir" flycheck-eslint-rules-directories)
             (eval flycheck-eslint-args)
             "--stdin-filename" source)
  :standard-input t
  :error-parser 'flycheck-parse-eslint
  :enabled #'(lambda () (flycheck-eslint-config-exists-p))
  :modes '(js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                   typescript-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
  :working-directory #'bvr-flycheck-eslint--find-working-directory
  :verify
  #'(lambda (_)
      (let* ((default-directory
               (flycheck-compute-working-directory 'bvr-javascript-eslint))
             (have-config (flycheck-eslint-config-exists-p)))
        (list
         (flycheck-verification-result-new
          :label "config file"
          :message (if have-config "found" "missing or incorrect")
          :face (if have-config 'success '(bold error))))))
  :error-explainer
  #'(lambda (err)
      (let ((error-code (flycheck-error-id err))
            (url "https://eslint.org/docs/rules/%s"))
        (and error-code
             ;; skip non-builtin rules
             (not ;; `seq-contains-p' is only in seq >= 2.21
              (with-no-warnings (seq-contains error-code ?/)))
             `(url . ,(format url error-code))))))


(add-to-list 'flycheck-checkers 'bvr-javascript-eslint)

(provide 'bvr-flycheck-eslint)
;;; bvr-flycheck-eslint.el ends here
