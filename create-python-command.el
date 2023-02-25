(defun bvr/create-python-command (beg end)
  "Create python commands from a set of newline-separated options "
  (interactive "*r")
  (let* ((names (split-string
                 (buffer-substring beg end)
                 "\n"))
         (names (remove-if #'string-empty-p names))
         (names (mapcar #'bvr/cpc-trim names))
         (kebabs (mapcar #'bvr/cpc-as-kebab names))
         (snakes (mapcar #'bvr/cpc-as-snake names)))
    (bvr/cpc-get-string kebabs snakes)))

(defun bvr/insert-python-command (beg end)
  (interactive "*r")
  (let* ((cpc-string (bvr/create-python-command beg end)))
    (delete-region beg end)
    (insert cpc-string)))

(defun bvr/cpc-get-string (kebab-names snake-names)
  (with-temp-buffer
    (call-interactively #'python-mode)
    (insert "
@click.command(context_settings = dict(
  show_default                  = True,
  help_option_names             = ['-h','--help']
))
")
    (dolist (name kebab-names)
      (let* ((typeattr (bvr/cpc-typeattr name))
             (fmt "@click.option('--%s'%s)\n"))
        (insert (format fmt name typeattr))))
    (insert "\ndef main(\n")
    (dolist (name snake-names)
      (insert (format "%s,\n" name)))
    (insert "):\n  pass")
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun bvr/cpc-as-kebab (s)
  (string-replace " " "-" s))

(defun bvr/cpc-as-snake (s)
  (string-replace " " "_" s))

(defun bvr/cpc-trim (s)
  (string-trim s "# " " "))

(defun bvr/cpc-pathp (name)
  (or (bvr/cpc-filep name) (bvr/cpc-dirp name)))

(defun bvr/cpc-filep (name)
  (string-suffix-p "path" name))

(defun bvr/cpc-dirp (name)
  (string-suffix-p "dir" name))


(defun bvr/cpc-typeattr (name)
  (let* ((fmt ", type=click.Path(
exists=False, %s=False, path_type=Path,
)")
         (not-okay (if (bvr/cpc-filep name) "dir_okay"
                     "file_okay")))
    (if (bvr/cpc-pathp name)
        (format fmt not-okay)
      "")))


(provide 'create-python-command)
