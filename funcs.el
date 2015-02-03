
(defun load-contrib-if-exist (pkg)
  (let* ((contrib-dir (concat emacs-dir "spacemacs/contrib/" pkg))
         (config-list
          (list "config.el" "packages.el" "extensions.el" "funcs.el" "keybindings.el")))

    (when (file-exists-p contrib-dir)
      (dolist (f config-list)
        (let ((conf (concat contrib-dir "/" f)))
          (when (file-exists-p conf) (load-file conf)))))))
