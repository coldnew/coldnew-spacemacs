;;; init.el --- configuration entry point.
(eval-when-compile (require 'cl))

;; Define emacs-dir where all the files live.
;; We set `user-emacs-directory' here so we can use command-line
;; switch different emacs configuration like following:
;;
;;    emacs -q -l ~/coldnew-spacemacs/init.el
;;

;; Bind init.el to spacemacs, so we can make a entry point to
;; test spacemacs without change original emacs config.
(let* ((emacs-dir
        (file-name-directory (or load-file-name (buffer-file-name))))
       (spacemacs-init
        (concat emacs-dir "spacemacs/init.el")))

  ;; change user-emacs-directory
  (setq user-emacs-directory (file-name-directory spacemacs-init))

  ;; load my config
  (load (concat emacs-dir "config.el"))

  ;; load spacemacs
  (load spacemacs-init))

;;; init.el ends here.
