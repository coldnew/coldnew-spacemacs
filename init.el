;;; init.el --- configuration entry point.

;; Define emacs-dir where all the files live.
;; We set `user-emacs-directory' here so we can use command-line
;; switch different emacs configuration like following:
;;
;;    emacs -q -l ~/.coldnew-emacs/init.el
;;
;; I bind this init.el to spacemacs, so we can make a entry point to
;; test spacemacs without change my original emacs config.
(setq user-emacs-directory
      (file-name-directory
       (concat(or load-file-name (buffer-file-name)) "/spacemacs/init.el")))

;;; init.el ends here.
