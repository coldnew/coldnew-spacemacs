;;; init.el --- configuration entry point.
(eval-when-compile (require 'cl))

;; Define emacs-dir where all the files live.
(defconst emacs-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Where all my emacs files live, in most case, thie value is `~/.emacs.d/'")

(defconst spacemacs-dir
  (directory-file-name (concat emacs-dir "spacemacs"))
  "Where the spacemacs locate in emacs-dir.")

(defadvice configuration-layer/delete-orphan-packages (around null-func activate)
  "Overwrite the spacemacs's `configuration-layer/delete-orphan-packages'
to make it not remove any orphan packages.")

;; Bind init.el to spacemacs, so we can make a entry point to
;; test spacemacs without change original emacs config.
;; We also use config.el replace spacemacs's .spacemacs file.
(let* ((spacemacs-init
        (concat (file-name-as-directory spacemacs-dir) "init.el")))

  ;; change user-emacs-directory
  ;; We set `user-emacs-directory' here so we can use command-line
  ;; switch different emacs configuration like following:
  ;;
  ;;    emacs -q -l ~/coldnew-spacemacs/init.el
  ;;
  (setq user-emacs-directory (file-name-directory spacemacs-init))

  ;; load my config instead of make spacemacs load ~/.spacemacs
  (load (concat emacs-dir "spacemacs.el"))

  ;; Initial spacemacs, our emacs run on top of it
  (load spacemacs-init))

;; Load up org-mode and org-babel
(require 'org)
(setq org-confirm-babel-evaluate nil)

;; Load config.org from emacs-dir
(org-babel-load-file (expand-file-name "config.org" emacs-dir))

;;; init.el ends here.
