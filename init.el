;;; init.el --- configuration entry point -*- no-byte-compile: t -*-
(eval-when-compile (require 'cl-lib))

;; Define emacs-dir where all the files live.
(defconst emacs-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Where all my emacs files live, in most case, thie value is `~/.emacs.d/'")

;; since emacs 24.4, new option `load-prefer-newer' has been
;; introduce, which make me never accidentally using outdated compiled files.
(setq load-prefer-newer t)

;; change user-emacs-directory
;; We set `user-emacs-directory' here so we can use command-line
;; switch different emacs configuration like following:
;;
;;    emacs -q -l ~/coldnew-spacemacs/init.el
(setq user-emacs-directory (file-name-directory emacs-dir))

(require 'org)
(setq org-confirm-babel-evaluate nil)

;; Load config.org from emacs-dir
(org-babel-load-file (expand-file-name "config.org" emacs-dir))

;;; init.el ends here.
