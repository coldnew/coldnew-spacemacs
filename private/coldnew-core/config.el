;;; config.el --- coldnew-core Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;; Copyright (c) 2015 Yen-Chin, Lee
;;
;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>
;; URL: https://github.com/coldnew/coldnew-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'delsel)

;; use bar type cursor instead of box
(add-hook 'minibuffer-setup-hook '(lambda () (setq cursor-type 'bar)))

;; Create *scratch* automatically
(run-with-idle-timer 1 t
                     '(lambda ()
                        (unless (get-buffer "*scratch*")
                          (with-current-buffer (get-buffer-create "*scratch*")
                            (lisp-interaction-mode)))))
;; linum
(eval-after-load 'linum '(progn (global-linum-mode 1)))

;; Make emacs show lambda in greek symbols
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
  (global-prettify-symbols-mode 1))

;;;; Easy PG
(eval-after-load 'epa-file
  '(progn
     ;; Control whether or not to pop up the key selection dialog.
     (setq epa-file-select-keys 0)
     ;; Cache passphrase for symmetric encryption.
     (setq epa-file-cache-passphrase-for-symmetric-encryption t)))

;; ido
(eval-after-load 'ido
  '(progn
     ;; ignore some unimport files
     (let ((file-list
            (list
             "*.o" "*.bin" "*.elc" "*.exe" "*.obj" "*.map" "*.a" "*.so" "*.ln" "*.mod"
             "Icon\n" "\\.DS_Store" "\\`__pycache__/")))
       (dolist (f file-list) (add-to-list 'ido-ignore-files f))
       )))
