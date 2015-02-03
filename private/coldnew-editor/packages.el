;;; coldnew-editor.el --- coldnew-editor Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;; Copyright (c) 2015 Yen-Chin, Lee
;;
;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>
;; URL: https://github.com/coldnew/coldnew-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defvar coldnew-editor-packages
  '(
    ;; package coldnew-editors go here
    evil
    evil-leader
    hungry-delete
    undo-tree
    pangu-spacing
    ascii
    rainbow-mode
    linum-off
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar coldnew-editor-excluded-packages '()
  "List of packages to exclude.")

;;; ascii
(defun coldnew-editor/init-ascii ()
  "Initialize ascii"
  (use-package ascii :defer t :init
               (progn
                 ;; ascii-toggle
                 (defun ascii-toggle ()
                   "Toggle ascii-mode."
                   (interactive) (if (not (ascii-off)) (ascii-on)))

                 ;; alias ascii to ascii-toggle
                 (defalias 'ascii 'ascii-toggle))))

;;; rainbow-mode
(defun coldnew-editor/init-rainbow-mode ()
  "Initialize ascii"
  (use-package rainbow-mode :defer t))

;;; hungry-delete
(defun coldnew-editor/init-hungry-delete ()
  "Initialize hungry-delete"
  (use-package hungry-delete
               :defer t
               :init (global-hungry-delete-mode)
               :config
               (progn
                 ;; only horizontal whitespace
                 (setq-default hungry-delete-chars-to-skip " \t\f\v"))))

;;; pangu-spacing
(defun coldnew-editor/init-pangu-spacing ()
  (use-package pangu-spacing
               :defer t
               :init (global-pangu-spacing-mode 1)
               :config
               (progn
                 (add-hook 'org-mode-hook
                           '(lambda ()
                              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
                 )))

;;; linum-off
(defun coldnew-editor/init-linum-off ()
  (use-package linum-off
               :init
               (setq linum-disabled-mode-list
                     '(eshell-mode shell-mode term-mode erc-mode compilation-mode
                                   woman-mode w3m-mode calendar-mode org-mode
                                   ))))
