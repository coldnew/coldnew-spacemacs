;;; packages.el --- coldnew-core Layer packages File for Spacemacs
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

(defvar coldnew-core-packages
  '(
    ;; package coldnew-cores go here
    evil
    evil-leader
    hungry-delete
    undo-tree
    pangu-spacing
    ascii
    rainbow-mode
    linum-off
    flx-ido
    f
    cpputils-cmake
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar coldnew-core-excluded-packages '()
  "List of packages to exclude.")

;;; ascii
(defun coldnew-core/init-ascii ()
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
(defun coldnew-core/init-rainbow-mode ()
  "Initialize ascii"
  (use-package rainbow-mode :defer t))

;;; hungry-delete
(defun coldnew-core/init-hungry-delete ()
  "Initialize hungry-delete"
  (use-package hungry-delete
               :defer t
               :init (global-hungry-delete-mode)
               :config
               (progn
                 ;; only horizontal whitespace
                 (setq-default hungry-delete-chars-to-skip " \t\f\v"))))

;;; pangu-spacing
(defun coldnew-core/init-pangu-spacing ()
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
(defun coldnew-core/init-linum-off ()
  (use-package linum-off
               :init
               (setq linum-disabled-mode-list
                     '(eshell-mode shell-mode term-mode erc-mode compilation-mode
                                   woman-mode w3m-mode calendar-mode org-mode
                                   ))))

;;; flx-ido
(defun coldnew-core/init-flx-ido ()
  (use-package flx-ido
               :defer t
               :init (flx-ido-mode 1)
               :config (progn
                         (setq ido-enable-flx-matching t)
                         ;; disable ido faces to see flx highlights.
                         (setq ido-use-faces nil))))

;;; f
(defun coldnew-core/init-f ()
  (use-package f :defer t))

;;; cpputils-cmake
(defun coldnew-core/init-cpputils-cmake ()
  (use-package cpputils-cmake
               :defer t
               :config
               (progn
                 ;; Add support for C/C++ but skip system headers
                 (add-hook 'c-mode-common-hook
                           (lambda ()
                             (when (derived-mode-p 'c-mode 'c++-mode)
                               (if (not ((string-match "^/usr/local/include/.*" buffer-file-name)
                                         (string-match "^/usr/src/linux/include/.*" buffer-file-name))))
                               (cppcm-reload-all))
                             ))
                 )))

;; For each package, define a function coldnew-core/init-<package-coldnew-core>
;;
;; (defun coldnew-core/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
