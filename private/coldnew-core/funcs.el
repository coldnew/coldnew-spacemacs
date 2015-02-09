;;; funcs.el --- coldnew-core Layer funcs File for Spacemacs
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

;;;; Local functions

(defun my/set-key (keymap key def &rest bindings)
  (evil-leader--def-keys keymap key def bindings))

(defun my/set-ex-cmd (nothing cmd fn &rest bindings)
  (flet ((set-ex-cmd (cmd fn bindings)
                     (while cmd
                       (evil-ex-define-cmd cmd fn)
                       (setq cmd (pop bindings)
                             fn (pop bindings)))))
    (set-ex-cmd cmd fn bindings)))

;;;; Commands

(defun insert-U200B-char ()
  "Insert <U200B> char, this character is nice use in org-mode."
  (interactive)
  (insert "\ufeff"))

(defun create-private-layer (name)
  "spacemacs wrapper for create coldnew's spacemacs private layer."
  (require 'noflet)
  (interactive "sConfiguration layer name: ")
  (noflet ((configuration-layer//get-private-layer-dir
            (name)
            (concat (file-name-as-directory my/private-layer) name "/")))
          (configuration-layer/create-layer name)
          ))

;;;; Commands (private)

(defun coldnew/minibuffer-clear ()
  "Clear minibuffer"
  (interactive) (kill-line 0))

(defun coldnew/minibuffer-switch-dir (path)
  "Clear mimibuffer and insert dir path"
  (kill-line 0) (insert path))

(defun coldnew/minibuffer-switch-rootdir ()
  "Switch to tmpdir in minibuffer"
  (interactive) (coldnew/minibuffer-switch-dir "/"))

(defun coldnew/minibuffer-switch-tmpdir ()
  "Switch to tmpdir in minibuffer"
  (interactive) (coldnew/minibuffer-switch-dir "/tmp/"))

(defun coldnew/minibuffer-switch-homedir ()
  "Switch to ~/ in minibuffer"
  (interactive) (coldnew/minibuffer-switch-dir "~/"))

(defun coldnew/minibuffer-switch-workspace ()
  "Switch to ~/Workspace in minibuffer"
  (interactive) (coldnew/minibuffer-switch-dir "~/Workspace/"))

;;;; Commands
