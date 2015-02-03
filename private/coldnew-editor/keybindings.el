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

;;; X11

;; When under X11, make caps as control
(cond ((eq window-system 'x)
       ;; make caps lock a control key
       (shell-command "setxkbmap -option ctrl:nocaps")))

;;; Mac OSX
;;
;; Bind key under OSX as following
;;    Option  -> Super
;;    Command -> Meta

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; Normal

;; Insert
(my/set-key evil-insert-state-map
            "C-a" 'beginning-of-line
            "C-e" 'end-of-line
            "C-o" 'evil-execute-in-normal-state
            "C-d" 'hungry-delete-forward
            "C-l" 'hungry-delete-backward
            "C-/" 'undo-tree-undo
            "M-/" 'undo-tree-redo
            "C-n" 'evil-next-line
            "C-p" 'evil-previous-line
            "C-f" 'evil-forward-char
            "C-b" 'evil-backward-char
            "M-<SPC>" 'insert-U200B-char
            "s-<SPC>" 'insert-U200B-char
            "C-x C-l" 'recenter
            "M-l" 'backward-delete-word
            "M-d" 'forward-delete-word
            )

;;; ex
(my/set-ex-cmd 'nil
               "ag" 'helm-ag-this-file
               )

;;; minibuffer
(my/set-key minibuffer-local-map
            "M-l" 'backward-kill-word
            "M-p" 'previous-history-element
            "M-n" 'next-history-element
            "C-g" 'minibuffer-keyboard-quit
            "C-u" 'coldnew/minibuffer-clear
            "M-t" 'coldnew/minibuffer-switch-tmpdir
            "M-h" 'coldnew/minibuffer-switch-homedir
            "M-w" 'coldnew/minibuffer-switch-workspace
            "M-r" 'coldnew/minibuffer-switch-rootdir
            )
