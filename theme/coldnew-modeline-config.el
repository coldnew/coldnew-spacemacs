;;; coldnew-modeline-config.el --- coldnew's modeline-config theme.

;; Copyright (C) 2015 Yen-Chin, Lee.

;; Author: coldnew <coldnew.tw@gmail.com>
;; Kyewords: themes
;; Version: 0.1
;; X-Original-Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'powerline)
(require 'powerline-evil)


(defun coldnew/mode-line-prepare-left ()
  (let* ((active (powerline-selected-window-active))
         (mode-line (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (separator-left (intern (format "powerline-%s-%s"
                                         powerline-default-separator
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          powerline-default-separator
                                          (cdr powerline-default-separator-dir)))))
    (append
     ;; Evil-state indicator
     (let ((evil-face (powerline-evil-face)))
       (append (if evil-mode
                   (list (funcall separator-right face2 evil-face)
                         (powerline-raw " " evil-face)
                         (powerline-raw (powerline-evil-tag) evil-face 'l)
                         (powerline-raw " " evil-face)
                         (funcall separator-left evil-face mode-line)))

               (list (powerline-minor-modes face2 'l)
                     (powerline-raw " " face2)
                     (funcall separator-right face2 face1)))
       (list (powerline-raw (powerline-evil-tag) evil-face)
             (funcall separator-left evil-face mode-line)))
     (list
      ;; Buffer state indicator
      (cond (buffer-read-only
             (propertize "RO" 'face 'mode-line-read-only-face))
            ((buffer-modified-p)
             (propertize "**" 'face 'mode-line-modified-face))
            (t "--"))

      (powerline-buffer-id nil 'l)
      (powerline-raw " ")
      (funcall separator-left mode-line face1)

      ;; major mode
      (funcall separator-left face1 mode-line )
      (powerline-raw " " mode-line)
      (powerline-major-mode mode-line 'l)
      (powerline-raw "  " mode-line)
      (funcall separator-left mode-line face1)

      ;; VC
      (powerline-vc face1)
      (powerline-raw " " face1 '1)
      (funcall separator-left face1 face2)
      ))))


(defun coldnew/mode-line-prepare-right ()
  (let* ((active (powerline-selected-window-active))
         (mode-line (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (separator-left (intern (format "powerline-%s-%s"
                                         powerline-default-separator
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          powerline-default-separator
                                          (cdr powerline-default-separator-dir)))))
    (append
     (list
      (powerline-process face2)
      (funcall separator-right face2 face1)
      (when (boundp 'erc-modified-channels-object)
        (powerline-raw erc-modified-channels-object face2 'l))
      (powerline-raw "%4l" face1 'r)
      (powerline-raw ":" face1)
      (powerline-raw "%3c" face1 'r)
      (funcall separator-right face1 mode-line)
      (powerline-raw " ")
      (powerline-raw "%6p" nil 'r)
      (powerline-buffer-size nil 'r)
      (powerline-hud face2 face1))
     )))

(defun coldnew/mode-line-prepare ()
  (let* ((active (powerline-selected-window-active))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (lhs (coldnew/mode-line-prepare-left))
         (rhs (coldnew/mode-line-prepare-right)))
    (concat (powerline-render lhs)
            (powerline-fill face2 (powerline-width rhs))
            (powerline-render rhs))))

;; Setup my mode-line

(setq powerline-default-separator 'arrow)

(setq mode-line-format
      '("%e" (:eval (coldnew/mode-line-prepare))))

(defun spacemacs/mode-line-prepare ()
  "Overwrite spacemacs's mode-line-prepare so I can use my own modeline."
  (coldnew/mode-line-prepare))


(provide 'coldnew-modeline-config)
;;; coldnew-modeline-config.el ends here.
