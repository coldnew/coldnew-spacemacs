;;; powerline-coldnew-theme.el --- coldnew's emacs powerline-theme.

;;; Commentary:
;;
;;  This theme is based on `powerline-evil-center-color-theme'
;;

;;; Code:

(require 'powerline)
;;(require 'powerline-evil)

;; TODO: remove this
(setq mode-line-format-old-old-old
      '((" "
         mode-line-mule-info
         ;; read-only or modified status
         (:eval
          (cond (buffer-read-only
                 (propertize "RO" 'face 'mode-line-read-only-face))
                ((buffer-modified-p)
                 (propertize "**" 'face 'mode-line-modified-face))
                (t "--")))
         "   "
         (when (featurep 'evil)
           (:eval (evil-mode-string)))
         "   "
         mode-line-buffer-identification
         " " (:eval (mode-line-buffer-permissions))
         "   "
         ;; major-mode name
         (:eval (mode-line-major-mode))
         "   "
         ;; line and column
         "("
         (:eval (propertize "%02l" 'face 'font-lock-type-face))
         ","
         (:eval (propertize "%02c" 'face 'font-lock-type-face))
         ")"

         "   "
         (vc-mode vc-mode)
         "   "
         ;; relative position, size of file
         "["
         (:eval (propertize "%p" 'face 'font-lock-constant-face)) ;; % above top
         "/"
         (:eval (propertize "%I" 'face 'font-lock-constant-face)) ;; size
         "] "
         (when (featurep 'motivation)
           (:eval (motivation-display)))
         )))

;; TODO: remove this
(defun mode-line-buffer-permissions ()
  "Get buffer-file permissions."
  (when (buffer-file-name)
    (format "-%04o-" (file-modes (buffer-file-name)))))


(defun powerline-simpler-vc-mode (s)
  ;; (if s
  ;;     (replace-regexp-in-string "Git:" ":" s)
  ;;   s)


  )

;;;###autoload
(defun powerline-coldnew-theme ()
  "Powerline's coldnew them with the evil state in color."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (append
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
                                 (cond (buffer-read-only
                                        (propertize "RO" 'face 'mode-line-read-only-face))
                                       ((buffer-modified-p)
                                        (propertize "**" 'face 'mode-line-modified-face))
                                       (t "--"))

                                 (powerline-buffer-id nil 'l)
                                 (powerline-raw " ")
                                 (funcall separator-left mode-line face1)
                                 ;; (powerline-narrow face1 'l)

                                 ;;   (powerline-vc face1)
                                 ;;   (powerline-raw " " face1 '1)
                                 ;;   (powerline-narrow face1 'l)

                                 (funcall separator-left face1 mode-line )
                                 (powerline-raw " " mode-line)
                                 (powerline-major-mode mode-line 'l)
                                 (powerline-raw "  " mode-line)
                                 (funcall separator-left mode-line face2)

                                   (powerline-vc face2)
                                   (powerline-raw " " face2 '1)
                                   (powerline-narrow face2 'l)

                                 ;; (when (and (boundp 'which-func-mode) which-func-mode)
                                 ;;   (powerline-raw which-func-format face2 'l))
                                 )))
                          (rhs (append
                                (list
                                 (when (boundp 'erc-modified-channels-object)
                                   (powerline-raw erc-modified-channels-object face2 'l))
                                 (powerline-raw "%4l" face1 'r)
                                 (powerline-raw ":" face1)
                                 (powerline-raw "%3c" face1 'r)
                                 (funcall separator-right face1 mode-line)
                                 (powerline-raw " ")
                                 (powerline-raw "%6p" nil 'r)
                                 (powerline-buffer-size nil 'r)
                                 (powerline-hud face2 face1))))
                          (center (append (list
                                           (powerline-process face2)
                                           (funcall separator-right face2 face1)))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face2 (/ (powerline-width center) 2))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))


;;(powerline-coldnew-theme)

(provide 'powerline-coldnew-theme)
;;; powerline-coldnew-theme.el ends here
