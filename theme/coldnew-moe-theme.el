;;; coldnew-moe-theme.el --- coldnew's emacs color-theme based on moe-theme.

;;; Commentary:
;;

(require 'moe-theme)

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory (or load-file-name (buffer-file-name))))))

;;;###autoload
(defun coldnew-moe-theme-night ()
  (interactive)

  ;; Load original moe-theme
  (if (not (null moe-theme-revert-theme))
      (load-theme 'moe-dark t))

  ;; Overwrite on my own
  (when window-system
    (progn
      (set-background-color "#202020")
      (set-foreground-color "#c6c6c6")))

  ;; mode-line
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 2 :color "#B184CB"))

  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 2 :color "#565968"))

  (set-face-attribute 'mode-line-highlight nil :box
                      nil :weight 'bold)

  )

(provide 'coldnew-moe-theme)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; coldnew-moe-theme.el ends here
