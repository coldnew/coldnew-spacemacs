;;; coldnew-moe-theme.el --- coldnew's emacs color-theme based on moe-theme.

;;; Commentary:
;;

(defconst coldnew-theme-colors
  '((night . (
              (background  "#0b0b0b")
              (foreground  "#dcdcdc")
              (cursor  "#C2C2C2")
              (current-line  "#2a2a2a")
              (selection  "#444444")
              (highlight  "#CAE682")
              (comment  "#5d9ae4")
              (red     "#E52210")
              (orange  "#e65c00")
              (yellow  "#f0c674")
              (green   "#95e454")
              (aqua    "#5d9ae4")
              (blue    "#4d85ff")
              (purple  "#AD7fA8")

              ))
    ))

(defun coldnew-theme--build-colors-alist (mode)
  (mapcar (lambda (x) (list (symbol-name (car x)) (cadr x)))
          (cdr (assoc mode coldnew-theme-colors))))

(defmacro coldnew-theme--with-colors (mode &rest body)
  "`let' bind all colors defined in `ample-zen-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cadr cons)))
                   (coldnew-theme--build-colors-alist 'night)))
     ,@body))

(defmacro coldnew-theme--face-specs ()
  (quote
   `(

     (default ((,class (:foreground ,foreground :background ,background))))

     )))

(defun coldnew-theme--theme-name (mode)
  (intern (format "coldnew-theme-%s" (symbol-name mode))))

(defmacro coldnew-theme--define-theme (mode)
  "Define a theme for the coldnew variant `MODE'."
  (let ((name (coldnew-theme--theme-name mode))
        (doc (format "coldnew's personal color theme (%s version)" mode)))
    `(progn
       (deftheme ,name ,doc)
       (put ',name 'theme-immediate t)
       (message (format "%s : %s" (symbol-name ',name) ,doc))
       (coldnew-theme--with-colors
         ',mode
         (apply 'custom-theme-set-faces ',name
                (coldnew-theme--face-specs))
         (custom-theme-set-variables
          ',name
          ;;          `(fci-rule-color ,current-line)
          ;;`(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,purple ,aqua ,background))
          ;; '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
          )
         )
       (provide-theme ',name))))

(defun coldnew-theme--load-theme (mode)
  (if (fboundp 'load-theme)
      (let ((name (coldnew-theme--theme-name mode)))
          (if (> emacs-major-version 23)
              (load-theme name t)
            (load-theme name)))
    ;; not support for older emace.
    (error "emacs should support load-theme to make coldnew-theme work.")))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory (or load-file-name (buffer-file-name))))))

;;;###autoload
(defun coldnew-theme-night ()
  (interactive)
  (coldnew-theme--load-theme 'night))

;; TODO:
;; ;;;###autoload
;; (defun coldnew-theme-day ()
;;   (interactive)
;;   (coldnew-theme--load-theme 'day))

(provide 'coldnew-theme)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; coldnew-theme.el ends here
