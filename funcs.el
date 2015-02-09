
(defun load-contrib-if-exist (pkg)
  (let* ((contrib-dir (concat emacs-dir "spacemacs/contrib/" pkg))
         (config-list
          (list "config.el" "packages.el" "extensions.el" "funcs.el" "keybindings.el")))

    (when (file-exists-p contrib-dir)
      (dolist (f config-list)
        (let ((conf (concat contrib-dir "/" f)))
          (when (file-exists-p conf) (load-file conf)))))))



(defun aa()
  (let ((files (directory-files (concat emacs-dir "private") nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))
    (dolist (f files)
      (if (file-exists-p (concat
                          (file-name-as-directory
                           (concat (file-name-as-directory spacemacs-dir) "contrib")) f))
          ;;          (load-contrib-if-exist f)
          )
      )))

(defadvice configuration-layer/declare-all-layers (after declare-layers activate)
  (mapc (lambda (layer) (push layer configuration-layer-layers))
        ;; FIXME: layers here should all be my private layer
        (configuration-layer//declare-layers '(coldnew-core git coldnew-eshell))))

;; (defun create-private-layer (name)
;;   "spacemacs wrapper for create coldnew's spacemacs private layer."
;;   (interactive "sConfiguration layer name: ")
;;   (let ((prefix "private/"))
;;     (flet ((configuration-layer//get-private-layer-dir
;;             (name)
;;             (concat (file-name-as-directory my/private-layer)
;;                     (replace-regexp-in-string prefix "" name) "/")))

;;       (configuration-layer/create-layer (concat prefix name)))
;;     ;; create empty config.el
;;     (write-region
;;      (format "(when (fboundp 'load-contrib-if-exist) (load-contrib-if-exist \"%s\"))" name)
;;      nil (concat (file-name-as-directory my/private-layer) name "/config.el")
;;      )))

(defun private/configuration-layer//discover-layers ()
  (let* ((private-dir (expand-file-name (concat emacs-dir "private")))
         (layers (directory-files private-dir nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
         (result (make-hash-table :size 128)))

    ;;    (puthash 'aa (expand-file-name user-emacs-directory) result)
    (mapc (lambda (dir)
            (puthash 'x dir result))
          layers)
    result
    ))

;;(insert (format "\n%s" (private/configuration-layer//discover-layers)))

(defun private/configuration-layer//declare-layer (layer)
  (let* ((plist ))
    )
  )



(defun ab (layers)
  (dolist (layer layers)
    (let* ((sym (car layer)))
      (insert (format "\n%s" (configuration-layer/declare-all-layers)))
      )
    )
  )
