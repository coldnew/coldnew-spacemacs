;;; coldnew-theme.el --- coldnew's emacs color-theme.
;;
;; Copyright (C) 2011-2015 Yen-Chin, Lee
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
;;
;; This theme is based on following theme file:
;; https://github.com/bbatsov/solarized-emacs
;; https://github.com/sjrmanning/noctilux-theme
;; https://github.com/cpaulik/emacs-material-theme
;;
;;
;; 256-color charts
;; http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html

;;; Code:

(require 'noflet)

;; NOTE:
;; This vairable is filled by other file.
(defvar coldnew-theme-colors
  '( )
  "This is a table of all the colors used by the coldnew color
   theme. Each column is a different set, one of which will be
   chosen based on term capabilities, etc.")

(defun coldnew-theme--build-colors-alist (mode)
  ;;  (mapcar (lambda (x) (list (symbol-name (car x)) (nth 1 (cdr x))))
  (noflet ((find-color (color)
                       ;; 1: window system , 2: terminal
                       (let* ((index (if window-system 1 2)))
                         (nth index color))))
    (mapcar (lambda (x) (list (symbol-name (car x)) (find-color x)))
            (cdr (assoc (cadr `,mode) coldnew-theme-colors)))))

(defmacro coldnew-theme--with-colors (mode &rest body)
  "`let' bind all colors defined in `coldnew-theme-colors' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cadr cons)))
                   (coldnew-theme--build-colors-alist mode)))
     ,@body))

(defmacro coldnew-theme--face-specs ()
  (quote
   `(;; Ensure sufficient contrast on low-color terminals.
     (default ((((class color) (min-colors 4096))
                (:foreground ,foreground :background ,background))
               (((class color) (min-colors 256))
                (:foreground ,foreground :background ,background))
               (,class
                (:foreground ,foreground :background ,background))))
     ;; Basic
     (bold ((t (:weight bold))))
     (bold-italic ((t (:weight bold :slant italic))))
     (cursor ((t (:foreground ,base0 :background ,white))))
     (error ((t (:foreground ,red :bold t))))
     (italic ((t (:slant italic))))
     (shadow ((t (:foreground ,comment))))
     (success ((t (:foreground ,green))))
     (underline ((t (:underline t))))
     (warning ((t (:foreground ,orange))))

     ;; font-lock
     (font-lock-builtin-face ((t (:foreground ,buildin))))
     (font-lock-constant-face ((t (:foreground ,magenta))))
     (font-lock-comment-face ((t (:foreground ,comment :slant italic))))
     (font-lock-comment-delimiter-face ((t (:foreground ,comment-delimiter :slant italic))))
     (font-lock-doc-face ((t (:foreground ,doc :slant italic))))
     (font-lock-function-name-face ((t (:foreground ,function-name))))
     (font-lock-keyword-face ((t (:foreground ,keyword))))
     (font-lock-type-face ((t (:foreground ,type))))
     (font-lock-variable-name-face  ((t (:foreground ,variable-name))))
     (font-lock-string-face ((t (:foreground ,string))))

     (font-lock-exit-face ((t (:foreground ,red))))
     (font-lock-negation-char-face ((t (:foreground ,red))))
     (font-lock-other-emphasized-face ((t (:foreground ,magenta :weight bold))))
     (font-lock-other-type-face ((t (:foreground ,blue :slant italic))))
     (font-lock-preprocessor-face ((t (:foreground ,orange))))
     (font-lock-reference-face ((t (:foreground ,cyan))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,orange))))
     (font-lock-special-keyword-face  ((t (:foreground ,red))))
     (font-lock-warning-face ((t (:foreground ,red :weight bold))))

     ;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground ,rainbow-1))))
     (rainbow-delimiters-depth-2-face ((t (:foreground ,rainbow-2))))
     (rainbow-delimiters-depth-3-face ((t (:foreground ,rainbow-3))))
     (rainbow-delimiters-depth-4-face ((t (:foreground ,rainbow-4))))
     (rainbow-delimiters-depth-5-face ((t (:foreground ,rainbow-5))))
     (rainbow-delimiters-depth-6-face ((t (:foreground ,rainbow-6))))
     (rainbow-delimiters-depth-7-face ((t (:foreground ,rainbow-7))))
     (rainbow-delimiters-depth-8-face ((t (:foreground ,rainbow-8))))
     (rainbow-delimiters-depth-9-face ((t (:foreground ,rainbow-9))))
     (rainbow-delimiters-unmatched-face ((t (:inherit error))))

     ;; link
     (link ((t (:foreground ,magenta :underline t))))

     ;; region
     (region ((,class (:background ,selection))))

     ;; minibuffer
     (minibuffer-prompt ((t (:foreground ,cyan :weight bold))))

     ;; modeline
     (mode-line ((t (:foreground ,base0 :background ,base02 :weight bold :box nil))))
     (mode-line-inactive ((t (:foreground ,base01 :background ,base03 :weight bold :box nil))))

     ;; comint
     (comint-highlight-prompt ((t (:foreground ,blue))))

     ;; compilation
     (compilation-info ((t (:foreground ,green :weight bold))))
     (compilation-warning ((t (:foreground ,orange :weight bold))))

     ;; smartparens-mode
     (sp-pair-overlay-face ((t (:forground ,foreground :background ,current-line))))

     ;; custom
     (custom-button
      ((t (:foreground ,base1 :background ,base02
                       :box (:line-width 2 :style released-button)))))
     (custom-button-mouse
      ((t (:foreground ,base1 :background ,base02 :inherit custom-button))))
     (custom-button-pressed
      ((t (:foreground ,base1 :background ,base02
                       :box (:line-width 2 :style pressed-button)
                       :inherit custom-button-mouse))))
     (custom-changed ((t (:foreground ,blue :background ,base3))))
     (custom-comment ((t (:foreground ,base1 :background ,base02))))
     (custom-comment-tag ((t (:foreground ,base1 :background ,base02))))
     (custom-documentation ((t (:inherit default))))
     (custom-group-tag ((t (:foreground ,base1))))
     (custom-group-tag-1 ((t (:foreground ,base1 :weight bold))))
     (custom-invalid ((t (:foreground ,red :background ,base3))))
     (custom-link ((t (:foreground ,magenta))))
     (custom-state ((t (:foreground ,green))))
     (custom-variable-tag ((t (:foreground ,base1))))

     ;; emacs-wiki
     (emacs-wiki-bad-link-face ((t (:foreground ,red))))
     (emacs-wiki-link-face ((t (:foreground ,blue :underline t))))
     (emacs-wiki-verbatim-face ((t (:foreground ,base00 :underline t))))

     ;; diff-mode
     (diff-added ((t (:foreground ,green :weight bold))))
     (diff-changed ((t (:foreground ,yellow :weight bold))))
     (diff-removed ((t (:foreground ,red :weight bold))))
     (diff-refine-change ((t (:foreground ,blue :background ,base03 :weight bold))))
     (diff-file-header ((t (:background ,base03))))
     (diff-header ((t (:foreground ,base1 :background ,base03))))

     ;; ido
     (ido-only-match ((t (:foreground ,green))))
     (ido-subdir ((t (:foreground ,blue))))
     (ido-first-match ((t (:foreground ,green :weight bold))))

     ;; helm
     (helm-M-x-key ((t (:foreground ,magenta))))
     (helm-buffer-directory ((t (:foreground, yellow :weight bold))))
     (helm-buffer-file ((t (:foreground ,base0))))
     (helm-buffer-not-saved ((t (:foreground ,red :slant italic ))))
     (helm-buffer-process ((t (:foregorund ,blue))))
     (helm-buffer-saved-out ((t (:foreground ,cyan))))
     (helm-buffer-size ((t (:foreground ,magenta))))
     (helm-candidate-number ((t (:foreground ,red :background ,base02 :weight bold))))
     (helm-header-line-left-margin ((t (:foreground ,red))))
     (helm-ff-directory ((t (:foreground ,blue :weight bold))))
     (helm-ff-dotted-directory ((t (:foreground ,red))))
     (helm-ff-executable ((t (:foreground ,base0 :weight bold))))
     (helm-ff-file ((t (:foreground ,base0))))
     (helm-ff-invalid-symlink ((t (:foreground ,red :background ,base02 :underline t))))
     (helm-ff-prefix ((t (:foreground ,yellow :weight bold))))
     (helm-ff-symlink ((t (:foreground ,blue :background ,base02 :weight bold))))
     (helm-match ((t (:foreground ,red :underline t))))
     (helm-selection ((t (:foreground ,green :background ,base01))))
     (helm-source-header ((t (:foreground ,cyan :weight bold ))))
     (helm-visible-mark ((t (:foreground ,blue :background ,base02))))

     ;; mmm-mode
     (mmm-code-submode-face ((,class (:background ,current-line))))
     (mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
     (mmm-output-submode-face ((,class (:background ,current-line))))

     ;; info
     (info-xref ((t (:foreground ,blue :underline t))))
     (info-xref-visited ((t (:foreground ,magenta :inherit info-xref))))

     ;; outline
     (outline-1 ((t (:foreground ,blue))))
     (outline-2 ((t (:foreground ,cyan))))
     (outline-3 ((t (:foreground ,yellow))))
     (outline-4 ((t (:foreground ,red))))
     (outline-5 ((t (:foreground ,base0))))
     (outline-6 ((t (:foreground ,base01))))
     (outline-7 ((t (:foreground ,orange))))
     (outline-8 ((t (:foreground ,magenta))))

     ;; hl-line
     (hl-line ((t (:background ,selection :underline nil))))

     ;; highlight
     (highlight ((,class (:foreground ,current-line :background ,highlight))))

     ;; fringe
     ;; (fringe ((t (:background ,current-line))))
     (fringe ((t (:foreground ,base01 :background ,base02))))

     ;; header
     (header-line ((t (:foreground ,base0 :background ,base02 :weight bold))))

     ;; linum
     (linum ((t (:foreground ,base01 :background ,base02))))

     ;; org
     (org-level-1 ((t (:inherit outline-1 :bold t))))
     (org-level-2 ((t (:inherit outline-2))))
     (org-level-3 ((t (:inherit outline-3))))
     (org-level-4 ((t (:inherit outline-4))))
     (org-level-5 ((t (:inherit outline-5))))
     (org-level-6 ((t (:inherit outline-6))))
     (org-level-7 ((t (:inherit outline-7))))
     (org-level-8 ((t (:inherit outline-8))))
     (org-level-9 ((t (:inherit outline-9))))
     (org-block ((,class (:foreground ,green :background ,far-background))))
     (org-block-background ((,t (:background ,far-background))))

     (org-hide ((t (:foreground ,base03))))
     (org-link ((t (:inherit link))))
     ;; (org-todo ((t (:foreground ,base03 :background ,red :weight bold))))
     (org-todo ((t (:foreground ,red :bold t))))
     (org-done ((t (:foreground ,green :weight bold))))
     (org-todo-kwd-face ((t (:foreground ,red :background ,base03))))
     (org-done-kwd-face ((t (:foreground ,green :background ,base03))))
     (org-project-kwd-face ((t (:foreground ,magenta :background ,base03))))
     (org-waiting-kwd-face ((t (:foreground ,orange :background ,base03))))
     (org-someday-kwd-face ((t (:foreground ,blue :background ,base03))))
     (org-started-kwd-face ((t (:foreground ,yellow :background ,base03))))
     (org-cancelled-kwd-face ((t (:foreground ,green :background ,base03))))
     (org-delegated-kwd-face ((t (:foreground ,cyan  :background ,base03))))

     ;; table
     (table-cell ((t (:foreground ,base0 :background ,base03))))

     ;; speedbar
     (speedbar-button-face ((t (:foreground ,base1))))
     (speedbar-directory-face ((t (:foreground ,orange))))
     (speedbar-file-face ((t (:foreground ,green))))
     (speedbar-highlight-face ((t (:background ,base02))))
     (speedbar-selected-face ((t (:foreground ,yellow :underline t))))
     (speedbar-separator-face ((t (:inherit default))))
     (speedbar-tag-face ((t (:foreground ,blue))))

     ;; show-paren
     (show-paren-match ((t (:foreground ,cyan :background ,base02 :weight bold :underline t))))
     (show-paren-mismatch ((t (:inherit error))))

     ;; bm visual bookmarks
     (bm-fringe-face ((t (:foreground ,base03 :background ,orange))))
     (bm-fringe-persistent-face ((t (:foreground ,base03 :background ,blue))))

     ;; Flymake
     (flymake-errline ((t (:underline (:color ,red :style wave) :inherit default)))) ; ErrorMsg
     (flymake-warnline ((t (:underline (:color ,orange :style wave) :inherit default)))) ; WarningMsg

     ;; column-marker
     (column-marker-1 ((t (:background ,base01))))
     (column-marker-2 ((t (:background ,cyan))))
     (column-marker-3 ((t (:background ,magenta))))

     ;; jabber
     (jabber-activity-face ((t (:foreground ,red :weight bold))))
     (jabber-activity-personal-face ((t (:foreground ,blue :weight bold))))
     (jabber-chat-error ((t (:foreground ,red :weight bold))))
     (jabber-chat-prompt-foreign ((t (:foreground ,red :weight bold))))
     (jabber-chat-prompt-local ((t (:foreground ,blue :weight bold))))
     (jabber-chat-prompt-system ((t (:foreground ,green :weight bold))))
     (jabber-chat-text-foreign ((t (:foreground ,base1))))
     (jabber-chat-text-local ((t (:foreground ,base0))))
     (jabber-chat-rare-time-face ((t (:foreground ,green :underline t))))
     (jabber-roster-user-away ((t (:foreground ,green :slant italic))))
     (jabber-roster-user-chatty ((t (:foreground ,orange :weight bold))))
     (jabber-roster-user-dnd ((t (:foreground ,red :slant italic))))
     (jabber-roster-user-error ((t (:foregournd ,red :weight light :slant italic))))
     (jabber-roster-user-offline ((t (:foreground ,base01))))
     (jabber-roster-user-online ((t (:foreground ,blue :weight bold ))))
     (jabber-roster-user-xa ((t (:foreground ,magenta :slant italic))))

     ;; git-gutter
     (git-gutter:modified ((t (:foreground ,magenta))))
     (git-gutter:added ((t (:foreground ,green))))
     (git-gutter:deleted ((t (:foreground ,red))))

     ;; gnus
     (gnus-cite-1 ((t (:foreground ,blue))))
     (gnus-cite-2 ((t (:foreground ,cyan))))
     (gnus-cite-3 ((t (:foreground ,yellow))))
     (gnus-cite-4 ((t (:foreground ,red))))
     (gnus-cite-5 ((t (:foreground ,orange))))
     (gnus-cite-6 ((t (:foreground ,magenta))))
     (gnus-cite-7 ((t (:foreground ,green))))
     (gnus-cite-8 ((t (:foreground ,magenta))))
     (gnus-cite-9 ((t (:foreground ,base00))))
     (gnus-cite-10 ((t (:foreground ,base01))))
     (gnus-cite-11 ((t (:foreground ,base02))))
     (gnus-group-mail-1 ((t (:foreground ,base3 :weight bold))))
     (gnus-group-mail-1-empty ((t (:foreground ,base3))))
     (gnus-group-mail-2 ((t (:foreground ,base2 :weight bold))))
     (gnus-group-mail-2-empty ((t (:foreground ,base2))))
     (gnus-group-mail-3 ((t (:foreground ,magenta :weight bold))))
     (gnus-group-mail-3-empty ((t (:foreground ,magenta))))
     (gnus-group-mail-low ((t (:foreground ,base00 :weight bold))))
     (gnus-group-mail-low-empty ((t (:foreground ,base00))))
     (gnus-group-news-1 ((t (:foreground ,base1 :weight bold))))
     (gnus-group-news-1-empty ((t (:foreground ,base1))))
     (gnus-group-news-2 ((t (:foreground ,blue :weight bold))))
     (gnus-group-news-2-empty ((t (:foreground ,blue))))
     (gnus-group-news-low ((t (:foreground ,magenta :weight bold))))
     (gnus-group-news-low-empty ((t (:foreground ,magenta))))
     (gnus-emphasis-highlight-words  ((t (:foreground ,yellow))))
     (gnus-header-content ((t (:foreground ,base01)))) ; hdrdefault
     (gnus-header-from ((t (:foreground ,base00)))) ; header ^From
     (gnus-header-name ((t (:foregrund ,base01)))) ; hdrdefault
     (gnus-header-newsgroups ((t (:foreground ,base02))))
     (gnus-header-subject ((t (:foreground ,blue))))
     (gnus-server-agent ((t (:foreground ,base3 :weight bold))))
     (gnus-server-closed ((t (:foreground ,base1 :slant italic))))
     (gnus-server-denied ((t (:foreground ,base2 :weight bold))))

     (gnus-server-offline ((t (:foreground ,green :weight bold))))
     (gnus-server-opened ((t (:foreground ,cyan :weight bold))))
     (gnus-signature ((t (:foreground ,base01))))
     (gnus-splash ((t (:foreground ,base2))))
     (gnus-summary-cancelled  ((t (:foreground ,red))))
     (gnus-summary-high-ancient ((t (:weight bold :inherit gnus-summary-normal-ancient))))
     (gnus-summary-high-read ((t (:weight bold :inherit gnus-summary-normal-read))))
     (gnus-summary-high-ticked ((t (:weight bold :inherit gnus-summary-normal-ticked))))
     (gnus-summary-high-undownloaded ((t (:weight bold :inherit gnus-summary-normal-undownloaded))))
     (gnus-summary-high-unread ((t (:weight bold :inherit gnus-summary-normal-unread))))
     (gnus-summary-low-ancient ((t (:slant italic :inherit gnus-summary-normal-ancient))))
     (gnus-summary-low-read ((t (:slant italic :inherit gnus-summary-normal-ancient))))
     (gnus-summary-low-unread ((t (:slant italic :inherit gnus-summary-normal-unread))))
     (gnus-summary-low-ticked ((t (:slant italic :inherit gnus-summary-normal-ancient))))
     (gnus-summary-low-undownloaded ((t (:slant italic :inherit gnus-summary-normal-ancient))))
     (gnus-summary-normal-ancient  ((t (:foreground ,blue))))
     (gnus-summary-normal-read  ((t (:foreground ,base01))))
     (gnus-summary-normal-ticked  ((t (:foreground ,red))))
     (gnus-summary-normal-undownloaded ((t (:foreground ,base2))))
     (gnus-summary-normal-unread ((t (:foreground ,blue))))
     (gnus-summary-selected ((t (:foreground ,base03 :background ,yellow))))

     ;; magit
     (magit-bisect-bad ((t (:foreground ,red))))
     (magit-bisect-good ((t (:foreground ,green))))
     (magit-biset-skip ((t (:foreground ,blue))))
     (magit-branch-current ((t (:foreground ,green))))
     (magit-branch-local ((t (:foreground ,cyan))))
     (magit-branch-remote ((t (:foreground ,yellow))))
     (magit-cherry-equivalent ((t (:foreground ,base0))))
     (magit-cherry-unmatched ((t (:foreground ,orange))))
     (magit-dimmed ((t (:foregrund ,base01 :slant italic))))
     (magit-hash ((t (:foreground ,base2))))
     (magit-header-line ((t (:foreground ,blue :weight bold))))
     (magit-log-author ((t (:foregrund ,cyan))))
     (magit-log-date ((t (:foreground ,blue))))
     (magit-log-graph ((t (:foreground ,base01))))
     (magit-process-ng ((t (:foreground ,red :weight bold))))
     (magit-process-ok ((t (:foreground ,green :weight bold))))
     (magit-refname ((t (:foreground ,magenta))))
     (magit-section-heading ((t (:foreground ,cyan :weight bold))))
     (magit-siganture-unmatched ((t (:foreground ,magenta))))
     (magit-siganture-untrusted ((t (:foreground ,magenta))))
     (magit-signature-bad ((t (:foreground ,red))))
     (magit-signature-good ((t (:foreground ,green))))
     (magit-tag ((t (:foreground ,orange))))

     ;; message
     (message-mml ((t (:foreground ,blue))))
     (message-cited-text ((t (:foreground ,base2))))
     (message-separator ((t (:foreground ,base3))))
     (message-header-xheader ((t (:foreground ,magenta))))
     (message-header-name ((t (:foreground ,cyan))))
     (message-header-other ((t (:foreground ,red))))
     (message-header-newsgroups ((t (:foreground ,yellow :weight bold))))
     (message-header-subject ((t (:foreground ,base00))))
     (message-header-cc ((t (:foreground ,green :weight bold))))
     (message-header-to ((t (:foreground ,base1 :weight bold))))

     ;; parenface
     (paren-face ((t (:foreground ,base01))))

     ;; slime
     (slime-error-face ((t (:foreground ,red :inverse-video t)))) ; ErrorMsg
     (slime-note-face ((t (:foreground ,yellow))))
     (slime-repl-inputted-output-face ((t (:foreground ,red))))
     (slime-repl-output-mouseover-face ((t (:box (:color ,base3)))))
     (slime-style-warning-face ((t (:foreground ,orange :weight bold))))
     (slime-warning-face ((t (:foreground ,red :weight bold)))) ; WarningMsg

     ;; whitespace
     (whitespace-empty ((t (:foreground ,red))))
     (whitespace-hspace ((t (:foreground ,orange))))
     (whitespace-indentation ((t (:foreground ,base02))))
     (whitespace-space ((t (:foreground ,base02))))
     (whitespace-space-after-tab ((t (:foreground ,cyan))))
     (whitespace-space-before-tab ((t (:foreground ,red :weight bold))))
     (whitespace-tab ((t (:foreground ,base02))))
     (whitespace-trailing ((t (:foreground ,red :background ,base02 :weight bold))))
     (whitespace-highlight-face ((t (:foreground ,@red :background ,blue))))
     (whitespace-line ((t (:foreground ,magenta :background ,base03))))

     ;; rcirc
     (rcirc-my-nick ((t (:foreground ,blue))))
     (rcirc-nick-in-message ((t (:foreground ,orange))))
     (rcirc-other-nick ((t (:foreground ,green))))
     (rcirc-prompt ((t (:foreground ,yellow))))
     (rcirc-bright-nick ((t (:foreground ,magenta))))
     (rcirc-server ((t (:foreground ,base1))))
     (rcirc-timestamp ((t (:foreground ,base01))))

     ;; erc
     (erc-input-face ((t (:foreground ,base01))))
     (erc-keyword-face ((t (:foreground ,yellow :weight bold))))
     (erc-my-nick-face ((t (:foreground ,blue))))
     (erc-nick-defaunoctilux-face ((t (:foreground ,cyan))))
     (erc-notice-face ((t (:foreground ,blue))))
     (erc-timestamp-face ((t (:foreground ,base01))))

     ;; evil
     (evil-ex-lazy-highlight ((t :inherit lazy-highlight)))
     (evil-ex-search ((t :inherit isearch)))
     (evil-ex-substitute-matches ((t (:foreground ,orange))))
     (evil-ex-substitute-replacement ((t (:foreground ,red :underline t))))

     ;;font-latex
     (font-latex-warning-face ((t (:foreground ,red))))
     (font-latex-sectioning-5-face ((t (:foreground ,magenta))))

     ;;flyspell
     (flyspell-incorrect ((t (:foreground ,red))))
     (flyspell-duplicate ((t (:foreground ,yellow))))

     ;; company-mode
     (company-tooltip ((t (:foreground ,base0 :background ,base02))))
     (company-tooltip-selection ((t (:foreground ,base0 :background ,base01))))
     (company-tooltip-mouse ((t (:background ,base02))))
     (company-tooltip-common ((t (:foreground ,magenta :background ,base02))))
     (company-tooltip-common-selection ((t (:foreground ,magenta :background ,base01))))
     (company-tooltip-annotation ((t (:foreground ,base0 :background ,base02))))
     (company-scrollbar-fg ((t (:background ,base01))))
     (company-scrollbar-bg ((t (:background ,base3))))
     (company-preview ((t (:foreground ,base0 :background ,base01))))
     (company-preview-common ((t (:foreground ,base0 :background ,base01))))
     (company-preview-search ((t (:foreground ,magenta :background ,base01))))
     (company-echo ((t nil)))
     (company-echo-common ((t (:foreground ,magenta))))

     ;; ;; ansi-term
     (term-color-black ((t (:foreground ,black))))
     (term-color-red ((t (:foreground ,red))))
     (term-color-green ((t (:foreground ,green))))
     (term-color-yellow ((t (:foreground ,yellow))))
     (term-color-blue ((t (:foreground ,blue))))
     (term-color-magenta ((t (:foreground ,magenta))))
     (term-color-cyan ((t (:foreground ,cyan))))
     (term-color-white ((t (:foreground ,white))))


     ;;;; Following need to re-check
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (escape-glyph-face ((,class (:foreground ,red))))

     (isearch ((t (:foreground ,orange :backbround ,base03 :weight normal :slant normal  :underline nil :inverse-video t)))) ; IncSearch
     (isearch-fail ((t (:foreground ,orange :background ,base03 :weight normal :slant normal  :underline nil :inverse-video t)))) ; IncSearch
     (lazy-highlight ((t (:foreground ,yellow :background ,base03 :weight normal :slant normal :underline nil :inverse-video t)))) ; Search


     (menu ((t (:foreground ,base0 :background ,base02))))

;;     (region ((t (:foreground ,base01 :background ,base03 :weight bold :slant normal :underline nil  :inverse-video t)))) ; Visual
     (secondary-selection ((t (:background ,base02))))
;;     (shadow ((t (:foreground ,base01))))
     (trailing-whitespace ((t (:foreground ,red :weight normal :slant normal  :underline nil :inverse-video t))))
     (vertical-border ((t (:foreground ,base0))))

     ;; (outline-4 ((,class (:slant normal :foreground ,comment))))

     ;; ;; Font locks
     ;; (font-lock-builtin-face ((t (:foreground "#4BC98A"))))
     ;; (font-lock-comment-face ((t (:foreground ,comment :italic t))))
     ;; (font-lock-constant-face ((t (:foreground "#E53F3F" :bold t))))
     ;; (font-lock-function-name-face ((t (:foreground "#AD7FA8" :italic t :bold t))))
     ;; (font-lock-keyword-face ((t (:foreground "#FFC125"))))
     ;; (font-lock-string-face ((t (:foreground "#95E454" :italic t))))
     ;; (font-lock-type-face ((t (:foreground "#CAE682"))))
     ;; (font-lock-variable-name-face ((t (:foreground "#4BC98A"))))
     ;; (font-lock-warning-face ((t (:foreground "#E91303" :bold t))))
     ;; (font-lock-doc-face ((t (:foreground "#40AAFA"))))

     ;; ;; Auto Complete
     ;; (ac-candidate-face ((t (:background ,selection :foreground ,foreground))))
     ;; (ac-selection-face ((t (:background ,highlight :foreground ,background))))

     ;; ;; Elscreen
     ;; (elscreen-tab-background-face ((t (:background ,background))))
     ;; (elscreen-tab-control-face ((t (:foreground ,foreground :background "black"
     ;;                                             :weight extra-bold))))
     ;; (elscreen-tab-current-screen-face ((t (:background "#250628" :foreground "Gray90"
     ;;                                                    :bold t))))
     ;; (elscreen-tab-other-screen-face ((t (:background "#1D1D1F" :foreground "Gray85"
     ;;                                                  :bold t))))
     ;; ;; Evil
     ;; (evil-state-normal-face ((t :foreground ,purple :bold t)))
     ;; (evil-state-insert-face ((t :foreground ,red :bold t)))
     ;; (evil-state-visual-face ((t :foreground ,blue :bold t)))
     ;; (evil-state-emacs-face ((t :foreground ,green :bold t)))

     ;; ;; Flymake
     ;; (flymake-warnline ((,class (:underline ,orange :background ,background))))
     ;; (flymake-errline ((,class (:underline ,red :background ,background))))

     ;; ;; Clojure errors
     ;; (clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
     ;; (clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
     ;; (clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))

     ;; ;; For Brian Carper's extended clojure syntax table
     ;; (clojure-keyword ((,class (:foreground ,yellow))))
     ;; (clojure-parens ((,class (:foreground ,foreground))))
     ;; (clojure-braces ((,class (:foreground ,green))))
     ;; (clojure-brackets ((,class (:foreground ,yellow))))
     ;; (clojure-double-quote ((,class (:foreground ,aqua :background nil))))
     ;; (clojure-special ((,class (:foreground ,blue))))
     ;; (clojure-java-call ((,class (:foreground ,purple))))

     ;; ;; Search
     ;; (match ((,class (:foreground ,blue :background ,background :inverse-video t))))
     ;; (isearch ((,class (:foreground ,yellow :background ,background :inverse-video t))))
     ;; (isearch-lazy-highlight-face ((,class (:foreground ,aqua :background ,background :inverse-video t))))
     ;; (isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; ;; IDO
     ;; (ido-first-match ((,class (:foreground ,yellow :weight bold))))
     ;; (ido-only-match ((,class (:foreground ,orange :weight bold))))
     ;; (ido-virtual ((,class (:foreground ,comment))))
     ;; (ido-incomplete-regexp ((,class (:foreground ,red :bold t))))
     ;; (ido-subdir ((,class (:foreground ,aqua :bold t))))
     ;; (ido-virtual ((,class (:foreground ,purple))))

     ;; ;; which-function
     ;; (which-func ((,class (:foreground ,blue :background nil :weight bold))))

     ;; ;; Emacs interface

     ;; (linum ((,class (:foreground ,cursor :background ,background))))

     ;; ;;     (border ((,class (:background ,current-line))))
     ;; ;;     (border-glyph ((,class (nil))))
     ;; (highlight ((,class (:foreground ,current-line :background ,green))))
     ;; (link ((,class (:foreground ,blue))))
     ;; (link-visited ((,class (:foreground ,purple))))
     ;; (gui-element ((,class (:background ,current-line :foreground ,foreground))))

     ;; ;; mode-line
     ;; (mode-line ((,class (:background ,background :foreground "#b1c3d4"
     ;;                                  :box (:line-width 2 :color "#B184CB")))))
     ;; (mode-line-inactive ((,class (:background ,current-line :foreground "#7b8793"
     ;;                                           :box (:line-width 2 :color "#565968")))))
     ;; (mode-line-buffer-id ((,class (:foreground ,foreground :background nil))))
     ;; (mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
     ;; (mode-line-highlight ((,class (:foreground ,purple :box nil :weight bold))))

     ;; (minibuffer-prompt ((,class (:foreground ,red :bold t))))
     ;; (region ((,class (:background ,selection))))
     ;; (secondary-selection ((,class (:background ,current-line))))

     ;; ;; (header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))

     ;; (trailing-whitespace ((,class (:background ,red :foreground ,yellow))))
     ;; (whitespace-empty ((,class (:foreground ,red :background ,yellow))))
     ;; (whitespace-hspace ((,class (:background ,selection :foreground ,comment))))
     ;; (whitespace-indentation ((,class (:background ,yellow :foreground ,red))))
     ;; (whitespace-line ((,class (:background ,current-line :foreground ,purple))))
     ;; (whitespace-newline ((,class (:foreground ,comment))))
     ;; (whitespace-space ((,class (:background ,current-line :foreground ,comment))))
     ;; (whitespace-space-after-tab ((,class (:background ,yellow :foreground ,red))))
     ;; (whitespace-space-before-tab ((,class (:background ,orange :foreground ,red))))
     ;; (whitespace-tab ((,class (:background ,selection :foreground ,comment))))
     ;; (whitespace-trailing ((,class (:background ,red :foreground ,yellow))))

     ;; ;; Parenthesis matching (built-in)
     ;; (show-paren-match ((,class (:background ,blue :foreground ,current-line))))
     ;; (show-paren-mismatch ((,class (:background ,orange :foreground ,current-line))))

     ;; ;; Parenthesis matching (mic-paren)
     ;; (paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
     ;; (paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
     ;; (paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

     ;; ;; Parenthesis dimming (parenface)
     ;; (paren-face ((,class (:foreground ,comment :background nil))))

     ;; (sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
     ;; (sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
     ;; (slime-highlight-edits-face ((,class (:weight bold))))
     ;; (slime-repl-input-face ((,class (:weight normal :underline nil))))
     ;; (slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
     ;; (slime-repl-result-face ((,class (:foreground ,green))))
     ;; (slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

     ;; (csv-separator-face ((,class (:foreground ,orange))))

     ;; (diff-added ((,class (:foreground ,green))))
     ;; (diff-changed ((,class (:foreground ,yellow))))
     ;; (diff-removed ((,class (:foreground ,red))))
     ;; (diff-header ((,class (:background ,current-line))))
     ;; (diff-file-header ((,class (:background ,selection))))
     ;; (diff-hunk-header ((,class (:foreground ,yellow :italic t))))
     ;; (diff-context ((,class (:foreground ,foreground))))

     ;; (ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
     ;; (ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
     ;; (ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
     ;; (ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

     ;; (eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

     ;; ;; undo-tree
     ;; (undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
     ;; (undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
     ;; (undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
     ;; (undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; ;; auctex
     ;; (font-latex-bold-face ((,class (:foreground ,green))))
     ;; (font-latex-doctex-documentation-face ((,class (:background ,current-line))))
     ;; (font-latex-italic-face ((,class (:foreground ,green))))
     ;; (font-latex-math-face ((,class (:foreground ,orange))))
     ;; (font-latex-sectioning-0-face ((,class (:foreground ,yellow))))
     ;; (font-latex-sectioning-1-face ((,class (:foreground ,yellow))))
     ;; (font-latex-sectioning-2-face ((,class (:foreground ,yellow))))
     ;; (font-latex-sectioning-3-face ((,class (:foreground ,yellow))))
     ;; (font-latex-sectioning-4-face ((,class (:foreground ,yellow))))
     ;; (font-latex-sectioning-5-face ((,class (:foreground ,yellow))))
     ;; (font-latex-sedate-face ((,class (:foreground ,aqua))))
     ;; (font-latex-string-face ((,class (:foreground ,yellow))))
     ;; (font-latex-verbatim-face ((,class (:foreground ,orange))))
     ;; (font-latex-warning-face ((,class (:foreground ,red))))

     ;; ;; dired+
     ;; (diredp-compressed-file-suffix ((,class (:foreground ,blue))))
     ;; (diredp-dir-heading ((,class (:foreground nil :background nil :inherit heading))))
     ;; (diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
     ;; (diredp-exec-priv ((,class (:foreground ,blue :background nil))))
     ;; (diredp-executable-tag ((,class (:foreground ,red :background nil))))
     ;; (diredp-file-name ((,class (:foreground ,yellow))))
     ;; (diredp-file-suffix ((,class (:foreground ,green))))
     ;; (diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
     ;; (diredp-ignored-file-name ((,class (:foreground ,comment))))
     ;; (diredp-link-priv ((,class (:background nil :foreground ,purple))))
     ;; (diredp-mode-line-flagged ((,class (:foreground ,red))))
     ;; (diredp-mode-line-marked ((,class (:foreground ,green))))
     ;; (diredp-no-priv ((,class (:background nil))))
     ;; (diredp-number ((,class (:foreground ,yellow))))
     ;; (diredp-other-priv ((,class (:background nil :foreground ,purple))))
     ;; (diredp-rare-priv ((,class (:foreground ,red :background nil))))
     ;; (diredp-read-priv ((,class (:foreground ,green :background nil))))
     ;; (diredp-symlink ((,class (:foreground ,purple))))
     ;; (diredp-write-priv ((,class (:foreground ,yellow :background nil))))

     ;; ;; Magit
     ;; (magit-branch ((,class (:foreground ,green))))
     ;; (magit-header ((,class (:inherit nil :weight bold))))
     ;; (magit-item-highlight ((,class (:background ,background))))
     ;; (magit-log-graph ((,class (:foreground ,comment))))
     ;; (magit-log-sha1 ((,class (:foreground ,orange))))
     ;; (magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
     ;; (magit-log-head-label-bisect-good ((,class (:foreground ,green))))
     ;; (magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
     ;; (magit-log-head-label-local ((,class (:foreground ,blue))))
     ;; (magit-log-head-label-remote ((,class (:foreground ,green))))
     ;; (magit-log-head-label-tags ((,class (:foreground ,aqua :box nil :weight bold))))
     ;; (magit-section-title ((,class (:inherit diff-hunk-header))))

     ;; ;; git-gutter
     ;; (git-gutter-fr:modified ((,class (:foreground ,yellow))))
     ;; (git-gutter-fr:added ((,class (:inherit diff-added))))
     ;; (git-gutter-fr:deleted ((,class (:inherit diff-removed))))

     ;;     (link ((t (:foreground "dodger blue" :underline t))))
     ;; (widget-button ((,class (:underline t))))
     ;; (widget-field ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))))

     ;; ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     ;; (compilation-column-number ((,class (:foreground ,yellow))))
     ;; (compilation-line-number ((,class (:foreground ,yellow))))
     ;; (compilation-message-face ((,class (:foreground ,blue))))
     ;; (compilation-mode-line-exit ((,class (:foreground ,green))))
     ;; (compilation-mode-line-fail ((,class (:foreground ,red))))
     ;; (compilation-mode-line-run ((,class (:foreground ,blue))))

     ;; ;; Grep
     ;; (grep-context-face ((,class (:foreground ,comment))))
     ;; (grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     ;; (grep-hit-face ((,class (:foreground ,blue))))
     ;; (grep-match-face ((,class (:foreground nil :background nil :inherit match))))

     ;; (regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

     ;; ;; mark-multiple
     ;; (mm/master-face ((,class (:inherit region :foreground nil :background nil))))
     ;; (mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

     ;; (org-agenda-structure ((,class (:foreground ,purple))))
     ;; (org-agenda-date ((,class (:foreground ,blue :underline nil))))
     ;; (org-agenda-done ((,class (:foreground ,green))))
     ;; (org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
     ;; (org-block ((,class (:foreground ,yellow))))
     ;; (org-code ((,class (:foreground ,yellow))))
     ;; (org-column ((,class (:background ,current-line))))
     ;; (org-column-title ((,class (:inherit org-column :weight bold :underline t))))
     ;; (org-date ((,class (:foreground ,purple :underline t :bold t))))
     ;; (org-agenda-date-weekend ((t (:bold t :foreground ,orange :weight bold))))
     ;; (org-document-info ((,class (:foreground ,aqua))))
     ;; (org-document-info-keyword ((,class (:foreground ,green))))
     ;; (org-document-title ((,class (:weight bold :foreground ,orange :height 1.44))))
     ;; (org-ellipsis ((,class (:foreground ,comment))))
     ;; (org-footnote ((,class (:foreground ,aqua))))
     ;; (org-formula ((,class (:foreground ,red))))
     ;; ;;(org-hide ((,class (:foreground ,current-line))))
     ;; (org-hide ((t (:foreground "#0B0B0E"))))
     ;; (org-scheduled ((,class (:foreground ,green))))
     ;; (org-scheduled-previously ((,class (:foreground ,orange))))
     ;; (org-scheduled-today ((,class (:foreground ,green))))
     ;; (org-special-keyword ((,class (:foreground ,orange))))
     ;; ;;(org-table ((,class (:foreground ,purple))))
     ;; (org-todo ((,class (:foreground ,red :bold t))))
     ;; (org-done ((t (:foreground "#4BC98A" :bold t))))

     ;; (org-upcoming-deadline ((,class (:foreground ,orange))))
     ;; (org-warning ((,class (:weight bold :foreground ,red))))

     ;; (markdown-url-face ((,class (:inherit link))))
     ;; (markdown-link-face ((,class (:foreground ,blue :underline t))))

     ;; (hl-sexp-face ((,class (:background ,current-line))))
     ;; (highlight-80+ ((,class (:background ,current-line))))

     ;; ;; Python-specific overrides
     ;; (py-builtins-face ((,class (:foreground ,orange :weight normal))))

     ;; ;; js2-mode
     ;; (js2-warning-face ((,class (:underline ,orange))))
     ;; (js2-error-face ((,class (:foreground nil :underline ,red))))
     ;; (js2-external-variable-face ((,class (:foreground ,purple))))
     ;; (js2-function-param-face ((,class (:foreground ,blue))))
     ;; (js2-instance-member-face ((,class (:foreground ,blue))))
     ;; (js2-private-function-call-face ((,class (:foreground ,red))))

     ;; ;; js3-mode
     ;; (js3-warning-face ((,class (:underline ,orange))))
     ;; (js3-error-face ((,class (:foreground nil :underline ,red))))
     ;; (js3-external-variable-face ((,class (:foreground ,purple))))
     ;; (js3-function-param-face ((,class (:foreground ,blue))))
     ;; (js3-jsdoc-tag-face ((,class (:foreground ,orange))))
     ;; (js3-jsdoc-type-face ((,class (:foreground ,aqua))))
     ;; (js3-jsdoc-value-face ((,class (:foreground ,yellow))))
     ;; (js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
     ;; (js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
     ;; (js3-instance-member-face ((,class (:foreground ,blue))))
     ;; (js3-private-function-call-face ((,class (:foreground ,red))))

     ;; ;; nxml
     ;; (nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
     ;; (nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
     ;; (nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
     ;; (nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
     ;; (nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
     ;; (rng-error-face ((,class (:underline ,red))))

     ;; ;; RHTML
     ;; (erb-delim-face ((,class (:background ,current-line))))
     ;; (erb-exec-face ((,class (:background ,current-line :weight bold))))
     ;; (erb-exec-delim-face ((,class (:background ,current-line))))
     ;; (erb-out-face ((,class (:background ,current-line :weight bold))))
     ;; (erb-out-delim-face ((,class (:background ,current-line))))
     ;; (erb-comment-face ((,class (:background ,current-line :weight bold :slant italic))))
     ;; (erb-comment-delim-face ((,class (:background ,current-line))))

     ;; ;; Message-mode
     ;; (message-header-other ((,class (:foreground nil :background nil :weight normal))))
     ;; (message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
     ;; (message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
     ;; (message-header-cc ((,class (:inherit message-header-to :foreground nil))))
     ;; (message-header-name ((,class (:foreground ,blue :background nil))))
     ;; (message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))))
     ;; (message-separator ((,class (:foreground ,purple))))

     ;; ;; Jabber
     ;; (jabber-chat-prompt-local ((,class (:foreground ,yellow))))
     ;; (jabber-chat-prompt-foreign ((,class (:foreground ,orange))))
     ;; (jabber-chat-prompt-system ((,class (:foreground ,yellow :weight bold))))
     ;; (jabber-chat-text-local ((,class (:foreground ,yellow))))
     ;; (jabber-chat-text-foreign ((,class (:foreground ,orange))))
     ;; (jabber-chat-text-error ((,class (:foreground ,red))))

     ;; (jabber-roster-user-online ((,class (:foreground ,green))))
     ;; (jabber-roster-user-xa ((,class :foreground ,comment)))
     ;; (jabber-roster-user-dnd ((,class :foreground ,yellow)))
     ;; (jabber-roster-user-away ((,class (:foreground ,orange))))
     ;; (jabber-roster-user-chatty ((,class (:foreground ,purple))))
     ;; (jabber-roster-user-error ((,class (:foreground ,red))))
     ;; (jabber-roster-user-offline ((,class (:foreground ,comment))))

     ;; (jabber-rare-time-face ((,class (:foreground ,comment))))
     ;; (jabber-activity-face ((,class (:foreground ,purple))))
     ;; (jabber-activity-personal-face ((,class (:foreground ,aqua))))

     ;; ;; Gnus
     ;; (gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
     ;; (gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
     ;; (gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
     ;; (gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
     ;; (gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
     ;; (gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
     ;; (gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
     ;; (gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
     ;; ;; there are several more -cite- faces...
     ;; (gnus-header-content ((,class (:inherit message-header-other))))
     ;; (gnus-header-subject ((,class (:inherit message-header-subject))))
     ;; (gnus-header-from ((,class (:inherit message-header-other-face :weight bold :foreground ,orange))))

     ;; (gnus-header-name ((,class (:inherit message-header-name))))
     ;; (gnus-button ((,class (:inherit link :foreground nil))))
     ;; (gnus-signature ((,class (:inherit font-lock-comment-face))))

     ;; (gnus-summary-normal-unread ((,class (:foreground ,blue :weight normal))))
     ;; (gnus-summary-normal-read ((,class (:foreground ,foreground :weight normal))))
     ;; (gnus-summary-normal-ancient ((,class (:foreground ,aqua :weight normal))))
     ;; (gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))))
     ;; (gnus-summary-low-unread ((,class (:foreground ,comment :weight normal))))
     ;; (gnus-summary-low-read ((,class (:foreground ,comment :weight normal))))
     ;; (gnus-summary-low-ancient ((,class (:foreground ,comment :weight normal))))
     ;; (gnus-summary-high-unread ((,class (:foreground ,yellow :weight normal))))
     ;; (gnus-summary-high-read ((,class (:foreground ,green :weight normal))))
     ;; (gnus-summary-high-ancient ((,class (:foreground ,green :weight normal))))
     ;; (gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))))
     ;; (gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))))

     ;; (gnus-group-mail-low ((,class (:foreground ,comment))))
     ;; (gnus-group-mail-low-empty ((,class (:foreground ,comment))))
     ;; (gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
     ;; (gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
     ;; (gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
     ;; (gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
     ;; (gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
     ;; (gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
     ;; (gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,comment))))
     ;; (gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,comment))))
     ;; (gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,comment))))
     ;; (gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,comment))))
     ;; (gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,comment))))
     ;; (gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,comment))))
     ;; (gnus-group-news-1 ((,class (:foreground nil :weight normal :inherit outline-5))))
     ;; (gnus-group-news-2 ((,class (:foreground nil :weight normal :inherit outline-6))))
     ;; (gnus-group-news-3 ((,class (:foreground nil :weight normal :inherit outline-7))))
     ;; (gnus-group-news-4 ((,class (:foreground nil :weight normal :inherit outline-8))))
     ;; (gnus-group-news-5 ((,class (:foreground nil :weight normal :inherit outline-1))))
     ;; (gnus-group-news-6 ((,class (:foreground nil :weight normal :inherit outline-2))))
     ;; (gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :foreground ,comment))))
     ;; (gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :foreground ,comment))))
     ;; (gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :foreground ,comment))))
     ;; (gnus-group-news-4-empty ((,class (:inherit gnus-group-news-4 :foreground ,comment))))
     ;; (gnus-group-news-5-empty ((,class (:inherit gnus-group-news-5 :foreground ,comment))))
     ;; (gnus-group-news-6-empty ((,class (:inherit gnus-group-news-6 :foreground ,comment))))

     ;; ;; erc
     ;; (erc-direct-msg-face ((,class (:foreground ,orange))))
     ;; (erc-error-face ((,class (:foreground ,red))))
     ;; (erc-header-face ((,class (:foreground ,foreground :background ,selection))))
     ;; (erc-input-face ((,class (:foreground ,green))))
     ;; (erc-keyword-face ((,class (:foreground ,yellow))))
     ;; (erc-current-nick-face ((,class (:foreground ,green))))
     ;; (erc-my-nick-face ((,class (:foreground ,green))))
     ;; (erc-nick-default-face ((,class (:weight normal :foreground ,purple))))
     ;; (erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
     ;; (erc-notice-face ((,class (:foreground ,comment))))
     ;; (erc-pal-face ((,class (:foreground ,orange))))
     ;; (erc-prompt-face ((,class (:foreground ,blue))))
     ;; (erc-timestamp-face ((,class (:foreground ,aqua))))

     ;; ;; woman
     ;; (woman-italic-face ((t (:slant italic :weight bold))))
     ;; (woman-unknown ((t (:foreground ,red :weight bold))))
     ;; (woman-addition ((t (:foreground ,aqua))))
     ;; (woman-bold ((t (:inherit bold :foreground ,blue))))

     ;; (custom-variable-tag ((,class (:foreground ,blue))))
     ;; (custom-group-tag ((,class (:foreground ,blue))))
     ;; (custom-state ((,class (:foreground ,green))))

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
          `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,magenta ,cyan ,background))
          '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))
       (provide-theme ',name))))

(defun coldnew-theme--load-theme (mode)
  (if (fboundp 'load-theme)
      (let ((name (coldnew-theme--theme-name mode)))
        (if (> emacs-major-version 23)
            (load-theme name t)
          (load-theme name)))
    ;; not support for older emace.
    (error "emacs should support load-theme to make coldnew-theme work.")))

;; ;;;; Mode-line


;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory (or load-file-name (buffer-file-name))))))

(provide 'coldnew-theme)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; coldnew-theme.el ends here
