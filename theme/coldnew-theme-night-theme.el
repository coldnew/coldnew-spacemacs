;;; coldnew-theme-night-theme.el --- coldnew's emacs color-theme night version.

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
(require 'coldnew-theme)

(deftheme coldnew-theme-night
  "coldnew's night theme")

(add-to-list
 'coldnew-theme-colors
 '(night
   . (;; name              sRGB       256
      (background        "#202020" "#202020")
      (far-background    "#1c1f26" "#121212")
      (foreground        "#c6cccc" "#c6cccc")
      (cursor            "#00c8c8" "#00c8c8")
      (current-line      "#2a2a2a" "#2a2a2a")
      (selection         "#3b3f41" "#3b3f41")
      (highlight         "#CAE682" "#CAE682")

      ;; font-lock
      (buildin           "#ccaaff" "#ccaaff")
      (constant          "#ccaaff" "#ccaaff")
      (comment           "#9ac"    "#9ac")
      (comment-delimiter "#5f5f5f" "#5f5f5f")
      (doc               "#97abc6" "#97abc6")
      (function-name     "#aaccff" "#aaccff")
      (keyword           "#aaffaa" "#aaffaa")
      (type              "#fff59d" "#fff59d")
      (variable-name     "#aaccff" "#aaccff")
      (string            "#aadddd" "#aadddd")

      ;; extra color
      (base03         "#202020"  "#202020")
      (base02         "#292929"  "#292929")
      (base01         "#5f5f5f"  "#5f5f5f")
      (base00         "#999999"  "#999999")
      (base0          "#cccccc"  "#cccccc")
      (base1          "#aaaaaa"  "#aaaaaa")
      (base2          "#e9e2cb"  "#e9e2cb")
      (base3          "#fcf4dc"  "#fcf4dc")

      ;; terminal color
      (red            "#ff3333" "#ff3333")
      (yellow         "#fff59d" "#fff59d")
      (orange         "#ff8888" "#ff8888")
      (green          "#aaffaa" "#aaffaa")
      (blue           "#aaccff" "#aaccff")
      (magenta        "#ccaaff" "#ccaaff")
      (cyan           "#aadddd" "#aadddd")
      (white          "#ffffff" "#ffffff")
      (black          "#2a2a2a" "#2a2a2a")
      (aqua           "#81d4fa" "#81d4fa")

      ;; rainbow delimiters
      (rainbow-1      "#aadddd" "#aadddd")
      (rainbow-2      "#81d4fa" "#81d4fa")
      (rainbow-3      "#aaccff" "#aaccff")
      (rainbow-4      "#aaeecc" "#aaeecc")
      (rainbow-5      "#ccaaff" "#ccaaff")
      (rainbow-6      "#fff59d" "#fff59d")
      (rainbow-7      "#ff8888" "#ff8888")
      (rainbow-8      "#795548" "#795548")
      (rainbow-9      "#827717" "#827717")
      )))

(coldnew-theme--with-colors
  'night
  (apply 'custom-theme-set-faces 'coldnew-theme-night
         (coldnew-theme--face-specs))
  (custom-theme-set-variables
   'coldnew-theme-night
   `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,magenta ,cyan ,background))
   '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))

;;;###autoload
(defun coldnew-theme-night ()
  (interactive)
  (coldnew-theme--load-theme 'night))

(provide 'coldnew-theme-night-theme)
;;; coldnew-theme-night-theme.el ends here.
