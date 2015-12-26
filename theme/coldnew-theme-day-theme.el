;;; coldnew-theme-day-theme.el --- coldnew's emacs color-theme day version.

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

(deftheme coldnew-theme-day
  "coldnew's day theme")

(add-to-list
 'coldnew-theme-colors
 '(day
   .
   (;;  name        sRGB       256
    (background   "#FAFAFA" "#FAFAFA")
    (foreground   "#212121" "#212121")
    (comment      "#607d8b" "#607d8b")
    (current-line "#ECEFF1" "#dadada")

    (red          "#B71C1C" "#B71C1C")
    (orange       "#FF5722" "#FF5722")
    (yellow       "#FFA000" "#FFA000")
    (green        "#558b2f" "#558b2f")
    (aqua         "#00796b" "#00796b")
    (cyan           "#aadddd" "#aadddd")
    (blue         "#2196f3" "#2196f3")
    (magenta      "#4527A0" "#4527A0")
    (black        "#2a2a2a" "#2a2a2a")
    (white        "#ffffff" "#ffffff")

    ;; extra color
    (base03  "#202020"  "#202020")
    (base02  "#292929"  "#292929")
    (base01  "#5f5f5f"  "#5f5f5f")
    (base00  "#999999"  "#999999")
    (base0   "#cccccc"  "#cccccc")
    (base1   "#aaaaaa"  "#aaaaaa")
    (base2   "#e9e2cb"  "#e9e2cb")
    (base3   "#fcf4dc"  "#fcf4dc")

    ;; other
    (cursor  "#0B0B0E")
    (current-line  "#efefef")
    (selection  "#d6d6d6")
    (highlight  "#CAE682")
    ;; (comment  "#8e908c")

    ;; rainbow delimiters
    (rainbow-1    "#e91e63" "#e91e63")
    (rainbow-2    "#1565C0" "#1565C0")
    (rainbow-3    "#EF6C00" "#EF6C00")
    (rainbow-4    "#B388FF" "#B388FF")
    (rainbow-5    "#76FF03" "#76FF03")
    (rainbow-6    "#26A69A" "#26A69A")
    (rainbow-7    "#B71C1C" "#B71C1C")
    (rainbow-8    "#795548" "#795548")
    (rainbow-9    "#827717" "#827717")
    )))

(coldnew-theme--with-colors
  'day
  (apply 'custom-theme-set-faces 'coldnew-theme-day
         (coldnew-theme--face-specs))
  (custom-theme-set-variables
   'coldnew-theme-day
   `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,magenta ,cyan ,background))
   '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))

;; FIXME: need to rewrite

;; ;;;###autoload
;; (defun coldnew-theme-day ()
;;   (interactive)
;;   (coldnew-theme--load-theme 'day))

(provide 'coldnew-theme-day-theme)
;;; coldnew-theme-day-theme.el ends here
