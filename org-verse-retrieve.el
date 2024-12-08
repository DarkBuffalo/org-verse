;;; org-verse-retrieve.el --- Internal search functions for org-verse  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Matthias David

;; Author: Matthias David <darkbuffalo@gnu.re>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Pour afficher le titre au lieu du nom du fichier dans les backlinks

;; 

;;; Code:

(defconst org-verse-retrieve--title-front-matter-key-regexp
  "^\\(?:#\\+\\)?\\(?:title\\)\\s-*[:=]"
  "Regular expression for title key.")

(defun org-verse-retrieve--search (file key-regexp &optional key)
  "Return value of KEY-REGEXP key in current buffer from FILE.
If optional KEY is non-nil, return the key instead."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward key-regexp nil t 1)
          (if key
              (match-string-no-properties 0)
            (let ((trims "[ \t\n\r\"']+"))
              (string-trim
               (buffer-substring-no-properties (point) (pos-eol))
               trims trims))))))))

(defun org-verse-retrieve--value-title (file &optional key)
  "Return title value from FILE.
If optional KEY is non-nil, return the key instead."
  (org-verse-retrieve--search file org-verse-retrieve--title-front-matter-key-regexp key))


(provide 'org-verse-retrieve)
;;; org-verse-retrieve.el ends here
