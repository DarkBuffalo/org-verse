;;; org-verse-capture.el --- Capture functionality   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Matthias David

;; Author: Matthias David <work@gnu.re>


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

;; 

;;; Code:
(require 'org-verse)

(defun* add-org-capture-templates (&rest project-spec)
  "Add org project."
  (add-to-list 'org-capture-templates project-spec t))


(add-org-capture-templates
 "v" "Org Verse")

(add-org-capture-templates
 "vn" "Org verse note"
 'entry `(file+headline ,(expand-file-name "notes.org" org-verse-directory) "Notes")
 "* %? %^g\n %i\n [%a]\n"
 :empty-lines 1)

(provide 'org-verse-capture)
;;; org-verse-capture.el ends here
