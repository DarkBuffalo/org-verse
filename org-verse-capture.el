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

(defun capt-f ()
	"Define capture file."
	(expand-file-name "notes.org" org-verse-directory))

(defun nil-or-not ()
	"Test if org-verse-current-verse is nil."
	(if (org-capture-get :original-buffer)
			(with-current-buffer (org-capture-get :original-buffer) org-verse-current-verse)
		"essai"
		))

(defun* add-org-capture-templates (&rest project-spec)
  "Add org project."
  (add-to-list 'org-capture-templates project-spec t))

(add-org-capture-templates
 "v" "Org Verse")

(add-org-capture-templates
 "vn" "Org verse note"
 'entry `(file+headline ,(capt-f) ,(nil-or-not))
 "* %?"
 :empty-lines 1)

(defun org-verse-capture ()
  "Capture verse."
  (interactive)
  (org-capture nil "vn"))



(defun my-org-capture-place-template-dont-delete-windows (oldfun &rest args)
	"Dont delete sidebar when capture. OLDFUN and ARGS are requis."
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

(with-eval-after-load "org-capture"
  (advice-add 'org-capture-place-template :around 'my-org-capture-place-template-dont-delete-windows))


(provide 'org-verse-capture)
;;; org-verse-capture.el ends here
