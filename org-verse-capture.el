;;; org-verse-capture.el --- Capture functionality   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Matthias David

;; Author: Matthias David <darkbuffalo@gnu.re>
;;          ______              __    _______        ___  ___       __
;;         |   _  \ .---.-.----|  |--|   _   .--.--.’  _.’  _.---.-|  .-----.
;;^_/-...-\_^  |   \|  _  |   _|    <|.  1   |  |  |   _|   _|  _  |  |  _  |
;;\__/> o\__/  |    |___._|__| |__|__|.  _   |_____|__| |__| |___._|__|_____|
;;   \   / |:  1    /                |:  1    \
;;   (^_^) |::.. . /                 |::.. .  /
;;         ‘------’                  ‘-------’

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

(defun org-verse-note-f ()
	"Define capture file."
	(expand-file-name "notes.org" org-verse-directory))

(defun add-org-capture-templates (capture-templates)
	"Add capture from CAPTURE-TEMPLATES."
	(setq org-capture-templates
				(append org-capture-templates
								capture-templates)))


(defun capture-get-destination-headline ()
	"Fonction de capture."
	(let* ((buf (org-capture-get :original-buffer))
				 (headline (buffer-local-value 'org-verse-current-verse buf))
				 (file (find-file (org-verse-note-f))))
		(switch-to-buffer file)
		(if (string= headline nil) ;;appel de org-capture en dehors du sidebar
				(org-verse-search-headline (org-verse-complete))
			(org-verse-search-headline headline))))

(defun org-verse-search-headline (headline)
	"Create headline from HEADLINE."
	(goto-char (point-min))
	(if (re-search-forward (format org-complex-heading-regexp-format
																 (regexp-quote headline))
												 nil t)
			(beginning-of-line)
		(goto-char (point-max))
		(unless (bolp) (insert "\n"))
		(insert "* " headline "\n")
		(org-capture-put :verse headline)
		(beginning-of-line 0)))

(add-org-capture-templates
 (doct `(("Org Verse" :keys "v"
					:file org-verse-note-f
					:function capture-get-destination-headline
					:type entry
					:children (("Org verse note"
											:keys "n"
											:template ("* %? \n:PROPERTIES:\n:verse: %(org-capture-get :verse) \n:END:")))))))

(defun org-verse-capture ()
  "Capture verse."
  (interactive)
  (org-capture nil "vn"))

;; fix error org-capture with sidebar

(defun my-org-capture-place-template-dont-delete-windows (oldfun &rest args)
	"Dont delete sidebar when capture. OLDFUN and ARGS are requis."
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

(with-eval-after-load "org-capture"
  (advice-add 'org-capture-place-template :around 'my-org-capture-place-template-dont-delete-windows))


(provide 'org-verse-capture)
;;; org-verse-capture.el ends here
