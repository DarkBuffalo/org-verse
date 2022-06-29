;;; org-verse-db.el --- database                     -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'org-verse)
(require 'db)


(defcustom org-verse-cache-dir (concat user-emacs-directory "orgverse/")
  "Org-verse's cache directory."
  :type 'directory
  :group 'org-verse)

(defvar org-verse-notes-db
  (db-make
   `(db-hash
     :filename ,(concat org-verse-cache-dir "org-verse-notes-db")))
  "Database for caching notes.")



(provide 'org-verse-db)
;;; org-verse-db.el ends here
