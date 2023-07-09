;;; org-verse-note.el --- Notes pour les versets     -*- lexical-binding: t; -*-

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

;;;; Database
(defvar org-verse-note-db-sqlite-interface
  (if (and (fboundp 'sqlite-available-p) (sqlite-available-p))
      'builtin
    'emacsql)
  "The interface to sqlite to use.
Either `builtin' or `emacsql'. Defaults to builtin when
available. Builtin is available when the version is Emacs 29 or
greater, and emacsql is usable when the `emacsql' package is
installed.")

(defconst org-verse-note-db-sqlite-executable "sqlite3"
  "If using emacs 29 builtin sqlite, this specifices the executable.
It is invoked to make backups.")


(defcustom org-verse-note-db-default-file
  (expand-file-name "~/.org-verse/notes.db")
  "Org-Verse-Note sqlite file for all entries."
  :group 'org-verse-note
  :type 'file)



(defun org-verse-note-db-connect (&optional file)
  "Connect to the database FILE and make sure it is populated.
If FILE is nil, use `org-verse-note-db-default-file'."
  (unless (pcase-exhaustive org-verse-note-db-sqlite-interface
              ('builtin
               (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
              ('emacsql (require 'emacsql nil t)))
    (error "The org-verse-note package requires either Emacs 29 or the emacsql package to be installed."))
  (let ((file (or file org-verse-note-db-default-file)))
    (pcase org-verse-note-db-sqlite-interface
      ('builtin (let* ((db (sqlite-open file)))
                  (org-verse-note-db-setup-table-for-builtin db)
                  db))
      ('emacsql
       (require 'emacsql)
       (let* ((db (emacsql-sqlite file))
              (triple-table-exists
               (emacsql db [:select name
                                    :from sqlite_master
                                    :where (= type table) :and (= name 'triples)])))
         (unless triple-table-exists
           (emacsql db [:create-table org-verse-note ([(id :primary-key)
                                               (refverse text :not-null)
                                               (titre :not-null)
                                               (notes text :not-null)
					       (tags text)])]))
         db)))))

(defun org-verse-note-db-setup-table-for-builtin (db)
  "Set up the triples table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS vnotes (id PRIMARY KEY, refverse TEXT NOT NULL, titre TEXT NOT NULL, notes TEXT NOT NULL, tags TEXT)"))


(defun org-verse-note-db-close (db)
  "Close sqlite database DB."
  (pcase org-verse-note-db-sqlite-interface
    ('builtin (sqlite-close db))
    ('emacsql (emacsql-close db))))



(defun org-verse-note-db--decolon (sym)
  "Remove colon from SYM."
  (intern (string-replace ":" "" (format "%s" sym))))

(defun org-verse-note-db---encolon (sym)
  "Add a colon to SYM."
  (intern (format ":%s" sym)))


(defun org-verse-note-db-standardize-val (val)
  "If VAL is a string, return it as enclosed in quotes
This is done to have compatibility with the way emacsql stores
values. Turn a symbol into a string as well, but not a quoted
one, because sqlite cannot handle symbols. Integers do not need
to be stringified."
  ;; Do not print control characters escaped - we want to get things out exactly
  ;; as we put them in.
  (let ((print-escape-control-characters nil))
    (pcase val
      ;; Just to save a bit of space, let's use "()" instead of "null", which is
      ;; what it would be turned into by the pcase above.
      ((pred null) "()")
      ((pred integerp) val)
      (_ (format "%S" val)))))

(defun org-verse-note-db-standardize-result (result)
  "Return RESULT in standardized form.
This imitates the way emacsql returns items, with strings
becoming either symbols, lists, or strings depending on whether
the string itself is wrapped in quotes."
  (if (numberp result)
        result
      (read result)))


(defun org-verse-note-db-select (db refverse)
  "Return rows from DB matching BOOK CHAPTER VERSE.
If any of these are nil, they are not included in the select statement."
  (pcase org-verse-note-db-sqlite-interface
    ('builtin (mapcar (lambda (row) (mapcar #'org-verse-note-db-standardize-result row))
                      (sqlite-select db "SELECT notes FROM org-verse-note WHERE refverse = ?"
                                     (mapcar #'org-verse-note-db-standardize-val
					     (seq-filter #'identity (list refverse))))))))


;;;; Application

(defgroup org-verse-note nil
  "The Emacs verses notes application.")

(defcustom org-verse-note-db-file nil
  "A filename specifying what the `org-verse-note' database."
  :type 'file
  :group 'org-verse-note)

(defvar org-verse-note-db nil
  "Live sqlite database connection.")

(defun org-verse-note-db-file ()
  "Return the database file we should use in org-verse.
If `org-verse-note-db-file' is nil use`org-verse-note-db-default-file',
otherwise use `org-verse-note-db-file'."
  (if  (null org-verse-note-db-file)
      org-verse-note-db-default-file
    org-verse-note-db-file))



(provide 'org-verse-note)
;;; org-verse-note.el ends here
