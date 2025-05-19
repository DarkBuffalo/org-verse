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


;;; Code:
(require 'cl-lib)
(require 'ewoc)

(defvar-local org-verse-note-n-tags nil
  "List of associated tags for creating and removing notes.")

(defface org-verse-note-metadata
  '((default :background "grey"))
  "Face shown for the metadata section of notes.")

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
  "If using Emacs 29 builtin sqlite, this specifices the executable.
It is invoked to make backups.")


(defcustom org-verse-note-db-default-file
  (expand-file-name "~/vnotes.db")
  "Org-Verse-Note sqlite file for all entries."
  :group 'org-verse-note
  :type 'file)


(defun org-verse-note-db-connect (&optional file)
  "Connect to the database FILE and make sure it is populated.
If FILE is nil, use `org-verse-note-db-default-file'."

  (let* ((file (or file org-verse-note-db-default-file))
        (db (sqlite-open file)))
    (org-verse-note-db-setup-table-for-builtin db)
    db))

(defun org-verse-note-db-setup-table-for-builtin (db)
  "Set up the triples table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS vnotes (id PRIMARY KEY, refverse TEXT NOT NULL, titre TEXT NOT NULL, notes TEXT NOT NULL, tags TEXT)"))


(defun org-verse-note-db-close (db)
  "Close sqlite database DB."
   (sqlite-close db))


(defun org-verse-note-db--decolon (sym)
  "Remove colon from SYM."
  (intern (string-replace ":" "" (format "%s" sym))))

(defun org-verse-note-db---encolon (sym)
  "Add a colon to SYM."
  (intern (format ":%s" sym)))


(defun org-verse-note-db-standardize-val (val)
  "If VAL is a string, return it as enclosed in quotes.
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
  "Return rows from DB matching REFVERSE.
If any of these are nil, they are not included in the select statement."
  (pcase org-verse-note-db-sqlite-interface
    ('builtin (mapcar (lambda (row) (mapcar #'org-verse-note-db-standardize-result row))
                      (sqlite-select db "SELECT notes FROM org-verse-note WHERE refverse = ?"
                                     (mapcar #'org-verse-note-db-standardize-val
					     (seq-filter #'identity (list refverse))))))))



;;;; Application

(cl-defstruct org-verse-note-n id refv title note tags creation-time modified-time)

(defun org-verse-note-n-create (ref titre note tags)
  "Create a new `org-verse-note-n' with REF, TITRE, NOTE and TAGS."
  (let* ((time (time-convert (current-time) 'integer)))
    (make-org-verse-note-n :refv ref
			   :title titre
			   :note note
			   :tags tags
			   :creation-time time
			   :modified-time time)))


(defvar-local org-verse-note-ewoc nil
  "Ewoc for the notes buffer.")

(defcustom org-verse-note-db-file nil
  "A filename specifying what the `org-verse-note' database."
  :type 'file
  :group 'org-verse)

(defvar org-verse-note-db nil
  "Live sqlite database connection.")

(defun org-verse-note-db-file ()
  "Return the database file we should use in org-verse.
If `org-verse-note-db-file' is nil use`org-verse-note-db-default-file',
otherwise use `org-verse-note-db-file'."
  (if  (null org-verse-note-db-file)
      org-verse-note-db-default-file
    org-verse-note-db-file))

(defalias 'org-verse-note-connect
  (let ((ovn--in-connect-call))
    (lambda ()
      (unless ovn--in-connect-call
        (setq ovn--in-connect-call t)
        (unwind-protect
            (progn (unless org-verse-note-db
                     (setq org-verse-note-db (org-verse-note-db-connect (org-verse-note-db-file)))))
          (setq ovn--in-connect-call nil)))))
  "Ensure EKG-DB is connected.
Also make sure the database is set up correctly. This should be
called before any access to triples, unless we are sure all
callers have already called this function")

(defun org-verse-note-capture ()
  "Capture note."
  (interactive)
  (let* ((buf (get-buffer-create (format "*OVN Capture*"))))
    (set-buffer buf)
    (funcall 'org-mode)
    (org-verse-note-capture-mode 1)
    (setq org-verse-note-n (org-verse-note-n-create "1:1:1" "titre de la  note" nil "essai" ) )
    (pop-to-buffer buf)))


(defvar org-verse-note-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-verse-note-capture-finalize)
    (define-key map "\C-c\C-k" #'org-verse-note-kill)
    map)
  "Key map for `org-verse-note-capture-mode', a minor mode.
This is used when capturing new notes.")

(define-minor-mode org-verse-note-capture-mode
  "Minor mode for simple finish/cancel keybindings."
  :init-value nil
  :lighter " OVN-CAP"
  :interactive nil
  (when org-verse-note-capture-mode
    (setq-local header-line-format
                (substitute-command-keys
                 "\\<org-verse-note-capture-mode-map>Capture buffer.  Finish \
`\\[org-verse-note-capture-finalize]'.\
Annuler `\\[org-verse-note-kill]'."))))


(defun org-verse-note-capture-finalize ()
  "Save the current note."
  (interactive nil org-verse-mode-capture-mode)
  ;;(ekg--update-from-metadata)
  (org-verse-note-save)
  (let ((note org-verse-note-n))
    (kill-buffer)
    (cl-loop for b being the buffers do
             (with-current-buffer b
               (when (seq-intersection (org-verse-note-n-tags note)
                                       org-verse-note-n-tags)
                 (ewoc-enter-last org-verse-note-ewoc note))))))


(defun org-verse-note-kill ()
  "Cancel the current note capture."
  (interactive)
  (kill-buffer))

(defun org-verse-note--metadata-overlay ()
  "Return the overlay used for metadata."
  (or (car (seq-filter
            (lambda (o) (eq 'org-verse-note-metadata (overlay-get o 'category)))
            (overlays-in (point-min) (point-max))))
      (make-overlay (point-min) (point-max) nil nil t)))


(defun org-verse-note-extract-inlines (text)
  "Return a cons of TEXT without inline commands, and the commands.
The commands returned are the most specific type of struct known,
or, if unknown, `ekg-inline'."
  (let ((inlines)
        (newtext text)
        (inline-rx (rx (seq (group-n 1 "%"
                                     (group-n 2 (zero-or-one "n"))
                                     (literal "(")
                                     (group-n 3 (*? anychar))
                                     (literal ")"))))))
    ;; Keep removing commands left to right to make sure our positions are
    ;; without commands, since that's how they will need to be inserted.
    (while (when-let (index (string-match inline-rx newtext))
             (push (make-org-verse-note-n-inline :pos index
						 :command (read (format "(%s)" (match-string 3 newtext)))
						 :type (pcase (match-string 2 newtext)
							 ("" 'command)
							 ("n" 'note)))
                   inlines)
             (setq newtext (replace-match "" nil nil newtext 1))))
    (cons newtext (nreverse inlines))))

(defun org-verse-note-save ()
  "Save note"
  (message "save note ;-)")
  ;;(ekg-connect)
  (widen)
  (let* ((text (buffer-substring (+ 1 (overlay-end (org-verse-note--metadata-overlay)))
                                 (point-max)))
         (ticons (org-verse-note-extract-inlines text)))
    (setf (ekg-note-text ekg-note) (car ticons)
          (ekg-note-inlines ekg-note) (cdr ticons)
          (ekg-note-mode ekg-note) major-mode
          (ekg-note-tags ekg-note) (seq-uniq (ekg-note-tags ekg-note)))))




(provide 'org-verse-note)
;;; org-verse-note.el ends here
