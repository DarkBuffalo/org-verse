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

(require 'triples)
(require 'seq)
;;(require 's)
(require 'ewoc)
;; (require 'cl-lib)
(require 'kv)

(defgroup org-verse nil
  "The emacs knowledge graph, an app for notes and structured data.")

(defcustom org-verse-capture-note-default-mode 'org-mode
  "The default mode for all new notes."
  :type 'symbol
  :group 'org-verse)

(defcustom org-verse-capture-note-acceptable-modes '(org-mode markdown-mode fundamental-mode)
  "Modes that make sense to use as note types."
  :type '(set symbol)
  :group 'org-verse)

(defcustom org-verse-capture-note-auto-tag-funcs '(org-verse-date-tag)
  "Functions to run to create tags automatically.
The functions are run in the note buffer before saving it.
Return a list of tags to add."
  :type '(set function)
  :group 'org-verse)

(defcustom org-verse-format-funcs '()
  "Functions to run to format what we display to the user.
What we display on the UI is different than what we store
internally, which is the document itself. Each function here will
be run in sequence on a temporary buffer, and is responsible for
making the changes in the current buffer to affect the formatting
however it wants. It is the responsibility of each function to
check for the mode of the buffer."
  :type '(set function)
  :group 'org-verse)

(defcustom org-verse-db-file "~/.org-verse-note.db"
  "Location of DB file used by ORG-VERSE."
  :type 'file
  :group 'org-verse)

(defface org-verse-title
  '((((type graphic)) :height 2.0 :box t :inherit hl-line)
    (((type tty))) :underline t :inherit hl-line)
  "Face shown for ORG-VERSE titles")

(defface org-verse-tag
  '((((type graphic)) :height 1.0 :box t :inherit default)
    (((type tty))) :underline t :inherit hl-line)
  "Face shown for ORG-VERSE tags")

(defface org-verse-metadata
  '((default :background "grey" :inherit default))
  "Face shown for the metadata section of notes.")

(defvar org-verse-db nil
  "Live sqlite database connection.")

(defvar org-verse-metadata-parsers '(("Tags" . org-verse--metadata-update-tag)
				     ("URL" . org-verse--metadata-update-url)
				     ("Title" . org-verse--metadata-update-title))
  "Alist of metadata field to a function that updates the buffer's
`org-verse-note' with the results of the field. The function takes one
argument, the field metadata property value.")

(defvar org-verse-metadata-labels '((reference/url . "URL")
				    (titled/title . "Title"))
  "Alist of properties that can be on the note, and the labels they
have in the metadata section. The label needs to match the keys
in the `org-verse-metadata-parsers' alist.")

(cl-defstruct org-verse-note
  id text mode tags creation-time modified-time properties)

(defun org-verse--connect ()
  "Ensure ORG-VERSE-DB is connected."
  (unless org-verse-db
    (setq org-verse-db (triples-connect org-verse-db-file))
    (org-verse-add-schema)))

(defun org-verse--close ()
  "Close the ORG-VERSE-DB connection."
  (setq org-verse-db nil))

(defun org-verse-add-schema ()
  "Add schema necessary for ORG-VERSE to function."
  (triples-add-schema org-verse-db 'tagged 'tag)
  (triples-add-schema org-verse-db 'text
                      '(text :base/unique t :base/type string)
                      '(mode :base/unique t :base/type symbol))
  (triples-add-schema org-verse-db 'time-tracked
                      '(creation-time :base/unique t :base/type integer)
                      '(modified-time :base/unique t :base/type integer))
  (triples-add-schema org-verse-db 'tag
                      '(tagged :base/virtual-reversed tagged/tag))
  (triples-add-schema org-verse-db 'named 'name)
  (triples-add-schema org-verse-db 'email 'address)
  ;; Person is just a marker
  (triples-add-schema org-verse-db 'person)
  (triples-add-schema org-verse-db 'reference '(url :base/type string))
  ;; A URL can be a subject too, and has data, including the title.
  (triples-add-schema org-verse-db 'titled '(title :base/type string)))

(defun org-verse--normalize-note (note)
  "Make sure NOTE adheres to org-verse-wide constraints before saving.
This
  1) makes sure all tags are lowercase.
  2) removes commas in tags, since those are used to separate tags.
  3) trim the text of any trailing or leading whitespace."
  (setf (org-verse-note-tags note)
        (mapcar (lambda (tag) (downcase (string-replace "," "" tag))) (org-verse-note-tags note)))
  (setf (org-verse-note-text note)
        (s-trim (org-verse-note-text note))))

(defun org-verse-save-note (note)
  "Save NOTE in database, replacing note information there."
  (org-verse--connect)
  (org-verse--normalize-note note)
  (triples-with-transaction org-verse-db
    (triples-set-type org-verse-db (org-verse-note-id note) 'tagged :tag (org-verse-note-tags note))
    (triples-set-type org-verse-db (org-verse-note-id note) 'text :text (org-verse-note-text note)
                      :mode (org-verse-note-mode note))
    ;; Note that we recalculate modified time here, since we are modifying the
    ;; entity.
    (let ((modified-time (time-convert (current-time) 'integer)))
      (triples-set-type org-verse-db (org-verse-note-id note) 'time-tracked
                        :creation-time (org-verse-note-creation-time note)
                        :modified-time modified-time)
      (setf (org-verse-note-modified-time note) modified-time))
    (mapc (lambda (tag) (triples-set-type org-verse-db tag 'tag)) (org-verse-note-tags note))
    (apply #'triples-set-types org-verse-db (org-verse-note-id note) (org-verse-note-properties note))))

(defun org-verse-get-notes-with-tags (tags)
  "Get all notes with TAGS, returning a list of `org-verse-note' structs."
  (org-verse--connect)
  (cl-loop for tag in tags
           with results = nil
           do
           (when-let (tag-ids (plist-get (triples-get-type org-verse-db tag 'tag) :tagged))
             (setq results
                   (if results (seq-intersection tag-ids results)
                     tag-ids)))
           finally return
           (cl-loop for id in results
                    collect
                    (org-verse-get-note-with-id id))))

(defun org-verse-get-notes-with-tag (tag)
  "Get all notes with TAG, returning a list of `org-verse-note' structs."
  (org-verse-get-notes-with-tags (list tag)))

(defun org-verse-get-note-with-id (id)
  "Get the specific note with ID."
  (let ((v (triples-get-subject org-verse-db id)))
    (make-org-verse-note :id id
                   :text (plist-get v :text/text)
                   :mode (plist-get v :text/mode)
                   :tags (plist-get v :tagged/tag)
                   :creation-time (plist-get v :time-tracked/creation-time)
                   :modified-time (plist-get v :time-tracked/modified-time)
                   :properties (kvalist->plist
                                (seq-filter (lambda (kvcons)
                                              (not (member (car kvcons)
                                                           '(text/text
                                                             text/mode
                                                             tagged/tag
                                                             time-tracked/creation-time
                                                             time-tracked/modified-time))))
                                            (kvplist->alist v))))))

(defun org-verse-note-delete (note)
  "Delete NOTE from the database.
This doesn't actually delete, but rather prepends all tags with
 'trash/'. This can be garbage collected at a later time.
If all tags are trash tags, then the note is really deleted."
  (if (seq-every-p #'org-verse-tag-trash-p (org-verse-note-tags note))
      (triples-delete-subject org-verse-db (org-verse-note-id note))
    (setf (org-verse-note-tags note)
          (mapcar (lambda (tag) (unless (org-verse-tag-trash-p tag)
                                  (org-verse-mark-trashed tag)))
                  (org-verse-note-tags note)))
    (org-verse-save-note note)))

(defun org-verse-has-live-tags-p (sub)
  "Return true if SUB represents an undeleted note."
  (seq-filter (lambda (tag) (not (org-verse-tag-trash-p tag))) (plist-get (triples-get-type org-verse-db sub 'tagged) :tag)))

(defun org-verse-displayable-note-text (note)
  "Return text, with mode-specific properties, of NOTE.
A text property `org-verse-note-id' is added with the id of the note.
FORMATTERS is the optional set of functions run in the buffer
before finishing."
  (with-temp-buffer
    (when (org-verse-note-text note)
      (insert (org-verse-note-text note)))
    (when (org-verse-note-mode note)
      (let ((mode-func (intern (format "%s-mode" (org-verse-note-mode note)))))
        (if (fboundp mode-func) (funcall #'mode-func)
          (funcall (org-verse-note-mode note)))))
    (mapc #'funcall org-verse-format-funcs)
    (font-lock-ensure)
    (put-text-property (point-min) (point-max) 'org-verse-note-id (org-verse-note-id note))
    (buffer-string)))

(defvar org-verse-capture-note-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-verse-capture-note-finalize)
    map)
  "Key map for `org-verse-capture-note-mode', a minor mode.
This is used when capturing new notes.")

(define-minor-mode org-verse-capture-note-mode
  "Minor mode for simple finish/cancel keybindings."
  :init-value nil
  :lighter "ORG-VERSE CAP"
  :interactive nil
  (when org-verse-capture-note-mode
    (setq-local completion-at-point-functions
                (cons #'org-verse--capf completion-at-point-functions)
                header-line-format
                (substitute-command-keys
                 "\\<org-verse-capture-note-mode-map>Capture buffer.  Finish \
`\\[org-verse-capture-note-finalize]'."))))

(defvar org-verse-capture-note-mode-hook nil
  "Hook for `org-verse-capture-note-mode'.")

(defvar org-verse-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-verse-edit-finalize)
    map)
  "Key map for `org-verse-edit-mode', a minor mode.
This is used when editing existing blocks.")

(define-minor-mode org-verse-edit-mode
  "Minor mode for simple finish/cancel keybindings."
  :init-value nil
  :lighter "ORG-VERSE ED"
  :interactive nil)

(defvar org-verse-edit-mode-hook nil
  "Hook for `org-verse-edit-mode'.")

(defvar-local org-verse-note nil
  "Holds the note information for buffers adding or changing notes.")

(defun org-verse-note-create (text mode tags)
  "Create a new `org-verse-note' with TEXT, MODE and TAGS."
  (let* ((time (time-convert (current-time) 'integer))
         (subject (sxhash (cons time (random 100)))))
    (make-org-verse-note :id subject
                   :text text
                   :mode mode
                   :tags tags
                   :creation-time time
                   :modified-time time)))

(defun org-verse--metadata-string-to-tag (s)
  "Return string S as a tag."
  (replace-regexp-in-string (rx ?\") "" s))

(defun org-verse--metadata-string (property value)
  "Return a representation of PROPERTY with VALUE for the metadata.
This will be displayed at the top of the note buffer."
  (format "%s: %s\n" (propertize property 'face 'bold 'read-only t)
          value))

(defun org-verse--replace-metadata ()
  "Replace the metadata in a buffer."
  (let ((note org-verse-note))
    (with-temp-buffer
      (insert
       (org-verse--metadata-string "Tags"
                             (mapconcat (lambda (tag) (format "%s" tag))
                                        (org-verse-note-tags note) ", ")))
      (mapc (lambda (pval-cons)
              (when-let (label (cdr (assoc (car pval-cons) org-verse-metadata-labels)))
                (insert (org-verse--metadata-string
                         label
                         (if (listp (cdr pval-cons))
                             (mapconcat (lambda (v) (format "%s" v))
                                        (cdr pval-cons) ", ")
                           (format "%s" (cdr pval-cons)))))))
            (kvplist->alist (org-verse-note-properties note)))
      (buffer-string))))

(defun org-verse--metadata-overlay ()
  "Return the overlay used for metadata"
  (or (car (seq-filter
            (lambda (o) (eq 'org-verse-metadata (overlay-get o 'category)))
            (overlays-in (point-min) (point-max))))
      (make-overlay (point-min) (point-max) nil nil t)))

(defun org-verse--metadata-modification (overlay after begin end &optional length)
  "Make sure that metadata region doesn't interfere with editing.
This function is called on modification within the metadata.
We want to make sure of a few things:
  1) The user isn't adding empty lines.
  2) There is at least one non-metadata line in the buffer."
  (when after
    (save-excursion
      (goto-char begin)
      (replace-regexp (rx (seq line-start (zero-or-more space) line-end)) "" nil begin end)
      (when (= (overlay-end overlay)
               (buffer-end 1))
        (goto-char (buffer-end 1))
        (insert "\n")))))

(defun org-verse-edit-display-metadata ()
  "Create or edit the overlay to show metadata."
  (let ((o (org-verse--metadata-overlay))
        (inhibit-read-only t))
    (buffer-disable-undo)
    (replace-region-contents (overlay-start o) (overlay-end o)
                             #'org-verse--replace-metadata)
    (goto-char (overlay-end o))
    (insert "\n")
    (move-overlay o (point-min) (- (overlay-end o) 1))
    (overlay-put o 'before-string (propertize "Note properties\n" 'face '(underline org-verse-metadata)
                                              'read-only t))
    (overlay-put o 'category 'org-verse-metadata)
    (overlay-put o 'modification-hooks '(org-verse--metadata-modification))
    (overlay-put o 'face 'org-verse-metadata)
    (buffer-enable-undo)
    ;; If org-mode is on, the metadata messes up the org-element-cache, so let's disable it.
    (when (eq major-mode 'org-mode)
      (setq-local org-element-use-cache nil))))

(defun org-verse-capture-note (&optional tags properties subject)
  "Capture a new note, with TAGS and other PROPERTIES.
If SUBJECT is given, force the triple subject to be that value."
  (interactive)
  (let ((buf (get-buffer-create "*ORG-VERSE Capture*")))
    (set-buffer buf)
    (funcall org-verse-capture-note-default-mode)
    (org-verse-capture-note-mode 1)
    (let* ((time (time-convert (current-time) 'integer)))
      (setq org-verse-note
            (org-verse-note-create nil org-verse-capture-note-default-mode
                             (seq-uniq (append
                                        tags
                                        (mapcan (lambda (f) (funcall f)) org-verse-capture-note-auto-tag-funcs)))))
      (when subject (setf (org-verse-note-id org-verse-note) subject)))
    (setf (org-verse-note-properties org-verse-note) properties)
    (org-verse-edit-display-metadata)
    (goto-char (point-max))
    (switch-to-buffer-other-window buf)))

(defun org-verse-capture-note-url (&optional url)
  "Capture a new note given a URL."
  (interactive "MURL: ")
  (org-verse-capture-note)
  (goto-char (overlay-start (org-verse--metadata-overlay)))
  (insert "URL: " url "\n"))

(defun org-verse-capture-note-change-mode (mode)
  "Change the mode of the current note."
  (interactive (list
                (completing-read "Mode: " org-verse-capture-note-acceptable-modes)) org-verse-capture-note-mode)
  (let ((note org-verse-note))
    (funcall (intern mode))
    (org-verse-capture-note-mode 1)
    (setq-local header-line-format 
                (substitute-command-keys
                 "\\<org-verse-capture-note-mode-map>Capture buffer.  Finish \
`\\[org-verse-capture-note-finalize]'."))
    (setq org-verse-note note)))

(defun org-verse-edit (note)
  "Edit an existing NOTE."
  (interactive nil org-verse-notes-mode)
  (let ((buf (get-buffer-create (format "*ORG-VERSE Edit: %s*" (org-verse-note-id note)))))
    (set-buffer buf)
    (erase-buffer)
    (when (org-verse-note-mode note)
      (funcall (org-verse-note-mode note)))
    (org-verse-edit-mode 1)
    (setq-local completion-at-point-functions
                (cons #'org-verse--capf completion-at-point-functions)
                header-line-format
                (substitute-command-keys
                 "\\<org-verse-edit-mode-map>Capture buffer.  Finish \
`\\[org-verse-edit-finalize]'.")
                org-verse-note (copy-org-verse-note note))
    (org-verse-edit-display-metadata)
    (insert (org-verse-note-text note))
    (goto-char (+ 1 (overlay-end (org-verse--metadata-overlay))))
    (switch-to-buffer-other-window buf)))

(defun org-verse--save-note-in-buffer ()
  "Save the current note.
Return the latest `org-verse-note' object."
  (org-verse--connect)
  (widen)
  (setf (org-verse-note-text org-verse-note)
        (buffer-substring (overlay-end (org-verse--metadata-overlay))
                          (point-max))
        (org-verse-note-mode org-verse-note) major-mode
        (org-verse-note-tags org-verse-note) (seq-uniq (org-verse-note-tags org-verse-note)))
  (org-verse-save-note org-verse-note)
  org-verse-note)

(defun org-verse--in-metadata-p ()
  "If the point is in the metadata section."
  (< (point) (overlay-end (org-verse--metadata-overlay))))

(defun org-verse--metadata-current-field ()
  "Return the label and value of the current metadata property.
If none can be found, return NIL."
  (when (org-verse--in-metadata-p)
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (when (string-match
              (rx (seq (group (seq (one-or-more (not ?\:))))
                       ?\: (one-or-more whitespace)
                       (group (zero-or-more anychar)))) line)
        (cons (substring-no-properties (match-string 1 line))
              (substring-no-properties (match-string 2 line)))))))

(defvar org-verse-capf-field-complete-funcs
  '(("Tags" . org-verse--tags-complete))
  "Functions to complete values in various metadata fields.
The function is expected to behave as normal for a function in
`completion-at-point-functions'.")

(defun org-verse--capf ()
  "Completion function for all metadata at `completion-at-point-functions'.
If no completion function is found for the field type, don't
attempt the completion."
  (if-let (field (org-verse--metadata-current-field))
    (when-let (completion-func (assoc (car field) org-verse-capf-field-complete-funcs
                                      #'equal))
      (funcall (cdr completion-func)))
    ;; There's no current field, but we're in the metadata, so let's complete
    ;; the possible fields.
    (when (org-verse--in-metadata-p)
      (org-verse--field-name-complete))))

(defun org-verse--field-name-complete ()
  "Completion function for metadata field names."
  (list (save-excursion (beginning-of-line) (point))
        (save-excursion (skip-chars-forward "^:\s\t\n") (point))
        (completion-table-dynamic
         (lambda (_)
           (seq-difference (mapcar #'car org-verse-metadata-parsers)
                           (mapcar #'car (org-verse--metadata-fields)))))))

(defun org-verse--tags-cap-exit (completion finished)
  "Cleanup after completion at point happened in a tag.
The cleanup now is just to always have a space after every comma."
  (when finished
    (save-excursion
      (when (search-backward "," (line-beginning-position) t)
        (replace-string (format ",%s" completion)
                        (format ", %s" completion))))))

(defun org-verse--tags-complete ()
  "Completion function for tags, CAPF-style."
  (let ((end (save-excursion
               (skip-chars-forward "^,\t\n")
               (point)))
	    (start (save-excursion
                 (skip-chars-backward "^,\t\n")
                 (point))))
    (list start end (completion-table-dynamic
                     (lambda (_) (org-verse-tags)))
          :exclusive t :exit-function #'org-verse--tags-cap-exit)))

(defun org-verse-edit-finalize ()
  "Save the edited note and refresh where it appears."
  (interactive nil org-verse-edit-mode)
  (org-verse--update-from-metadata)
  (let ((note (org-verse--save-note-in-buffer)))
    (kill-buffer)
    (cl-loop for b being the buffers do
           (with-current-buffer b
               (when (and (eq major-mode 'org-verse-notes-mode)
                          (seq-intersection (org-verse-note-tags note)
                                            org-verse-notes-tags))
                 (let ((n (ewoc-nth org-verse-notes-ewoc 0)))
                   (while n
                     (when (equal (org-verse-note-id (ewoc-data n))
                                  (org-verse-note-id note))
                       (ewoc-set-data n note))
                     (setq n (ewoc-next org-verse-notes-ewoc n))))
                 (ewoc-refresh org-verse-notes-ewoc))))))

(defun org-verse--metadata-update-tag (val)
  "Update the tag field from the metadata VAL."
  (setf (org-verse-note-tags org-verse-note) (s-split (rx (seq ?\, (zero-or-more space))) val t)))

(defun org-verse--metadata-update-title (val)
  "Update the title field from the metadata VAL."
  (setf (org-verse-note-properties org-verse-note)
        (plist-put (org-verse-note-properties org-verse-note) :titled/title (s-split (rx (seq ?\, (zero-or-more space))) val t))))

(defun org-verse--metadata-update-url (val)
  "Update the url field from the metadata VAL."
  (setf (org-verse-note-properties org-verse-note)
        (plist-put
         (org-verse-note-properties org-verse-note)
         :reference/url (s-split (rx (seq ?\, (zero-or-more space))) val t))))

(defun org-verse--metadata-fields ()
  "Return all metadata fields as a cons of labels and values."
  (save-excursion
    (let ((mo (org-verse--metadata-overlay))
          (fields))
      (goto-char (overlay-start mo))
      (while (< (point) (overlay-end mo))
        (if-let (field (org-verse--metadata-current-field))
            (push field fields)
          (warn "ORG-VERSE: No field could be parsed from metadata line at point %s" (point)))
        (forward-line))
      fields)))

(defun org-verse--update-from-metadata ()
  "Update the `org-verse-note' object from the metadata."
  (cl-loop for field in (org-verse--metadata-fields)
           do
           (if-let (func (assoc (car field) org-verse-metadata-parsers))
                (funcall (cdr func) (cdr field))
              (warn "ORG-VERSE: No function found for field %s" (car field)))))

(defun org-verse-capture-note-finalize ()
  "Save the current note."
  (interactive nil org-verse-capture-note-mode)
  (org-verse--update-from-metadata)
  (org-verse--save-note-in-buffer)
  (let ((note org-verse-note))
    (kill-buffer)
    (cl-loop for b being the buffers do
           (with-current-buffer b
               (when (and (eq major-mode 'org-verse-notes-mode)
                          (seq-intersection (org-verse-note-tags note)
                                            org-verse-notes-tags))
                 (ewoc-enter-last org-verse-notes-ewoc note))))))

(defun org-verse-tag-trash-p (tag)
  "Return true if TAG is part of the trash."
  ;; All tags should be strings, but better to ignore violations here.
  (and (stringp tag)
       (s-starts-with-p "trash/" tag)))

(defun org-verse-mark-trashed (tag)
  "Return TAG transformed to mark it as trash."
  (format "trash/%s" tag))

(defun org-verse-rename-tag (from-tag to-tag)
  "Rename FROM-TAG to TO-TAG.
This can be done whether or not TO-TAG exists or not."
  (interactive (list (completing-read "From tag: " (org-verse-tags))
                     (completing-read "To tag: " (org-verse-tags))))
  (triples-with-transaction
    org-verse-db
    (pcase triples-sqlite-interface
      ('builtin (sqlite-execute
                 org-verse-db
                 "UPDATE triples SET object = ? WHERE object = ? AND predicate = 'tagged/tag'"
                 (to-tag from-tag)))
      ('emacsql (emacsql org-verse-db [:update triples :set (= object $s1) :where (= object $s2) :and (= predicate 'tagged/tag)]
                         to-tag from-tag)))
    (triples-remove-type org-verse-db from-tag 'tag)
    (triples-set-type org-verse-db to-tag 'tag)))

(defun org-verse-tags ()
  "Return a list of all tags.
Does not include any 'trash' tags."
  (org-verse--connect)
  (seq-filter (lambda (tag) (not (org-verse-tag-trash-p tag)))
              (triples-subjects-of-type org-verse-db 'tag)))

(defun org-verse-tags-including (substring)
  "Return all tags including SUBSTRING."
  (org-verse--connect)
  (seq-filter (lambda (tag) (and (not (org-verse-tag-trash-p tag))
                                 (string-match-p (rx (literal substring)) tag)))
              (triples-subjects-of-type org-verse-db 'tag)))

(defun org-verse-tags-display (tags)
  "Return a propertized representation of TAGS, a list."
  (mapconcat (lambda (tag) (propertize tag 'face 'org-verse-tag))
             (sort tags #'string<) " "))

(defun org-verse-display-note (note)
  "Display NOTE in buffer."
  (insert (org-verse-displayable-note-text note))
  (insert "\n")
  (insert (org-verse-tags-display (org-verse-note-tags note))))

(defvar org-verse-notes-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "a" #'org-verse-notes-any-tags)
    (define-key map "c" #'org-verse-notes-create)
    (define-key map "d" #'org-verse-notes-delete)
    (define-key map "g" #'org-verse-notes-refresh)
    (define-key map "n" #'org-verse-notes-next)
    (define-key map "o" #'org-verse-notes-open)
    (define-key map "p" #'org-verse-notes-previous)
    (define-key map "r" #'org-verse-notes-remove)
    (define-key map "t" #'org-verse-notes-tag)
    map))

(define-derived-mode org-verse-notes-mode fundamental-mode "org-verse-notes"
  "Major mode for showing a list of notes that can be interacted with."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defvar-local org-verse-notes-fetch-notes-function nil
  "Function to call to fetch the notes that define this buffer.")

(defvar-local org-verse-notes-ewoc nil
  "Ewoc for the notes buffer.")

(defvar-local org-verse-notes-hl nil
  "Highlight for the notes buffer.")

(defvar-local org-verse-notes-tags nil
  "List of associated tags for creating and removing notes.")

(defun org-verse--note-highlight ()
  "In the buffer, highlight the current note."
  (let ((node (ewoc-locate org-verse-notes-ewoc)))
    (when (and node (ewoc-location node))
      (move-overlay org-verse-notes-hl
                    (ewoc-location node)
                    (- (or (if-let (next (ewoc-next org-verse-notes-ewoc node))
                               (ewoc-location next)
                             (point-max))) 1)))))

(defun org-verse--current-note-or-error ()
  "Return the current `org-verse-note'.
Raise an error if there is no current note."
  (unless (eq major-mode 'org-verse-notes-mode)
    (error "This command can only be used in `org-verse-notes-mode'."))
  (if-let (node (ewoc-locate org-verse-notes-ewoc))
      (ewoc-data node)
    (error "No current note is available to act on!  Create a new note first with `org-verse-capture-note'.")))

(defun org-verse-notes-tag (&optional tag)
  "Select amongst the tags of the current note."
  (interactive (list (completing-read "Tag: " (org-verse-note-tags (org-verse--current-note-or-error))))
              org-verse-notes-mode)
  (org-verse-show-tag tag))

(defun org-verse-notes-open ()
  "Open the current note."
  (interactive nil org-verse-notes-mode)
  (org-verse-edit (org-verse--current-note-or-error)))

(defun org-verse-notes-delete ()
  "Delete the current note."
  (interactive nil org-verse-notes-mode)
  (let ((note (org-verse--current-note-or-error))
        (inhibit-read-only t))
    (when (y-or-n-p "Or you sure you want to delete this note?")
      (org-verse-note-delete note)
      (ewoc-delete org-verse-notes-ewoc (ewoc-locate org-verse-notes-ewoc))
      (org-verse--note-highlight))))

(defun org-verse-notes-remove ()
  "Remove the current tags from the current note.
This prepends the tags with trash, which removes them from view,
but allows for re-instatement later."
  (interactive nil org-verse-notes-mode)
  (let ((note (org-verse--current-note-or-error)))
    (setf (org-verse-note-tags note)
          (mapcar (lambda (tag) (if (member tag org-verse-notes-tags)
                            (org-verse-marked-trash tag)
                          tag)) (org-verse-note-tags note)))
    (org-verse-save-note note))
  (setq buffer-read-only nil)
    (ewoc-delete org-verse-notes-ewoc (ewoc-locate org-verse-notes-ewoc))
    (setq buffer-read-only t)
    (org-verse--note-highlight))

(defun org-verse-notes-refresh ()
  "Refresh the current `org-verse-notes' buffer."
  (interactive nil org-verse-notes-mode)
  (org-verse--show-notes org-verse-notes-fetch-notes-function org-verse-notes-tags))

(defun org-verse-notes-create ()
  "Add a note that default in the tags in the buffer."
  (interactive nil org-verse-notes-mode)
  (org-verse-capture-note org-verse-notes-tags))

(defun org-verse-notes-next ()
  "Move to the next note, if possible."
  (interactive nil org-verse-notes-mode)
  (if-let (next (ewoc-next org-verse-notes-ewoc (ewoc-locate org-verse-notes-ewoc)))
      (progn
        (goto-char (ewoc-location next))
        (org-verse--note-highlight))))

(defun org-verse-notes-previous ()
  "Move to the previous note, if possible."
  (interactive nil org-verse-notes-mode)
  (if-let (prev (ewoc-prev org-verse-notes-ewoc (ewoc-locate org-verse-notes-ewoc)))
      (progn
        (goto-char (ewoc-location prev))
        (org-verse--note-highlight))))

(defun org-verse-notes-any-tags ()
  "Show notes with any of the tags in the current note."
  (interactive nil org-verse-notes-mode)
  (org-verse-show-tags-any (org-verse-note-tags (ewoc-data (ewoc-locate org-verse-notes-ewoc)))))

(defun org-verse--show-notes (notes-func tags)
  "Display notes from NOTES-FUNC in buffer, with notes having TAGS."
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((ewoc (ewoc-create #'org-verse-display-note (propertize (org-verse-tags-display tags) 'face 'org-verse-title))))
    (mapc (lambda (note) (ewoc-enter-last ewoc note))
          (funcall notes-func))
    (org-verse-notes-mode)
    (setq-local org-verse-notes-ewoc ewoc
                org-verse-notes-fetch-notes-function notes-func
                org-verse-notes-hl (make-overlay 1 1)
                org-verse-notes-tags tags)
    (overlay-put org-verse-notes-hl 'face hl-line-face)
    ;; Move past the title
    (forward-line 1)
    (org-verse--note-highlight)))

(defun org-verse-show-tags-any (tags)
  "Show notes with any of TAGS."
  (interactive (list (completing-read-multiple "Tags: " (org-verse-tags))))
  (let ((buf (get-buffer-create (format "org-verse tags (any): %s" (mapconcat #'identity tags " ")))))
    (set-buffer buf)
    (org-verse--show-notes (lambda ()
                       (sort 
                        (seq-uniq (mapcan (lambda (tag) (org-verse-get-notes-with-tag tag)) tags))
                        (lambda (a b)
                          ;; Sort more tags over fewer tags
                          (or (> (length (org-verse-note-tags a)) (length (org-verse-note-tags b)))
                              (string< (car (org-verse-note-tags a)) (car (org-verse-note-tags b))))))) tags)
    (switch-to-buffer buf)))

(defun org-verse-show-tags-all (tags)
  "Show notes that contain all TAGS."
  (interactive (list (completing-read-multiple "Tags: " (org-verse-tags))))
  (let ((buf (get-buffer-create (format "org-verse tags (all): %s" (mapconcat #'identity tags " ")))))
    (set-buffer buf)
    (org-verse--show-notes (lambda ()
                       (sort
                        (org-verse-get-notes-with-tags tags)
                        (lambda (a b)
                          ;; Sort more tags over fewer tags
                          (or (> (length (org-verse-note-tags a)) (length (org-verse-note-tags b)))
                              (string< (car (org-verse-note-tags a)) (car (org-verse-note-tags b))))))) tags)
    (switch-to-buffer buf)))

(defun org-verse-show-tag (tag)
  (interactive (list (completing-read "Tag: " (org-verse-tags))))
  (let ((buf (get-buffer-create (format "org-verse tag: %s" tag))))
    (set-buffer buf)
    (org-verse--show-notes (lambda () (org-verse-get-notes-with-tag tag)) (list tag))
    (switch-to-buffer buf)))

(defun org-verse-show-today ()
  (interactive)
  (org-verse-show-tag (car (org-verse-date-tag))))

(defun org-verse-document-titles ()
  "Return an alist of all titles.
The key is the subject and the value is the title."
  (org-verse--connect)
  (mapcan (lambda (sub)
            (mapcar (lambda (title) (cons sub title)) (plist-get (triples-get-type org-verse-db sub 'titled) :title)))
          (seq-filter #'org-verse-has-live-tags-p
                      (triples-subjects-of-type org-verse-db 'titled))))

(defun org-verse-browse-url (title)
  "Browse the url corresponding to TITLE.
If no corresponding URL is found, an error is thrown."
  (interactive (list (completing-read "Doc: "
                                      (mapcan
                                       (lambda (tcons)
                                         (when (ffap-url-p (car tcons))
                                           (list (cdr tcons))))
                                       (org-verse-document-titles)))))
  (let ((subjects (seq-filter #'ffap-url-p (triples-subjects-with-predicate-object org-verse-db 'titled/title title))))
    (when (= 0 (length subjects)) (error "Could not fetch existing URL title: %s" title))
    (when (> (length subjects) 1) (warn "Multiple URLs with the same title exist: %s" title))
    (browse-url (car subjects))))

;; The following is unused, experimental code.

(cl-defstruct org-verse-person
  "Data about a person, stored in ORG-VERSE."
  names emails tags)

(defun org-verse-add-people (people)
  "Add PEOPLE, `org-verse-person' objects to ORG-VERSE.
The subject of a person is the shortest email address they have."
  (org-verse--connect)
  (triples-with-transaction org-verse-db
      (mapc (lambda (person)
              (let* ((sorted-email (sort (org-verse-person-emails person)
                                         (lambda (a b)
                                           (< (length a) (length b)))))
                     (subject (car sorted-email)))
                (triples-set-type org-verse-db subject 'named
                                  :name (org-verse-person-names person))
                (triples-set-type org-verse-db subject 'email
                                  :address sorted-email)
                (triples-set-type org-verse-db subject 'tagged
                                  :tag (org-verse-person-tags person))
                (triples-set-type org-verse-db subject 'person)))
            people)))

(defun org-verse-people ()
  "Return a list of all people as `org-verse-person' structs."
  (org-verse--connect)
  (cl-loop for id in (triples-subjects-of-type org-verse-db 'person)
           collect
           (let ((v (triples-get-subject org-verse-db id)))
             (make-org-verse-person
              :names (plist-get v :name)
              :emails (plist-get v :email/address)
              :tags (plist-get v :tagged/tag)))))

;; Auto-tag functions

(defun org-verse-date-tag ()
  "Get single tag representing the date as a ISO 8601 format."
  (list (format-time-string "%F")))

;; Links for org-mode
(require 'ol)

(defun org-verse--store-any-tags-link ()
  "Store a link to an any-tags org-verse page."
  (when (eq major-mode 'org-verse-notes-mode)
    ;; TODO: Stop assuming every notes mode is an any tags.
    (org-link-store-props :type "org-verse-tags-any" :link (concat "org-verse-tags-any:" (format "%S" org-verse-notes-tags))
                          :description (format "ORG-VERSE page for any of the tags: %s"
                                               (mapconcat #'identity org-verse-notes-tags ", ")))))

(defun org-verse--open-any-tags-link (stags)
  "Open a link to an org-verse page given by TAGS."
  (let ((tags (read stags)))
    (if (= 1 (length tags))
        (org-verse-show-tag (car tags))
      (org-verse-show-tags-any tags))))

(defun org-verse--store-note-link ()
  "Store a link to an individual note."
  (let ((id (when (member 'org-verse-edit-mode local-minor-modes)
              (org-verse-note-id org-verse-note))))
    (when id
      (org-link-store-props :type "org-verse-note"
                            :link (concat "org-verse-note:" (format "%S" id))
                            :description (format "ORG-VERSE note: %S" id)))))

(defun org-verse--open-note-link (id)
  "Open a link to a note given its ID."
  (org-verse-edit (org-verse-get-note-with-id (read id))))

(org-link-set-parameters "org-verse-tags-any" :follow #'org-verse--open-any-tags-link
                         :store #'org-verse--store-link)

(org-link-set-parameters "org-verse-note" :follow #'org-verse--open-note-link
                         :store #'org-verse--store-note-link)

(provide 'org-verse-note)
;;; org-verse-note.el ends here
