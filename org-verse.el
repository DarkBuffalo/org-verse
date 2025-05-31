;;; org-verse.el --- Highlight verses in org-mode -*- lexical-binding: t -*-
;; Author: Matthias David

;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1")(dash)(anaphora)(esqlite)(s)(db)(doct)(magit-section))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2021-2021, Matthias David
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This minor mode provides syntax highlighting of verse in orgmode.
;;
;; To enable: call `org-verse-mode'.
;;
;;; Code:
;;;

(require 'button)
(require 'dash)
(require 'cl-lib)
(require 'anaphora)
(require 's)
(require 'magit-section)
(require 'org-verse-retrieve)
(require 'org)
(require 'org-element)


(defgroup org-verse nil
  "Highlight verse on orgmode file."
  :prefix "org-verse"
  :group 'applications)

;;;; Customs
(defcustom org-verse-directory "~/verses/"
  "Main verse directory."
  :type 'directory
  :group 'org-verse)

(defcustom org-verse-note-filename "Perles.org"
  "Main verse directory."
  :type 'string
  :group 'org-verse)

(defcustom org-verse-note-file
  (expand-file-name org-verse-note-filename org-verse-directory)
  "Principal notes File for save captures."
  :type 'file
  :group 'org-verse)

(defcustom org-verse-file-extension "org"
  "The extension for verse files."
  :type 'string
  :group 'org-verse)


;;;; Faces
(defface org-verse-number-face
  '((t ;;:inherit font-lock-constant-face
     :foreground "#81B145"
     ;;:background "#1B2229"
     :box (:line-width 2)
     :weight bold))
  "Face used to verse references."
  :group 'org-verse)

(defvar org-verse-db-table-name "bible"
  "Table name.")

;;;; Constantes
(defconst *user-home-directory*
  (getenv "HOME")
  "Path to user home directory.")

(defconst org-verse-lexical
  '((org-verse-search-for-verse
     (0 'org-verse-number-face t))))

(defvar org-verse-pattern nil
  "Pattern regex pour la reconnaissance des versets, généré dynamiquement.")

(defun org-verse-search-for-key (regex limit)
  "Match REGEX in buffer until LIMIT. Search is case-insensitive."
  (let (match-data-to-set
        found
        ;;insensitive
        (case-fold-search t))
    (save-match-data
      (while (and (null match-data-to-set)
                  (re-search-forward regex limit t))
        (setq match-data-to-set (match-data))))
    (when match-data-to-set
      (set-match-data match-data-to-set)
      (goto-char (match-end 0))
      t)))

(defun org-verse-search-for-verse (limit)
  "Search a verse with LIMIT."
  (org-verse-search-for-key org-verse-pattern limit))


(defvar org-verse--keywords nil)



;;;; STRUCTURE
(defconst org-verse--canon nil
  "A list of all the book.")

;; TODO LANGUAGE & VERSION
(defcustom org-verse-version 'nwt
  "Version actuelle de la Bible."
  :type 'symbol
  :group 'org-verse)

(defcustom org-verse-language 'fr
  "Langue actuelle de la Bible."
  :type 'symbol
  :group 'org-verse)

(defcustom org-verse-available-versions
  '((nwt . "Traduction du Monde Nouveau")
    (lsg . "Louis Segond")
    (niv . "New International Version")
    (kjv . "King James Version"))
  "Versions disponibles de la Bible."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-verse)

(defcustom org-verse-available-languages
  '((fr . "Français")
    (en . "English")
    (es . "Español"))
  "Langues disponibles."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-verse)


(defun org-verse-load-canon ()
  "Charge le fichier de canon pour la version et la langue définies."
  (let* ((filename (format "canon/%s-%s.el"
                           (symbol-name org-verse-version)
                           (symbol-name org-verse-language)))
         (fullpath (expand-file-name filename
                                     (file-name-directory
                                      (or load-file-name
                                          (buffer-file-name)
                                          default-directory)))))
    ;; Vider le canon actuel avant de charger le nouveau
    (setq org-verse--canon nil)

    (if (file-exists-p fullpath)
        (progn
          (load-file fullpath)
          ;; Reconstruire le pattern après le chargement
          (setq org-verse-pattern (org-verse-build-pattern))
          (message "Canon chargé : %s-%s (%d livres)"
                   org-verse-version
                   org-verse-language
                   (length org-verse--canon)))


      (error "Fichier canon introuvable pour la version `%s` et la langue `%s`"
             org-verse-version org-verse-language))))




;; STRUCTURES
(cl-defstruct org-verse--book
  id        ; symbole unique (ex: 'gen, 'exo, etc.)
  pattern   ; regex pour la reconnaissance
  abbrev    ; abréviation courte
  name      ; nom complet dans la langue
  number    ; numéro du livre (1-66)
  verses)   ; vecteur avec le nombre de versets par chapitre

(defun org-verse-add-book (id pattern abbrev name number verses)
  "Ajoute un livre au canon avec ID, PATTERN, ABBREV, NAME, NUMBER et VERSES."
  (let ((new-book (make-org-verse--book
                   :id id
                   :pattern pattern
                   :abbrev abbrev
                   :name name
                   :number number
                   :verses verses)))
    ;; Remplacer par ID au lieu du nom pour éviter les conflits
    (aif (cl-position id org-verse--canon
                      :test #'eq
                      :key #'org-verse--book-id)
        (setf (nth it org-verse--canon) new-book)
      (push new-book org-verse--canon))))


(defun org-verse-build-pattern ()
  "Construit dynamiquement le pattern regex basé sur le canon actuel."
  (when org-verse--canon
    (let* ((book-regexps
            (mapcar (lambda (book)
                      (let ((pat (org-verse--book-pattern book)))
                        (cond
                         ((and (listp pat) (eq (car pat) 'rx))
                          (rx-to-string (cadr pat)))
                         ((stringp pat) pat)
                         (t (error "Pattern invalide pour %s"
                                   (org-verse--book-name book))))))
                    org-verse--canon))
           (books-pattern (concat "\\("
                                  (mapconcat 'identity book-regexps "\\|")
                                  "\\)"))
           ;; Pattern amélioré pour gérer les références multi-chapitres
           (full-pattern (concat books-pattern
                                 "\\s-+"
                                 "\\([0-9]+\\)"  ; Chapitre
                                 ":"
                                 "\\([0-9]+\\)"  ; Verset de début
                                 "\\(?:"         ; Groupe optionnel pour la suite
                                 "-"
                                 "\\(?:"
                                 "\\([0-9]+\\):" ; Chapitre de fin (optionnel)
                                 "\\)?"
                                 "\\([0-9]+\\)"  ; Verset de fin
                                 "\\)?")))
      full-pattern)))


(defun org-verse-select-version ()
  "Changer la version et la langue de la Bible et recharger le canon."
  (interactive)
  (let* ((version (org-verse-completing-read
                   "Version : "
                   org-verse-available-versions))
         (lang (org-verse-completing-read
                "Langue : "
                org-verse-available-languages)))
    (setq org-verse-version version)
    (setq org-verse-language lang)
    (org-verse-load-canon)
    ;; Rafraîchir le buffer si nécessaire
    (when org-verse-mode
      (org-verse-scan-entire-buffer))))


;;;; HELPER
(defun org-verse-expand-book-name (book)
  "Retourne le nom complet du livre à partir de son nom ou abréviation BOOK."
  (let ((book-struct (seq-find (lambda (b)
                                 (string-match-p (org-verse--book-pattern b) book))
                               org-verse--canon)))
    (if book-struct
        (org-verse--book-name book-struct)
      book))) ; Retourne l'original si pas trouvé

(defun org-verse-format-reference (book chapter verses)
  "Formate une référence biblique avec le nom complet du livre."
  (format "%s %s:%s"
          (org-verse-expand-book-name book)
          chapter
          verses))


;;;; ZK

(defvar org-verse-history nil)

(defcustom org-verse-select-file-function #'org-verse--select-file
  "Function for performing completing read.
Must take an optional prompt and a list of files"
  :type 'function)


;; WIN grep -irl "Matt 6:26" C:/Users/matth/Dropbox/Org/notes --include=*.org
(defun org-verse--grep-file-list (str)
  "Return a list of files containing regexp STR."
  (let* ((files (if (eq system-type 'windows-nt)
                    (shell-command-to-string (concat
                                              "grep -irl "
                                              (shell-quote-argument str)
                                              " "
                                              (file-truename org-verse-directory)
                                              " --include=*."
                                              org-verse-file-extension))
                  (shell-command-to-string (concat
                                            "grep -lir --include \\*."
                                            org-verse-file-extension
                                            " -e "
                                            (shell-quote-argument str)
                                            " "
                                            (expand-file-name org-verse-directory)
                                            " 2>/dev/null")))))
    (split-string files "\n" t)))


(defun org-verse--select-file (&optional prompt list)
  "Wrapper around `completing-read' to select org-verse-file.
Offers candidates from `org-verse-directory', or from LIST when
supplied. Can take a PROMPT argument."
  (let* ((files (if list list
                  ;;(org-verse--directory-files t)
                  (directory-files org-verse-directory t (concat "\\." org-verse-file-extension "$")))))
    (completing-read
     (if prompt prompt
       "Select File: ")
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (group-function . org-verse--group-function)
             (category . org-verse-file))
         (complete-with-action action files string predicate)))
     nil t nil 'org-verse-history)))

(defun org-verse--group-function (file transform)
  "TRANSFORM completion candidate FILE to note title."
  (if transform
      (progn
        (string-match (concat "\\(?1:.*?\\)\\."
                              org-verse-file-extension
                              ".*")
                      file)
        (match-string 1 file))
    "file with your search org verse"))


;;;###autoload
(defun org-verse-find-file-by-full-text-search (str)
  "Find files containing regexp STR."
  (interactive
   (list (read-string "Search string: ")))
  (let ((files (org-verse--grep-file-list str)))
    (if files
        (find-file (funcall org-verse-select-file-function
                            (format "Files containing \"%s\": " str) files))
      (user-error "No results for \"%s\"" str))))


;;;; Backlinks

(defun org-verse--truncate-headline (text &optional max-length)
  "Tronque TEXT  MAX-LENGTH caractres (par dfaut 80).
Ajoute   la fin si tronqué. Ne fait rien si TEXT est nil."
  (let ((max (or max-length 80)))
    (if (and text (> (length text) max))
        (concat (substring text 0 (- max 1)) "\u2026")
      text)))

(defun org-verse--build-backlink-data (files verse-key)
  "Retourne une liste d'alists contenant :file et :position pour chaque fichier contenant VERSE-KEY."
  (let ((regexp (regexp-quote verse-key)))
    (delq nil
          (mapcar (lambda (f)
                    (let ((pos (org-verse--find-position-in-file f regexp)))
                      (when pos
                        `(:file ,f :position ,pos))))
                  files))))

(defun org-verse--find-position-in-file (file regexp)
  "Retourne la position du REGEXP dans FILE, ou nil si non trouvé."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward regexp nil t)
          (match-beginning 0))))))

(defun org-verse--get-headline-at-pos (file pos)
  "Retourne le titre Org (headline) à la position POS dans le fichier FILE.
Si aucun titre n'est trouvé  cette position, retourne nil."
  (when (and (stringp file)
             (file-readable-p file)
             (number-or-marker-p pos))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (min pos (point-max))) ;; scurise si pos > fichier
      (require 'org-element)
      (let ((element (org-element-at-point)))
        ;; remonter jusqu'à un headline si nécessaire
        (while (and element
                    (not (eq (org-element-type element) 'headline)))
          (setq element (org-element-property :parent element)))
        (when (and element (eq (org-element-type element) 'headline))
          (let ((title (org-element-property :raw-value element)))
            (when (stringp title)
              title)))))))


(defun org-verse--get-org-title (file)
  "Retourne le contenu de #+TITLE: dans FILE, ou nil si absent."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" nil t)
        (string-trim (match-string 1))))))


(defvar org-verse-link-button-action #'find-file-other-window
  "Action for `org-verse-link--find-file'.")


(defun org-verse-link--find-file (button)
  "Ouvre le fichier à la position donne dans une autre fenètre depuis BUTTON.
Déplie le contexte Org si nécessaire."
  (let ((file (button-get button :file))
        (pos (button-get button :position)))
    (when (and file (file-exists-p file))
      (let ((win (display-buffer (find-file-noselect file))))
        (with-selected-window win
          (goto-char (min (or pos (point-min)) (point-max)))
          (when (derived-mode-p 'org-mode)
            ;; Déplier et afficher la section au bon endroit
            (org-fold-show-entry)
            (org-fold-show-subtree)
            (org-reveal)))))))



(define-button-type 'org-verse-link-backlink-button
  'follow-link t
  'action #'org-verse-link--find-file
  'face 'unspecified)

(defun org-verse--prepare-backlinks (files)
  "Prepare backlinks' including FILES.
Use optional TITLE for a prettier heading."
  (magit-insert-section (backlinks)
                        (magit-insert-heading
                         (insert (propertize (format "%s\n" "Backlinks") 'face 'magit-section-heading)))
                        (magit-insert-section-body
                          (mapc (lambda (f)
                                  (let* ((file (plist-get f :file))
                                         (pos (plist-get f :position))
                                         (name (file-name-nondirectory file))
                                         (title (org-verse--get-org-title file))
                                         (headline (org-verse--get-headline-at-pos file pos))
                                         (label (org-verse--truncate-headline (or headline title name))))
                                    (insert (propertize label 'face 'italic))
                                    (make-button (pos-bol) (pos-eol)
                                                 :type 'org-verse-link-backlink-button
                                                 :file file
                                                 :position pos
                                                 :help-echo name)
                                    (newline)))
                                files))))


(defun org-verse--no-backlinks ()
  "No backlinks'."
  (magit-insert-section (backlinks)
    (magit-insert-heading
      (insert (propertize (format "%s\n" "Backlinks") 'face 'magit-section-heading)))
    (magit-insert-section-body
      (insert "Pas de références de ce verset."))))

(defun org-verse--prepare-notes ()
  "Prepare notes' including links."
  (with-temp-buffer
    (insert-file-contents org-verse-note-file)
    ;; [[https://stackoverflow.com/questions/17208842/how-to-parse-org-mode-by-org-element-elegantly#17310307][parse headings]]
    (let* ((parsetree (org-element-parse-buffer 'headline))
           (heads (org-element-map parsetree 'headline
                    (lambda (hl) (org-element-property :title hl)))))))

  (magit-insert-section (notes)
                        (magit-insert-heading
                         (insert (propertize (format "%s" "Notes") 'face 'magit-section-heading)))
                        (magit-insert-section-body (insert (format "%s" "Notes")))))


;;;; Completion
(defun org-verse-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Call `completing-read' but return the value.
from PROMPT and COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT
HIST DEF INHERIT-INPUT-METHOD.
Simple wrapper around the `completing-read' function that assumes
the collection is either an alist, or a hash-table, and returns
the _value_ of the choice, not the selected choice. For instance,
give a variable of choices like:

    (defvar favorite-hosts '((\"Glamdring\" . \"192.168.5.12\")
                             (\"Orcrist\"   . \"192.168.5.10\")
                             (\"Sting\"     . \"192.168.5.220\")
                             (\"Gungnir\"   . \"192.168.5.25\")))

We can use this function to `interactive' without needing to call
`alist-get' afterwards:

    (defun favorite-ssh (hostname)
      \"Start a SSH session to a given HOSTNAME.\"
      (interactive (list (alt-completing-read \"Host: \" favorite-hosts)))
      (message \"Rockin and rollin to %s\" hostname))"

  ;; Yes, Emacs really should have an `alistp' predicate to make this code more readable:
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))

    (let* ((choice
            (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection def nil 'equal))
                     (t                         choice))))
      (if (listp results) (cl-first results) results))))

(defun org-verse-completing-read-by (fn prompt list &rest args)
  "Apply FN to each element of LIST and prompt the user to select a resulting value.
The output of the function will be the corresponding element of LIST
ARGS will be passed to `completing-read' after the PROMPT and COLLECTION arguments."
  (let ((hash (make-hash-table :test 'equal)))
    (mapc (lambda (elem) (puthash (funcall fn elem) elem hash)) list)
    (gethash (apply 'completing-read prompt hash args) hash)))

(defun org-verse-complete ()
  "Choose verse with completion."
  (let* ((book (org-verse-completing-read-by 'org-verse--book-name "Livre: " org-verse--canon))
         (chapter (string-to-number (org-verse-completing-read "Chapitre: "
                                                               (seq-map 'int-to-string
                                                                        (mapcar 'car  (org-verse--book-verses book))))))
         (verses (org-verse-completing-read "Verset: "
                                            (seq-map 'int-to-string
                                                     (mapcan 'list
                                                             (cl-loop for i from 1 to (cdr (assoc chapter (org-verse--book-verses book)))
                                                                      collect i))))))
    (format "%s %s:%s" (org-verse--book-abbrev book) chapter verses)))

;;;; DATABASE SQLITE

(defcustom org-verse--db-file-name "bible.db"
  "Name of the db containing all the verses."
  :type 'file
  :group 'verse)

(defun org-verse--get-db-url ()
  "Get the org-verse database URL."
  (expand-file-name org-verse--db-file-name *user-home-directory*))

(defvar org-verse--db nil
  "The org-verse database.")

(defun org-verse--get-db ()
  "Get the org-verse database."
  (unless org-verse--db
    (setq org-verse--db (sqlite-open (org-verse--get-db-url))))
  org-verse--db)

(defun org-verse--reset-db-connection ()
  "Reset the org-verse database connection."
  (when org-verse--db
    (sqlite-close org-verse--db)
    (setq org-verse--db nil)))

(defun org-verse--db-read (query)
  "Call the QUERY on the database and return the result."
  (sqlite-select (org-verse--get-db) query))


(defun org-verse-analyse-verses (book chapter verses)
  "Analyse VERSES with BOOK and CHAPTER."
  (let ((verses! nil)
        (ref nil))
    (cond
     ;; Cas: référence multi-chapitres (ex: "1-7:29")
     ((string-match "^\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)$" verses)
      (let* ((start-verse (string-to-number (match-string 1 verses)))
             (end-chapter (string-to-number (match-string 2 verses)))
             (end-verse (string-to-number (match-string 3 verses)))
             (start-chapter (string-to-number chapter))
             (all-verses '())
             ;; Trouver le livre une seule fois
             (book-struct (seq-find (lambda (b)
                                      (string-match-p (org-verse--book-pattern b) book))
                                    org-verse--canon)))

        (if (not book-struct)
            (setq verses! (format "[Livre %s non trouvé]" book))

          ;; Vérifier que les chapitres existent
          (let ((max-chapters (length (org-verse--book-verses book-struct))))
            (if (or (> start-chapter max-chapters)
                    (> end-chapter max-chapters))
                (setq verses! (format "[Chapitre invalide: %s n'a que %d chapitres]"
                                      (org-verse--book-name book-struct)
                                      max-chapters))

              ;; Récupérer tous les versets du chapitre de début
              (let ((verses-in-chapter (aref (org-verse--book-verses book-struct)
                                             (1- start-chapter))))
                (setq ref (org-verse-serialize book start-chapter start-verse nil verses-in-chapter))
                (when ref
                  (push (org-verse-db-get-multiverse
                         (plist-get ref :book)
                         start-chapter
                         start-verse
                         verses-in-chapter)
                        all-verses)))

              ;; Récupérer tous les chapitres intermédiaires complets
              (cl-loop for chap from (1+ start-chapter) to (1- end-chapter)
                       when (<= chap max-chapters)
                       do (let ((verses-count (aref (org-verse--book-verses book-struct)
                                                     (1- chap))))
                            (setq ref (org-verse-serialize book chap 1 nil verses-count))
                            (when ref
                              (push (org-verse-db-get-multiverse
                                     (plist-get ref :book)
                                     chap
                                     1
                                     verses-count)
                                    all-verses))))

              ;; Récupérer les versets du chapitre de fin
              (when (<= end-chapter max-chapters)
                (setq ref (org-verse-serialize book end-chapter 1 nil end-verse))
                (when ref
                  (push (org-verse-db-get-multiverse
                         (plist-get ref :book)
                         end-chapter
                         1
                         end-verse)
                        all-verses)))

              (setq verses! (mapconcat 'identity (reverse all-verses) "\n")))))))

     ;; Cas: versets multiples avec virgules (ex: "1,3,5" ou "1,3-5,7")
     ((string-match-p "," verses)
      (let ((parts (split-string verses ","))
            (all-verses '()))
        (dolist (part parts)
          (cond
           ;; Plage de versets dans une partie (ex: "3-5")
           ((string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" part)
            (let ((start (string-to-number (match-string 1 part)))
                  (end (string-to-number (match-string 2 part))))
              (setq ref (org-verse-serialize book chapter start nil end))
              (when ref
                (push (org-verse-db-get-multiverse
                       (plist-get ref :book)
                       (plist-get ref :chapter)
                       start end)
                      all-verses))))
           ;; Verset unique dans une partie (ex: "7")
           (t
            (let ((v (string-to-number part)))
              (setq ref (org-verse-serialize book chapter v))
              (when ref
                (push (org-verse-db-get-verse
                       (plist-get ref :book)
                       (plist-get ref :chapter)
                       v)
                      all-verses))))))
        (setq verses! (mapconcat 'identity (reverse all-verses) " "))))

     ;; Cas: plage simple dans le même chapitre (ex: "3-5")
     ((string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" verses)
      (let ((start (string-to-number (match-string 1 verses)))
            (end (string-to-number (match-string 2 verses))))
        (setq ref (org-verse-serialize book chapter start nil end))
        (when ref
          (setq verses! (org-verse-db-get-multiverse
                         (plist-get ref :book)
                         (plist-get ref :chapter)
                         start end)))))

     ;; Cas: verset unique (ex: "7")
     (t
      (let ((v (string-to-number verses)))
        (setq ref (org-verse-serialize book chapter v))
        (when ref
          (setq verses! (org-verse-db-get-verse
                         (plist-get ref :book)
                         (plist-get ref :chapter)
                         v))))))

    ;; S'assurer qu'on retourne toujours quelque chose
    (or verses!
        (format "[Versets %s %s:%s non trouvés]"
                book chapter verses))))


(defun org-verse-serialize (livre chapter verse &optional endchapter endverse)
  "Serialize reference from LIVRE, CHAPTER, VERSE, ENDCHAPTER and ENDVERSE."
  (let ((ref nil))
    (cl-loop for x in org-verse--canon
             when (string-match-p (org-verse--book-pattern x) livre)
             do (progn
                  (message "Livre trouvé: %s -> %s (numéro %d)"
                           livre
                           (org-verse--book-name x)
                           (org-verse--book-number x))
                  (setq ref (list :book (org-verse--book-number x)
                                  :chapter chapter
                                  :verse verse
                                  :endchapter endchapter
                                  :endverse endverse))
                  (cl-return)))  ; Sortir dès qu'on trouve le livre
    ref))

(defun org-verse-db-get-verse (book chapter verse)
  "Retrieve the ref for BOOK, CHAPTER and VERSE."
  (aif
      (org-verse--db-read
       (format "SELECT verse FROM \"%s\" WHERE book=%s AND chapter=%s AND nbverse=%s"
               org-verse-db-table-name book chapter verse))
      (car (car it))))

(defun org-verse-db-get-multiverse (book chapter verse endverse)
  "Get verses from BOOK CHAPTER VERSE and ENDVERSE."
  (aif
      (org-verse--db-read
       (format "SELECT verse FROM \"%s\" WHERE book=%s AND chapter=%s AND nbverse BETWEEN %s AND %s "
               org-verse-db-table-name book chapter verse endverse ))
      (mapconcat 'identity (apply 'append it) " ")))

;;;; SIDEBAR BUFFER VERSE
;; https://github.com/Kinneyzhang/gkroam/blob/b40555f45a844b8fefc419cd43dc9bf63205a0b4/gkroam.el#L1359
(defvar org-verse-buffer "*Verse*"
  "Verse Buffer name.")

;; inspiration:
;; https://github.com/chenyanming/wallabag.el/blob/master/wallabag.el
;; https://github.com/huytd/org-journal-list/blob/master/org-journal-list.el

(defcustom org-verse-sidebar-select-window nil
  "If non-nil, switch to verse sidebar upon displaying it."
  :type 'boolean
  :safe 'booleanp
  :group 'org-verse-sidebar)

(defcustom org-verse-sidebar-persistent-window t
  "When non-nil, sidebar will persist when calling `delete-other-windows'.
This marks `no-delete-other-windows' window parameter as non-nil.
Use `org-verse-toggle-sidebar' or `quit-window' to close the sidebar."
  :type 'boolean
  :safe 'booleanp
  :group 'org-verse-sidebar)

(defcustom org-verse-sidebar-display-alist
  '((side . right)
    (window-width . 30)
    (slot . -1))
  "Association list used to display verse sidebar buffer.
See `display-buffer-in-side-window' for example options."
  :type 'alist
  :safe (lambda (value)
          (and (listp value)
               (seq-every-p 'consp value)))
  :group 'org-verse-sidebar)

(defun org-verse-sidebar-create-buffer ()
  "Return verse sidebar buffer."
  (with-current-buffer (get-buffer-create org-verse-buffer)
    (local-set-key (kbd "q") 'org-verse-sidebar-quit)
    ;;(local-set-key (kbd "<tab>") 'magit-section-toggle)
    (local-set-key (kbd "a") 'org-verse-capture)
    (local-set-key (kbd "n") 'org-verse-note-capture)
    (current-buffer)))

(defun org-verse-sidebar-create-window ()
  "Return verse sidebar window."
  (let ((display-buffer-mark-dedicated t))
    (display-buffer-in-side-window
     (org-verse-sidebar-create-buffer)
     (append org-verse-sidebar-display-alist
             (when org-verse-sidebar-persistent-window
               (list '(window-parameters (no-delete-other-windows . t))))))))

;;;###autoload
(defun org-verse-toggle-sidebar ()
  "Toggle visibility of verse sidebar window."
  (interactive)
  (if (window-live-p (get-buffer-window org-verse-buffer))
      (delete-window (get-buffer-window org-verse-buffer))
    (org-verse-sidebar-create-window)
    (when org-verse-sidebar-select-window
      (select-window (get-buffer-window org-verse-buffer)))))

(defvar-local org-verse-current-verse
    nil
  "Current verse open on sidebar for `org-capture'.")

(defun org-verse-sidebar-refresh (refverse book chapter verses)
  "Refresh the sidebar with REFVERSE, BOOK, CHAPTER and VERSES."
  (with-current-buffer org-verse-buffer
    (with-silent-modifications
      (setq header-line-format (list :propertize "VERSET" 'face 'bold))
      (erase-buffer)
      (visual-line-mode 1)

      (let ((reference (org-verse-analyse-verses book chapter verses))
            (inhibit-read-only t))
        (setq org-verse-current-verse refverse) ; for org-capture
        (magit-insert-section (root)
          (magit-insert-section (verse)
            ;; utilisation du helper pour afficher le nom du livres plein dans la sidebar
            (magit-insert-heading
              (insert (propertize (org-verse-format-reference book chapter verses)
                                  'face 'magit-section-heading))
              (insert "\n"))
            ;; insertion du verset
            (magit-insert-section-body (insert reference)))
          (insert "\n\n")
          ;; Lister les fichiers avec les position de la référence
          (let* ((files (org-verse--grep-file-list refverse))
                 (backlinks (org-verse--build-backlink-data files refverse)))
            (if backlinks
                (org-verse--prepare-backlinks backlinks)
              (org-verse--no-backlinks)))


          (insert "\n")
          (org-verse--prepare-notes)

          (insert "\n"))

        (read-only-mode)))
    (goto-char (point-min))))

(defun org-verse-sidebar-quit ()
  "Quit verse buffer."
  (interactive)
  (if (window-live-p (get-buffer-window org-verse-buffer))
      (delete-window (get-buffer-window org-verse-buffer))))

;;;; BUTTONIZE BUFFER
(defun org-verse-button-verse (button)
  "BUTTON verse."
  (let ((refverse (button-get button 'title))
        (book (button-get button 'book))
        (chapter (button-get button 'chapter))
        (verses (button-get button 'verses)))

    ;; si buffer pas ouvert, ouvrir sinon mettre a jour
    (if (window-live-p (get-buffer-window org-verse-buffer))
        (org-verse-sidebar-refresh refverse book chapter verses)
      (org-verse-toggle-sidebar)
      (org-verse-sidebar-refresh refverse book chapter verses))))

(define-button-type 'org-verse-button
  'action #'org-verse-button-verse
  'follow-link t
  'face 'org-verse-number-face
  'help-echo "Clic le boutton pour lire le verset.")


;; recuperer le contenu de la recherche pour le mettre en titre
;; https://github.com/Kinneyzhang/gkroam/blob/b40555f45a844b8fefc419cd43dc9bf63205a0b4/gkroam.el#L708
(defun org-verse-make-button (start end)
  "Créer un bouton entre START et END avec une action personnalisée."
  (let* ((full-match (match-string-no-properties 0))
         (book (match-string-no-properties 1))
         (chapter (match-string-no-properties 2))
         (start-verse (match-string-no-properties 3))
         (end-chapter (match-string-no-properties 4))
         (end-verse (match-string-no-properties 5))
         ;; Construire la chaîne des versets
         (verses (if end-chapter
                     (format "%s-%s:%s" start-verse end-chapter end-verse)
                   (if end-verse
                       (format "%s-%s" start-verse end-verse)
                     start-verse))))
    (make-text-button (match-beginning 0)
                      (match-end 0)
                      :type 'org-verse-button
                      'title full-match
                      'book book
                      'chapter chapter
                      'verses verses)))


(defun org-verse-scan-buffer ()
  "Parcourt le buffer courant et transforme chaque `verset' en bouton."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-verse-pattern nil t)
      (org-verse-make-button (match-beginning 0) (match-end 0)))))


(defun org-verse-remove-buttons (beg end)
  "Supprime les boutons texte dans la région BEG à END."
  (remove-text-properties beg end '(button nil)))

(defun org-verse-scan-region (beg end)
  "Parcourt la rgion BEG à END et transforme `bonjour' en boutons."
  (unless (stringp org-verse-pattern)
    (error "org-verse-pattern is not initialized")) ;; debug

  (save-excursion
    (goto-char beg)
    (while (re-search-forward org-verse-pattern end t)
      ;; Supprimer les anciens boutons sil y en a
      (org-verse-remove-buttons (match-beginning 0) (match-end 0))
      (org-verse-make-button (match-beginning 0) (match-end 0)))))

(defun org-verse-after-change (beg end _len)
  "Met  jour les boutons dans les lignes modifiées."
  (save-excursion
    (goto-char beg)
    (let ((line-start (line-beginning-position))
          (line-end (progn (goto-char end) (line-end-position))))
      (org-verse-remove-buttons line-start line-end)
      (org-verse-scan-region line-start line-end))))

(defun org-verse-scan-entire-buffer ()
  "Re-scanne tout le buffer pour mettre à jour les boutons."
  (interactive)
  (org-verse-remove-buttons (point-min) (point-max))
  (org-verse-scan-region (point-min) (point-max)))


(defconst org-verse-mode-map (make-keymap))
(define-key org-verse-mode-map (kbd "C-c /") #'org-verse-scan-entire-buffer)

;;;###autoload
(define-minor-mode org-verse-mode
  "Highlight bible verses in org buffer."
  ;;:init-value nil
  :lighter " verse"
  :keymap org-verse-mode-map
  (if org-verse-mode
      (progn
        ;; Actions à effectuer lors de l'activation du mode
        ;; (unless (stringp org-verse-pattern)
        ;;   (setq org-verse-pattern (org-verse-build-pattern)))

        (org-verse-scan-entire-buffer)
        (setq org-verse-note-db (org-verse-note-db-connect (org-verse-note-db-file)))
        (add-hook 'after-change-functions #'org-verse-after-change nil t)
        (add-hook 'before-save-hook #'org-verse-scan-entire-buffer nil t))
    ;; Actions à effectuer lors de la désactivation du mode
   (progn
     (org-verse-remove-buttons (point-min) (point-max))
     (remove-hook 'after-change-functions #'org-verse-after-change t)
     (remove-hook 'before-save-hook #'org-verse-scan-entire-buffer t)
     (message "Le mode mineur `Org Verse' est désactivé!"))))



;;; EXPORT
;; (defun org-verse--find-book (name)
;;   "Trouve un livre dans `org-verse--canon` à partir du nom `name`, en testant les patterns."
;;   (seq-find (lambda (book)
;;               (when-let ((pat (org-verse--book-pattern book)))
;;                 (string-match (eval pat) name)))
;;             org-verse--canon))

;; (defun org-verse--jwlink (book-num chapter verse)
;;   (format "%02d%03d%03d" book-num chapter verse))

;; (defun org-verse--parse-verses (book-num chapter raw)
;;   "Analyse les versets (ex: '3-5,7') et retourne une liste de liens ou plages."
;;   (let ((parts (split-string raw ",")))
;;     (mapcar
;;      (lambda (part)
;;        (if (string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" part)
;;            (let ((start (string-to-number (match-string 1 part)))
;;                  (end (string-to-number (match-string 2 part))))
;;              (format "jwlibrary:///finder?srcid=jwlshare&wtlocale=F&prefer=lang&pub=nwtsty&bible=%s-%s"
;;                      (org-verse--jwlink book-num chapter start)
;;                      (org-verse--jwlink book-num chapter end)))
;;          ;; Verset simple
;;          (let ((v (string-to-number part)))
;;            (format "jwlibrary:///finder?srcid=jwlshare&wtlocale=F&prefer=lang&pub=nwtsty&bible=%s"
;;                    (org-verse--jwlink book-num chapter v)))))
;;      parts)))

;; (defun org-verse--export-replace-bible-refs (text backend _info)
;;   "Remplace les références bibliques par des liens jwlibrary dans l'export."
;;   (when (org-export-derived-backend-p backend 'latex 'html 'ascii)
;;     (replace-regexp-in-string
;;      org-verse-pattern
;;      (lambda (match)
;;        (let* ((book-name (match-string 1 match))
;;               (chapter-str (match-string 2 match))
;;               (verses-str (match-string 3 match))
;;               (book (org-verse--find-book book-name))
;;               (book-num (org-verse--book-number book))
;;               (chapter (string-to-number chapter-str))
;;               (links (org-verse--parse-verses book-num chapter verses-str)))
;;          ;; Affiche un lien cliquable pour chaque verset/plage
;;          (string-join
;;           (mapcar (lambda (link)
;;                     (format "[[%s][%s %s:%s]]"
;;                             link book-name chapter-str verses-str))
;;                   links)
;;           " ")))
;;      text)))

;; (add-to-list 'org-export-filter-plain-text-functions
;;              #'org-verse--export-replace-bible-refs)



;;package bootstrap
(provide 'org-verse)

(cl-eval-when (load eval)
  (require 'org-verse-capture)
  (require 'org-verse-db)
  ;;(require 'org-verse-retrieve)
  (require 'org-verse-note)

  ;; Charger le canon par défaut
  (when (and (boundp 'org-verse-version)
             (boundp 'org-verse-language))
    (condition-case err
        (org-verse-load-canon)
      (error (message "Impossible de charger le canon par défaut : %s" err))))

  )


;;; org-verse.el ends here
