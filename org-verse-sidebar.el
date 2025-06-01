;;; org-verse-sidebar.el --- Sidebar functionality for org-verse -*- lexical-binding: t -*-

;; Copyright (c) 2021-2024, Matthias David
;; Author: Matthias David
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1") (magit-section) (transient))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; This module provides the sidebar functionality for org-verse,
;; including display, navigation, and interaction with Bible verses.

;;; Code:

(require 'magit-section)
(require 'transient)

;;;; Variables

(defvar org-verse-buffer "*Verse*"
  "Verse Buffer name.")

(defvar-local org-verse-current-verse nil
  "Current verse open on sidebar for `org-capture'.")

(defvar org-verse-sidebar-history nil
  "Historique de navigation dans la sidebar.")

(defvar org-verse-sidebar-history-index -1
  "Position actuelle dans l'historique.")

;;;; Customization

(defgroup org-verse-sidebar nil
  "Sidebar settings for org-verse."
  :group 'org-verse)

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

;;;; Mode definition

(defvar org-verse-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'org-verse-sidebar-quit)
    (define-key map (kbd "Q") #'kill-this-buffer)
    (define-key map (kbd "a") #'org-verse-capture)
    (define-key map (kbd "n") #'org-verse-note-capture)
    (define-key map (kbd "RET") #'org-verse-sidebar-follow-link)
    (define-key map (kbd "TAB") #'magit-section-toggle)
    (define-key map (kbd "<tab>") #'magit-section-toggle)
    (define-key map (kbd "p") #'org-verse-sidebar-previous-verse)
    (define-key map (kbd "N") #'org-verse-sidebar-next-verse)
    (define-key map (kbd "c") #'org-verse-sidebar-copy-verse)
    (define-key map (kbd "s") #'org-verse-sidebar-search)
    (define-key map (kbd "r") #'org-verse-sidebar-refresh-current)
    (define-key map (kbd "?") #'org-verse-sidebar-help)
    (define-key map (kbd "h") #'org-verse-sidebar-help)
    (define-key map (kbd "x") #'org-verse-sidebar-actions)
    map)
  "Keymap for `org-verse-sidebar-mode'.")

(define-derived-mode org-verse-sidebar-mode special-mode "Verse"
  "Major mode for the org-verse sidebar.

\\{org-verse-sidebar-mode-map}"
  :group 'org-verse
  (setq truncate-lines nil)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq-local cursor-type nil)
  (visual-line-mode 1)
  (hl-line-mode 1))

;;;; Core functions

(defun org-verse-sidebar-create-buffer ()
  "Return verse sidebar buffer."
  (with-current-buffer (get-buffer-create org-verse-buffer)
    (unless (eq major-mode 'org-verse-sidebar-mode)
      (org-verse-sidebar-mode))
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

(defun org-verse-sidebar-quit ()
  "Quit verse buffer."
  (interactive)
  (if (window-live-p (get-buffer-window org-verse-buffer))
      (delete-window (get-buffer-window org-verse-buffer))))

;;;; Display functions

(defun org-verse-sidebar-refresh (refverse book chapter verses)
  "Refresh the sidebar with REFVERSE, BOOK, CHAPTER and VERSES."
  ;; Ajouter à l'historique
  (org-verse-sidebar-push-history refverse)

  (with-current-buffer (get-buffer-create org-verse-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)

      ;; Header avec navigation
      (setq header-line-format
            (list (propertize " ◄ "
                            'face (if (< org-verse-sidebar-history-index
                                         (1- (length org-verse-sidebar-history)))
                                      'bold
                                    'shadow)
                            'help-echo "Verset précédent (p)")
                  (propertize " ► "
                            'face (if (> org-verse-sidebar-history-index 0)
                                      'bold
                                    'shadow)
                            'help-echo "Verset suivant (N)")
                  " "
                  (propertize "VERSET" 'face 'bold)))

      ;; Contenu principal
      (let* ((reference (condition-case nil
                            (org-verse-analyse-verses book chapter verses)
                          (error nil)))
             (book-struct (seq-find (lambda (b)
                                      (string-match-p (org-verse--book-pattern b) book))
                                    org-verse--canon))
             (full-book-name (if book-struct
                                 (org-verse--book-name book-struct)
                               book)))

        (setq org-verse-current-verse refverse)

        (magit-insert-section (root)
          (magit-insert-section (verse)
            (magit-insert-heading
              (insert (propertize (format "%s %s:%s\n"
                                          full-book-name
                                          chapter
                                          verses)
                                  'face 'magit-section-heading)))
            (magit-insert-section-body
              (insert (or reference
                          (propertize "[Verset non trouvé dans la base de données]"
                                      'face 'font-lock-warning-face)))))

          (insert "\n\n")

          ;; Backlinks
          (condition-case nil
              (let* ((files (org-verse--grep-file-list refverse))
                     (backlinks (org-verse--build-backlink-data files refverse)))
                (if backlinks
                    (org-verse--prepare-backlinks backlinks)
                  (org-verse--no-backlinks)))
            (error (insert (propertize "Erreur lors du chargement des backlinks\n"
                                       'face 'font-lock-warning-face))))

          (insert "\n")

          ;; Notes
          (condition-case nil
              (org-verse--prepare-notes)
            (error (insert (propertize "Erreur lors du chargement des notes\n"
                                       'face 'font-lock-warning-face))))

          (insert "\n"))))

    (goto-char (point-min))))

;;;; History navigation

(defun org-verse-sidebar-push-history (verse)
  "Ajoute VERSE à l'historique."
  (when verse
    ;; Supprimer les éléments après la position actuelle
    (when (>= org-verse-sidebar-history-index 0)
      (setq org-verse-sidebar-history
            (seq-take org-verse-sidebar-history
                      (1+ org-verse-sidebar-history-index))))
    ;; Ajouter le nouveau verset
    (push verse org-verse-sidebar-history)
    (setq org-verse-sidebar-history-index 0)))

(defun org-verse-sidebar-previous-verse ()
  "Navigue vers le verset précédent dans l'historique."
  (interactive)
  (when (< org-verse-sidebar-history-index
           (1- (length org-verse-sidebar-history)))
    (cl-incf org-verse-sidebar-history-index)
    (let ((verse (nth org-verse-sidebar-history-index
                      org-verse-sidebar-history)))
      (org-verse-sidebar-navigate-to-verse verse))))

(defun org-verse-sidebar-next-verse ()
  "Navigue vers le verset suivant dans l'historique."
  (interactive)
  (when (> org-verse-sidebar-history-index 0)
    (cl-decf org-verse-sidebar-history-index)
    (let ((verse (nth org-verse-sidebar-history-index
                      org-verse-sidebar-history)))
      (org-verse-sidebar-navigate-to-verse verse))))

(defun org-verse-sidebar-navigate-to-verse (verse-string)
  "Navigue vers VERSE-STRING."
  (when verse-string
    (let ((parts (split-string verse-string " ")))
      (when (>= (length parts) 2)
        (let* ((book (car parts))
               (ref (cadr parts))
               (chapter-verses (split-string ref ":"))
               (chapter (car chapter-verses))
               (verses (cadr chapter-verses)))
          ;; Ne pas ajouter à l'historique car on navigue dans l'historique
          (let ((org-verse-sidebar-history-index -1))
            (org-verse-sidebar-refresh verse-string book chapter verses)))))))

;;;; Interactive commands

(defun org-verse-sidebar-follow-link ()
  "Suit le lien sous le curseur."
  (interactive)
  (if-let ((button (button-at (point))))
      (button-activate button)
    (user-error "Pas de lien à cette position")))

(defun org-verse-sidebar-copy-verse ()
  "Copie le verset actuel dans le kill-ring."
  (interactive)
  (if org-verse-current-verse
      (let ((parts (split-string org-verse-current-verse " ")))
        (when (>= (length parts) 2)
          (let* ((book (car parts))
                 (ref (cadr parts))
                 (chapter-verses (split-string ref ":"))
                 (chapter (car chapter-verses))
                 (verses (cadr chapter-verses))
                 (content (org-verse-analyse-verses book chapter verses)))
            (if content
                (let ((full-text (format "%s\n%s" org-verse-current-verse content)))
                  (kill-new full-text)
                  (message "Copié : %s" (truncate-string-to-width full-text 60)))
              (progn
                (kill-new org-verse-current-verse)
                (message "Référence copiée : %s" org-verse-current-verse))))))
    (user-error "Aucun verset actuel")))

(defun org-verse-sidebar-copy-reference ()
  "Copie uniquement la référence du verset."
  (interactive)
  (when org-verse-current-verse
    (kill-new org-verse-current-verse)
    (message "Référence copiée : %s" org-verse-current-verse)))

(defun org-verse-sidebar-refresh-current ()
  "Rafraîchit la sidebar avec le verset actuel."
  (interactive)
  (if org-verse-current-verse
      (let ((parts (split-string org-verse-current-verse " ")))
        (when (>= (length parts) 2)
          (let* ((book (car parts))
                 (ref (cadr parts))
                 (chapter-verses (split-string ref ":"))
                 (chapter (car chapter-verses))
                 (verses (cadr chapter-verses)))
            (org-verse-sidebar-refresh org-verse-current-verse book chapter verses))))
    (user-error "Aucun verset actuel")))

(defun org-verse-sidebar-search ()
  "Recherche un verset via completion."
  (interactive)
  (let ((verse (org-verse-complete)))
    (when verse
      (let ((parts (split-string verse " ")))
        (when (>= (length parts) 2)
          (let* ((book (car parts))
                 (ref (cadr parts))
                 (chapter-verses (split-string ref ":"))
                 (chapter (car chapter-verses))
                 (verses (cadr chapter-verses)))
            (org-verse-sidebar-refresh verse book chapter verses)))))))

(defun org-verse-sidebar-search-notes ()
  "Recherche dans les notes de versets."
  (interactive)
  (let ((search-term (read-string "Rechercher dans les notes : ")))
    (org-verse-find-file-by-full-text-search search-term)))

;;;; Copy formats

(defun org-verse-sidebar-copy-org-format ()
  "Copie le verset au format org-mode."
  (interactive)
  (when org-verse-current-verse
    (let ((parts (split-string org-verse-current-verse " ")))
      (when (>= (length parts) 2)
        (let* ((book (car parts))
               (ref (cadr parts))
               (chapter-verses (split-string ref ":"))
               (chapter (car chapter-verses))
               (verses (cadr chapter-verses))
               (content (org-verse-analyse-verses book chapter verses)))
          (when content
            (let ((org-text (format "#+begin_quote\n%s\n— %s\n#+end_quote"
                                    content org-verse-current-verse)))
              (kill-new org-text)
              (message "Copié au format org-mode"))))))))

(defun org-verse-sidebar-copy-markdown-format ()
  "Copie le verset au format Markdown."
  (interactive)
  (when org-verse-current-verse
    (let ((parts (split-string org-verse-current-verse " ")))
      (when (>= (length parts) 2)
        (let* ((book (car parts))
               (ref (cadr parts))
               (chapter-verses (split-string ref ":"))
               (chapter (car chapter-verses))
               (verses (cadr chapter-verses))
               (content (org-verse-analyse-verses book chapter verses)))
          (when content
            (let ((md-text (format "> %s\n> \n> — %s"
                                   content org-verse-current-verse)))
              (kill-new md-text)
              (message "Copié au format Markdown"))))))))

;;;; Window management

(defun org-verse-sidebar-increase-width ()
  "Augmente la largeur de la sidebar."
  (interactive)
  (let ((window (get-buffer-window org-verse-buffer)))
    (when window
      (window-resize window 5 t))))

(defun org-verse-sidebar-decrease-width ()
  "Diminue la largeur de la sidebar."
  (interactive)
  (let ((window (get-buffer-window org-verse-buffer)))
    (when window
      (window-resize window -5 t))))

(defun org-verse-sidebar-open-directory ()
  "Ouvre le répertoire des notes dans dired."
  (interactive)
  (dired org-verse-directory))

;;;; Transient interfaces

(transient-define-prefix org-verse-sidebar-help ()
  "Aide et commandes pour org-verse sidebar."
  ["Navigation"
   [("q" "Fermer la sidebar" org-verse-sidebar-quit)
    ("Q" "Fermer et tuer le buffer" kill-this-buffer)]
   [("p" "Verset précédent (historique)" org-verse-sidebar-previous-verse
     :if (lambda () (< org-verse-sidebar-history-index
                       (1- (length org-verse-sidebar-history)))))
    ("N" "Verset suivant (historique)" org-verse-sidebar-next-verse
     :if (lambda () (> org-verse-sidebar-history-index 0)))]]

  ["Actions"
   [("RET" "Suivre le lien" org-verse-sidebar-follow-link)
    ("TAB" "Plier/Déplier section" magit-section-toggle)]
   [("c" "Copier le verset" org-verse-sidebar-copy-verse)
    ("C" "Copier la référence seule" org-verse-sidebar-copy-reference)]]

  ["Recherche et Notes"
   [("s" "Rechercher un verset" org-verse-sidebar-search)
    ("/" "Recherche dans les notes" org-verse-sidebar-search-notes)]
   [("a" "Capturer une note" org-verse-capture)
    ("n" "Nouvelle note" org-verse-note-capture)]]

  ["Affichage"
   [("r" "Rafraîchir" org-verse-sidebar-refresh-current)
    ("+" "Augmenter la largeur" org-verse-sidebar-increase-width)
    ("-" "Diminuer la largeur" org-verse-sidebar-decrease-width)]]

  ["Configuration"
   [("v" "Changer de version/langue" org-verse-select-version)
    ("d" "Ouvrir le répertoire des notes" org-verse-sidebar-open-directory)]])

(transient-define-prefix org-verse-sidebar-actions ()
  "Actions contextuelles pour le verset actuel."
  [:description
   (lambda ()
     (if org-verse-current-verse
         (propertize (format "Actions pour : %s" org-verse-current-verse)
                     'face 'transient-heading)
       "Aucun verset sélectionné"))
   ["Copier"
    ("cc" "Verset complet" org-verse-sidebar-copy-verse)
    ("cr" "Référence seule" org-verse-sidebar-copy-reference)
    ("cf" "Format org-mode" org-verse-sidebar-copy-org-format)
    ("cm" "Format Markdown" org-verse-sidebar-copy-markdown-format)]])

;;;; Button action

(defun org-verse-button-verse (button)
  "BUTTON verse - ouvre ou met à jour la sidebar."
  (let ((refverse (button-get button 'title))
        (book (button-get button 'book))
        (chapter (button-get button 'chapter))
        (verses (button-get button 'verses)))
    ;; Si buffer pas ouvert, ouvrir sinon mettre à jour
    (if (window-live-p (get-buffer-window org-verse-buffer))
        (org-verse-sidebar-refresh refverse book chapter verses)
      (org-verse-toggle-sidebar)
      (org-verse-sidebar-refresh refverse book chapter verses))))

(provide 'org-verse-sidebar)
;;; org-verse-sidebar.el ends here
