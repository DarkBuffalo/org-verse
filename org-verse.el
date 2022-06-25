;;; org-verse.el --- Highlight verses in org-mode -*- lexical-binding: t -*-
;; Author: Matthias David

;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1")(dash)(anaphora)(esqlite)(s)(db))

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

;; This minor mode provides syntax highlighting of numeric literals
;; in source code, like what many editors provide by default.
;;
;; To enable: call `org-verse-mode'.
;;
;;; Code:
;;;

(require 'button)
(require 'dash)
(require 'cl-lib)
(require 'anaphora)
(require 'esqlite)
(require 's)
(require 'db)

(require 'magit-section)

(defgroup verse nil
  "Highlight numbers in source code."
  :prefix "org-verse-"
  :group 'faces)

(defface org-verse-number-face
  '((t ;;:inherit font-lock-constant-face
     :foreground "#81B145"
     ;;:background "#1B2229"
		 :box (:line-width 2)
     :weight bold))
  "Face used to verse references."
  :group 'verse)

(defvar org-verse-db-table-name "bible"
	"Table name.")

(defconst *user-home-directory*
  (getenv "HOME")
  "Path to user home directory.")


;;;; Variables
(defcustom org-verse-directory nil
  "Main verse directory."
  :type 'string)

(defcustom org-verse-file-extension "org"
  "The extension for verse files."
  :type 'string)

(defcustom org-verse-cache-dir (concat user-emacs-directory "orgverse/")
  "Org-verse's cache directory."
  :type 'directory
  :group 'org-verse)

(defvar org-verse-notes-db
  (db-make
   `(db-hash
     :filename ,(concat org-verse-cache-dir "org-verse-notes-db")))
  "Database for caching notes.")

(defconst org-verse-lexical
  '((org-verse-search-for-verse
     (0 'org-verse-number-face t))))

(defconst org-verse-pattern
  (rx  (group  (or (or "Genèse" "Gen" "Gn")
                   (or "Exode" "Ex")
                   (or "Lévitique" "Lev" "Lv")
                   (or "Nombres" "Nb")
                   (or "Deutéronome" "Deut" "Dt")
                   (or "Josué" "Jos")
                   (or "Juges" "Jg")
                   (or "Ruth" "Ru")
                   (or "1 Samuel" "1S" "1Sam" "1 Sam")
                   (or "2 Samuel" "2S" "2Sam" "2 Sam")
                   (or "1 Rois" "1R" "1 R")
                   (or "2 Rois" "2R" "2 R")
                   (or "1 Chroniques" "1Ch" "2 Ch")
                   (or "2 Chroniques" "2Ch" "2 Ch")
                   (or "Esdras" "Esd")
                   (or "Néhémie" "Né" "Ne")
                   (or "Esther" "Est")
                   (or "Job" "Jb")
                   (or "Psaumes" "Ps")
                   (or "Proverbes" "Prov" "Pro" "Pr")
                   (or "Ecclésiaste" "Ecc" "Ec" "Ecl")
                   (or "Chant de Salomon" "Ct")
                   (or "Isaïe" "Is")
                   (or "Jérémie" "Jer" "Jr")
                   (or "Lamentations" "Lam" "Lm")
                   (or "Ézéchiel" "Ez")
                   (or "Daniel" "Dan" "Dn")
                   (or "Osée" "Os")
                   (or "Joël" "Jl")
                   (or "Amos" "Am")
                   (or "Abdias" "Ab")
                   (or "Jonas" "Jon")
                   (or "Michée" "Mi")
                   (or "Nahum" "Na")
                   (or "Habacuc" "Hab")
                   (or "Sophonie" "Soph" "Sph")
                   (or "Aggée" "Ag")
                   (or "Zacharie" "Za")
                   (or "Malachie" "Ml")
                   (or "Matthieu" "Matt" "Mt")
                   (or "Marc" "Mc")
                   (or "Luc" "Lc")
                   (or "Jean" "Je")
                   (or "Actes" "Ac")
                   (or "Romains" "Rom" "Rm")
                   (or "1 Corinthiens" "1Co" "1Cor" )
                   (or "2 Corinthiens" "2Co" "2Cor")
                   (or "Galates" "Ga")
                   (or "Éphésiens" "Eph")
                   (or "Philippiens" "Php")
                   (or "Colossiens" "Col")
                   (or "1 Thessaloniciens" "1Th")
                   (or "2 Thessaloniciens" "2Th")
                   (or "1 Timothée" "1Tm" "1Tim")
                   (or "2 Timothée" "2Tm" "2Tim")
                   (or "Tite" "Tt")
                   (or "Philémon" "Phm")
                   (or "Hébreux" "Hé" "He")
                   (or "Jacques" "Jc")
                   (or "1 Pierre" "1Pierre" "1P")
                   (or "2 Pierre" "2Pierre" "2P")
                   (or "1 Jean" "1J")
                   (or "2 Jean" "2J")
                   (or "3 Jean" "3J")
                   (or "Jude")
                   (or "Révélation" "Rév" "Ré" "Re" "Rv")))
       space
       (group (1+ digit))
       ":"
       (group (or
               (group (group(any digit))"-"(group (1+ digit)":"(1+ digit)))
               (group (1+ (1+ digit )(0+ ","))))))
  "Generic regexp for number highlighting.
It is used when no mode-specific one is available.")


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

;; STRUCTURE --------------------------------------------------------------------------------
(defconst org-verse--canon nil
  "A list of all the book.")

(cl-defstruct org-verse--book
  pattern abbrev name number verses)

(defun org-verse-add-book (pattern abbrev name number verses)
  "Define a new book for canon with PATTERN, ABBREV, NAME, NUMBER and VERSES in parameters."
  (let ((new-book (make-org-verse--book
	           :pattern pattern
	           :abbrev abbrev
	           :name name
	           :number number
	           :verses verses)))
    ;; Replace the entry if there is already a book with the same name.
    (aif (cl-position name org-verse--canon
                      :test #'equal
                      :key #'org-verse--book-name)
        (setf (nth it org-verse--canon) new-book)
      ;; Freshly add it, otherwise.
      (push new-book org-verse--canon))))

(org-verse-add-book '((rx (or "Genèse" "Gen" "Gn"))) "Gn" "Genèse" 1 '((1 . 31) (2 . 25) (3 . 24) (4 . 26) (5 . 32) (6 . 22) (7 . 24) (8 . 22) (9 . 29) (10 . 32) (11 . 32) (12 . 20) (13 . 18) (14 . 24) (15 . 21) (16 . 16) (17 . 27) (18 . 33) (19 . 38) (20 . 18) (21 . 34) (22 . 24) (23 . 20) (24 . 67) (25 . 34) (26 . 35) (27 . 46) (28 . 22) (29 . 35) (30 . 43) (31 . 55) (32 . 32) (33 . 20) (34 . 31) (35 . 29) (36 . 43) (37 . 36) (38 . 30) (39 . 23) (40 . 23) (41 . 57) (42 . 38) (43 . 34) (44 . 34) (45 . 28) (46 . 34) (47 . 31) (48 . 22) (49 . 33) (50 . 26)))
(org-verse-add-book '((rx (or "Exode" "Ex"))) "Ex" "Exode" 2  '((1 . 22) (2 . 25) (3 . 22) (4 . 31) (5 . 23) (6 . 30) (7 . 25) (8 . 32) (9 . 35) (10 . 29) (11 . 10) (12 . 51) (13 . 22) (14 . 31) (15 . 27) (16 . 36) (17 . 16) (18 . 27) (19 . 25) (20 . 26) (21 . 36) (22 . 31) (23 . 33) (24 . 18) (25 . 40) (26 . 37) (27 . 21) (28 . 43) (29 . 46) (30 . 38) (31 . 18) (32 . 35) (33 . 23) (34 . 35) (35 . 35) (36 . 38) (37 . 29) (38 . 31) (39 . 43) (40 . 38)))
(org-verse-add-book '((rx (or "Lévitique" "Lev" "Lv"))) "Lv" "Lévitique" 3  '((1 . 17) (2 . 16) (3 . 17) (4 . 35) (5 . 19) (6 . 30) (7 . 38) (8 . 36) (9 . 24) (10 . 20) (11 . 47) (12 . 8) (13 . 59) (14 . 57) (15 . 33) (16 . 34) (17 . 16) (18 . 30) (19 . 37) (20 . 27) (21 . 24) (22 . 33) (23 . 44) (24 . 23) (25 . 55) (26 . 46) (27 . 34)))
(org-verse-add-book '((rx (or "Nombres" "Nb"))) "Nb" "Nombres" 4      '((1 . 54) (2 . 34) (3 . 51) (4 . 49) (5 . 31) (6 . 27) (7 . 89) (8 . 26) (9 . 23) (10 . 36) (11 . 35) (12 . 16) (13 . 33) (14 . 45) (15 . 41) (16 . 50) (17 . 13) (18 . 32) (19 . 22) (20 . 29) (21 . 35) (22 . 41) (23 . 30) (24 . 25) (25 . 18)(26 . 65) (27 . 23) (28 . 31) (29 . 40) (30 . 16) (31 . 54) (32 . 42) (33 . 56) (34 . 29) (35 . 34) (36 . 13)))
(org-verse-add-book '((rx (or "Deutéronome" "Deut" "Dt"))) "Dt" "Deutéronome" 5  '((1 . 46) (2 . 37) (3 . 29) (4 . 49) (5 . 33) (6 . 25) (7 . 26) (8 . 20) (9 . 29) (10 . 22) (11 . 32) (12 . 32) (13 . 18) (14 . 29) (15 . 23) (16 . 22) (17 . 20) (18 . 22) (19 . 21) (20 . 20) (21 . 23) (22 . 30) (23 . 25) (24 . 22) (25 . 19)(26 . 19) (27 . 26) (28 . 68) (29 . 29) (30 . 20) (31 . 30) (32 . 52) (33 . 29) (34 . 12)))
(org-verse-add-book '((rx (or "Josué" "Jos"))) "Jos" "Josué" 6  '((1 . 18) (2 . 24) (3 . 17) (4 . 24) (5 . 15) (6 . 27) (7 . 26) (8 . 35) (9 . 27) (10 . 43) (11 . 23) (12 . 24) (13 . 33) (14 . 15) (15 . 63) (16 . 10) (17 . 18) (18 . 28) (19 . 51) (20 . 9) (21 . 45) (22 . 34) (23 . 16) (24 . 33)))
(org-verse-add-book '((rx (or "Juges" "Jg"))) "Jg" "Juges" 7  '((1 . 36) (2 . 23) (3 . 31) (4 . 24) (5 . 31) (6 . 40) (7 . 25) (8 . 35) (9 . 57) (10 . 18) (11 . 40) (12 . 15) (13 . 25) (14 . 20) (15 . 20) (16 . 31) (17 . 13) (18 . 31) (19 . 30) (20 . 48) (21 . 25)))
(org-verse-add-book '((rx (or "Ruth" "Ru"))) "Ru" "Ruth" 8  '((1 . 22) (2 . 23) (3 . 18) (4 . 22)))
(org-verse-add-book '((rx (or "1 Samuel" "1S" "1Sam" "1 Sam"))) "1S" "1 Samuel" 9  '((1 . 28) (2 . 36) (3 . 21) (4 . 22) (5 . 12) (6 . 21) (7 . 17) (8 . 22) (9 . 27) (10 . 27) (11 . 15) (12 . 25) (13 . 23) (14 . 52) (15 . 35) (16 . 23) (17 . 58) (18 . 30) (19 . 24) (20 . 42) (21 . 15) (22 . 23) (23 . 29) (24 . 22) (25 . 44) (26 . 25) (27 . 12) (28 . 25) (29 . 11) (30 . 31) (31 . 13)))
(org-verse-add-book '((rx (or "2 Samuel" "2S" "2Sam" "2 Sam"))) "2S" "2 Samuel" 10  '((1 . 27) (2 .32) (3 . 39) (4 . 12) (5 . 25) (6 . 23) (7 . 29) (8 . 18) (9 . 13) (10 . 19) (11 . 27) (12 . 31) (13 . 39) (14 . 33) (15 . 37) (16 . 23) (17 . 29) (18 . 33) (19 . 43) (20 . 26) (21 . 22) (22 . 51) (23 . 39) (24 . 25)))
(org-verse-add-book '((rx (or "1 Rois" "1R" "1 R"))) "1R" "1 Rois" 11  '((1 . 53) (2 . 46) (3 . 28) (4 . 34) (5 . 18) (6 . 38) (7 . 51) (8 . 66) (9 . 28) (10 . 29) (11 . 43) (12 . 33) (13 . 34) (14 . 31) (15 . 34) (16 . 34) (17 . 24) (18 . 46) (19 . 21) (20 . 43) (21 . 29) (22 . 53)))
(org-verse-add-book '((rx (or "2 Rois" "2R" "2 R"))) "2R" "2 Rois" 12  '((1 . 18) (2 . 25) (3 . 27) (4 . 44) (5 . 27) (6 . 33) (7 . 20) (8 . 29) (9 . 37) (10 . 36) (11 . 21) (12 . 21) (13 . 25) (14 . 29) (15 . 38) (16 . 20) (17 . 41) (18 . 37) (19 . 37) (20 . 21) (21 . 26) (22 . 20) (23 . 37) (24 . 20) (25 . 30)))
(org-verse-add-book '((rx (or "1 Chroniques" "1Ch" "2 Ch"))) "1Ch" "1 Chroniques" 13  '((1 . 54) (2 . 55) (3 . 24) (4 . 43) (5 . 26) (6 . 81) (7 . 40) (8 . 40) (9 . 44) (10 . 14) (11 . 47) (12 . 40) (13 . 14) (14 . 17) (15 . 29) (16 . 43) (17 . 27) (18 . 17) (19 . 19) (20 . 8) (21 . 30) (22 . 19) (23 . 32) (24 . 31) (25 . 31) (26 . 32) (27 . 34) (28 . 21) (29 . 30)))
(org-verse-add-book '((rx (or "2 Chroniques" "2Ch" "2 Ch"))) "2Ch" "2 Chroniques" 14  '((1 . 17) (2 . 18) (3 . 17) (4 . 22) (5 . 14) (6 . 42) (7 . 22) (8 . 18) (9 . 31) (10 . 19) (11 . 23) (12 . 16) (13 . 22) (14 . 15) (15 . 19) (16 . 14) (17 . 19) (18 . 34) (19 . 11) (20 . 37) (21 . 20) (22 . 12) (23 . 21) (24 . 27) (25 . 28) (26 . 23) (27 . 9) (28 . 27) (29 . 36) (30 . 27) (31 . 21) (32 .33) (33 . 25) (34 . 33) (35 . 27) (36 . 23)))
(org-verse-add-book '((rx (or "Esdras" "Esd"))) "Esd" "Esdras" 15  '((1 . 11) (2 . 70) (3 . 13) (4 . 24) (5 . 17) (6 . 22) (7 . 28) (8 . 36) (9 . 15) (10 . 44)))
(org-verse-add-book '((rx (or "Néhémie" "Né" "Ne"))) "Né" "Néhémie" 16  '((1 . 11) (2 . 20) (3 . 32) (4 . 23) (5 . 19) (6 . 19) (7 . 73) (8 . 18) (9 . 38) (10 . 39) (11 . 36) (12 . 47) (13 . 31)))
(org-verse-add-book '((rx (or "Esther" "Est"))) "Est" "Esther" 17  '((1 . 22) (2 . 23) (3 . 15) (4 . 17) (5 . 14) (6 . 14) (7 . 10) (8 . 17) (9 . 32) (10 . 3)))
(org-verse-add-book '((rx (or "Job" "Jb"))) "Jb" "Job" 18  '((1 . 22) (2 . 13) (3 . 26) (4 . 21) (5 . 27) (6 . 30) (7 . 21) (8 . 22) (9 . 35) (10 . 22) (11 . 20) (12 . 25) (13 . 28) (14 . 22) (15 . 35) (16 . 22) (17 . 16) (18 . 21) (19 . 29) (20 . 29) (21 . 34) (22 . 30) (23 . 17) (24 . 25) (25 . 6) (26 . 14) (27 . 23) (28 . 28) (29 . 25) (30 . 31) (31 . 40) (32 . 22) (33 . 33) (34 . 37) (35 . 16) (36 . 33) (37 . 24) (38 . 41) (39 . 30) (40 . 24) (41 . 34) (42 . 17)))
(org-verse-add-book '((rx (or "Psaumes" "Ps"))) "Ps" "Psaumes" 19  '((1 . 6) (2 . 12) (3 . 8) (4 . 8) (5 . 12) (6 . 10) (7 . 17) (8 . 9) (9 . 20) (10 . 18) (11 . 7) (12 . 8) (13 . 6) (14 . 7) (15 . 5) (16 . 11) (17 . 15) (18 . 50) (19 . 14) (20 . 9) (21 . 13) (22 . 31) (23 . 6) (24 . 10) (25 . 22) (26 . 12) (27 . 14) (28 . 9) (29 . 11) (30 . 12) (31 . 24) (32 . 11) (33 . 22) (34 . 22) (35 . 28) (36 . 12) (37 . 40) (38 . 22) (39 . 13) (40 . 17) (41 . 13) (42 . 11) (43 . 5) (44 . 26) (45 . 17) (46 . 11) (47 . 9) (48 . 14) (49 . 20) (50 . 23) (51 . 19) (52 . 9) (53 . 6) (54 . 7) (55 . 23) (56 . 13) (57 . 11) (58 . 11) (59 . 17) (60 . 12) (61 . 8) (62 . 12) (63 . 11) (64 . 10) (65 . 13) (66 . 20) (67 . 7) (68 . 35) (69 . 36) (70 . 5) (71 . 24) (72 . 20) (73 . 28) (74 . 23) (75 . 10) (76 . 12) (77 . 20) (78 . 72) (79 . 13) (80 . 19) (81 . 16) (82 . 8) (83 . 18) (84 . 12) (85 . 13) (86 . 17) (87 . 7) (88 . 18) (89 . 52) (90 . 17) (91 . 16) (92 . 15) (93 . 5) (94 . 23) (95 . 11) (96 . 13) (97 . 12) (98 . 9) (99 .  9) (100 . 5) (101 . 8) (102 . 28) (103 . 22) (104 . 35) (105 . 45) (106 . 48) (107 . 43) (108 . 13) (109 . 31) (110 . 7) (111 . 10) (112 . 10) (113 . 9) (114 . 8) (115 . 18) (116 . 19) (117 . 2) (118. 29) (119 . 176) (120 . 7) (121 . 8) (122 . 9) (123 . 4) (124 . 8) (125 . 5) (126 . 6) (127 . 5) (128 . 6) (129 . 8) (130 . 8) (131 . 3) (132 . 18) (133 . 3) (134 . 3) (135 . 21) (136 . 26) (137 . 9) (138 . 8) (139 . 24) (140 . 13) (141 . 10) (142 . 7) (143 . 12) (144 . 15) (145 . 21) (146 . 10) (147 . 20) (148 . 14) (149 . 9) (150 . 6)))
(org-verse-add-book '((rx (or "Proverbes" "Prov" "Pro" "Pr"))) "Pr" "Proverbes" 20  '((1 . 3) (2 . 22) (3 . 35) (4 . 27) (5 . 23) (6 . 35) (7 . 27) (8 . 36) (9 . 18) (10 . 32) (11 . 31) (12 . 28) (13 . 25) (14 . 35) (15 . 33) (16 . 33) (17 . 28) (18 . 24) (19 . 29) (20 . 30) (21 . 31) (22 . 29) (23 . 35) (24 . 34) (25 . 28) (26 . 28) (27 . 27) (28 . 28) (29 . 27) (30 . 33) (31 . 31)))
(org-verse-add-book '((rx (or "Ecclésiaste" "Ecc" "Ec" "Ecl"))) "Ec" "Ecclésiaste" 21  '((1 . 18) (2 . 26) (3 . 22) (4 . 16) (5 . 20) (6 . 12) (7 . 29) (8 . 17) (9 . 18) (10 . 20) (11 . 10) (12 . 14)))
(org-verse-add-book '((rx (or "Chant de Salomon" "Ct"))) "Ct" "Chant de Salomon" 22  '((1 . 17) (2 . 17) (3 . 11) (4 . 16) (5 . 16) (6 . 13) (7 . 13) (8. 14)))
(org-verse-add-book '((rx (or "Isaïe" "Is"))) "Is" "Isaïe" 23  '((1 . 31) (2 . 22) (3 . 26) (4 . 6) (5 . 30) (6 . 13) (7 . 25) (8 . 22) (9 . 21) (10 . 34) (11 . 16) (12 . 6) (13 . 22) (14 . 32) (15 . 9) (16 . 14) (17 . 14) (18 . 7) (19 . 25) (20 . 6) (21 . 17) (22 . 25) (23 . 18) (24 . 23) (25 . 12) (26 . 21) (27 . 13) (28 . 29) (29 . 24) (30 . 33) (31 . 9) (32 . 20) (33 . 24) (34 . 17) (35 . 10) (36 . 22) (37 . 38) (38 . 22) (39 . 8) (40 . 31) (41 . 29) (42 . 25) (43 . 28) (44 . 28) (45 . 25) (46 . 13) (47 . 15) (48 . 22) (49 . 26) (50 . 11) (51 . 23) (52 . 15) (53 . 12) (54 . 17) (55 . 13) (56 . 12) (57 . 21) (58 . 14) (59 . 21) (60 . 22) (61 . 11) (62 . 12) (63 . 19) (64 . 12) (65 . 25) (66 . 24)))
(org-verse-add-book '((rx (or "Jérémie" "Jer" "Jr"))) "Jr" "Jérémie" 24  '((1 . 19) (2 . 37) (3 . 25) (4 . 31) (5 . 31) (6 . 30) (7 . 34) (8 . 22) (9 . 26) (10 . 25) (11 . 23) (12 . 17) (13 . 27) (14 . 22) (15 . 21) (16 . 21) (17 . 27) (18 . 23) (19 . 15) (20 . 18) (21 . 14) (22 . 30) (23 . 40) (24 . 10) (25 . 38) (26 . 24) (27 . 22) (28 . 17) (29 . 32) (30 . 24) (31 . 40) (32 . 44) (33 . 26) (34 . 22) (35 . 19) (36 . 32) (37 . 21) (38 . 28) (39 . 18) (40 . 16) (41 . 18) (42 . 22) (43 . 13) (44 . 30) (45 . 5) (46 . 28) (47 . 7) (48 . 47) (49 . 39) (50 . 46) (51 . 64) (52 . 34)))
(org-verse-add-book '((rx (or "Lamentations" "Lam" "Lm"))) "Lm" "Lamentations" 25  '((1 . 22) (2 . 22) (3 . 66) (4 . 22) (5 . 22)))
(org-verse-add-book '((rx (or "Ézéchiel" "Ez"))) "Ez" "Ezéchiel" 26  '((1 . 28) (2 . 10) (3 . 27) (4 . 17) (5 . 17) (6 . 14) (7 . 27) (8 . 18) (9 . 11) (10 . 22) (11 . 25) (12 . 28) (13 . 23) (14 . 23) (15 . 8) (16 . 63) (17 . 24) (18 . 32) (19 . 14) (20 . 49) (21 . 32) (22 . 31) (23 . 49) (24 . 27) (25 . 17) (26 . 21) (27 . 36) (28 . 26) (29 . 21) (30 . 26) (31 . 18) (32 . 32) (33 . 33) (34 . 31)(35 . 15) (36 . 38) (37 . 28) (38 . 23) (39 . 29) (40 . 49) (41 . 26) (42 . 20) (43 . 27) (44 . 31) (45 . 25) (46 . 24) (47 . 23) (48 . 35)))
(org-verse-add-book '((rx (or "Daniel" "Dan" "Dn"))) "Dn" "Daniel" 27  '((1 . 21) (2 . 49) (3 . 30) (4 . 37) (5 . 31) (6 . 28) (7 . 28) (8 . 27) (9 . 27) (10 . 21) (11 . 45) (12 . 1)))
(org-verse-add-book '((rx (or "Osée" "Os"))) "Os" "Osée" 28  '((1 . 11) (2 . 23) (3 . 5) (4 . 19) (5 . 15) (6 . 11) (7 . 16) (8 . 14) (9 . 17) (10 . 15) (11 . 12) (12 . 14) (13 . 16) (14 . 9)))
(org-verse-add-book '((rx (or "Joël" "Jl"))) "Jl" "Joël" 29  '((1 . 20) (2 . 32) (3 . 21)))
(org-verse-add-book '((rx (or "Amos" "Am"))) "Am" "Amos" 30  '((1 . 15) (2 . 16) (3 . 15) (4 . 13) (5 . 27) (6 . 14) (7 . 17) (8 . 14) (9 . 15)))
(org-verse-add-book '((rx (or "Abdias" "Ab"))) "Ab" "Abdias" 31  '((1 . 21)))
(org-verse-add-book '((rx (or "Jonas" "Jon"))) "Jon" "Jonas" 32  '((1 . 17) (2 . 10) (3 . 10) (4 . 11)))
(org-verse-add-book '((rx (or "Michée" "Mi"))) "Mi" "Michée" 33  '((1 . 16) (2 . 13) (3 . 12) (4 . 13) (5 . 15) (6 . 16) (7 . 20)))
(org-verse-add-book '((rx (or "Nahum" "Na"))) "Na" "Nahum" 34  '((1 . 15) (2 . 13) (3 . 19)))
(org-verse-add-book '((rx (or "Habacuc" "Hab"))) "Hab" "Habacuc" 35  '((1 . 17) (2 . 20) (3 . 19)))
(org-verse-add-book '((rx (or "Sophonie" "Soph" "Sph"))) "Sph" "Sophonie" 36  '((1 . 18) (2 . 15)  (3 . 20)))
(org-verse-add-book '((rx (or "Aggée" "Ag"))) "Ag" "Aggée" 37  '((1 . 15) (2 . 23)))
(org-verse-add-book '((rx (or "Zacharie" "Za"))) "Za" "Zacharie" 38  '((1 . 21) (2 . 13) (3 . 10) (4 . 14) (5 . 11) (6 . 15) (7 . 14) (8 . 23) (9 . 17) (10 . 12) (11 . 17) (12 . 14) (13 . 9) (14 . 21)))
(org-verse-add-book '((rx (or "Malachie" "Ml"))) "Ml" "Malachie" 39  '((1 . 14) (2 . 17) (3 . 18) (4 . 6)))
(org-verse-add-book '((rx (or "Matthieu" "Matt" "Mt"))) "Mt" "Matthieu" 40  '((1 . 5) (2 . 23) (3 . 17) (4 . 25) (6 . 48) (7 . 34) (8 . 29) (9 . 34) (10 . 38) (11 . 42) (12 . 30) (13 . 50) (14 . 58) (15 . 36) (16 . 39) (17 . 28) (18 . 27) (19 . 35) (20 . 30) (21 . 34) (22 . 46) (23 . 46) (24 . 39) (25 . 51) (26 . 46) (27 . 75) (28 . 66) (29 . 20)))
(org-verse-add-book '((rx (or "Marc" "Mc"))) "Mc" "Marc" 41  '((1 . 45) (2 . 28) (3 . 35) (4 . 41) (5 . 43) (6 . 56) (7 . 37) (8 . 38) (9 . 50) (10 . 52) (11 . 33) (12 . 44) (13 . 37) (14 . 72) (15 . 47) (16 . 20)))
(org-verse-add-book '((rx (or "Luc" "Lc"))) "Lc" "Luc" 42  '((1 . 80) (2 . 52) (3 . 38) (4 . 44) (5 . 39) (6 . 49) (7 . 50) (8 . 56) (9 . 62) (10 . 42) (11 . 54) (12 . 59) (13 . 35) (14 . 35) (15 . 32) (16 . 31) (17 . 37) (18 . 43) (19 . 48) (20 . 47) (21 . 38) (22 . 71) (23 . 56) (24 . 53)))
(org-verse-add-book '((rx (or "Jean" "Je"))) "Jean" "Jean" 43  '((1 . 51) (2 . 25) (3 . 36) (4 . 54) (5 . 47) (6 . 71) (7 . 53) (8 . 59) (9 . 41) (10 . 42) (11 . 57) (12 . 50) (13 . 38) (14 . 31) (15 . 27) (16 . 33) (17 . 26) (18 . 40) (19 . 42) (20 . 31) (21 . 25)))
(org-verse-add-book '((rx (or "Actes" "Ac"))) "Ac" "Actes" 44  '((1 . 26) (2 . 47) (3 . 26) (4 . 37) (5 . 42) (6 . 15) (7 . 60) (8 . 40) (9 . 43) (10 . 48) (11 . 30) (12 . 25) (13 . 52) (14 . 28) (15 . 41) (16 . 40) (17 . 34) (18 . 28) (19 . 41) (20 . 38) (21 . 40) (22 . 30) (23 . 35) (24 . 27) (25 . 27) (26 . 32) (27 . 44) (28 . 31)))
(org-verse-add-book '((rx (or "Romains" "Rom" "Rm"))) "Rm" "Romains" 45  '((1 . 32) (2 . 29) (3 . 31) (4 . 25) (5 . 21) (6 . 23) (7 . 25) (8 . 39) (9 . 33) (10 . 21) (11 . 36) ( 12 . 21) (13 . 14) (14 . 23) (15 . 33) (16 . 27)))
(org-verse-add-book '((rx (or "1 Corinthiens" "1Co" "1Cor" ))) "1Co" "1 Corinthiens" 46  '((1 . 31) (2 . 16) (3 . 23) (4 . 21) (5 . 13) (6 . 20) (7 . 40) (8 . 13) (9 . 27) (10 . 33) (11 . 34) (12 . 31) (13 . 13) (14 . 40) (15 . 58) (16 . 24)))
(org-verse-add-book '((rx (or "2 Corinthiens" "2Co" "2Cor"))) "2Co" "2 Corinthiens" 47  '((1 . 24) (2 . 17) (3 . 18) (4 . 18) (5 . 21) (6 . 18) (7 . 16) (8 . 24) (9 . 15) (10 . 18) (11 . 33) (12 . 21) (13 . 14)))
(org-verse-add-book '((rx (or "Galates" "Ga"))) "Ga" "Galates" 48  '((1 . 24) (2 . 21) (3 . 29) (4 . 31) (5 . 26) (6 . 18)))
(org-verse-add-book '((rx (or "Éphésiens" "Eph"))) "Eph" "Ephésiens" 49  '((1 . 23) (2 . 22) (3 . 21) (4 . 32) (5 . 33) (6 . 24)))
(org-verse-add-book '((rx (or "Philippiens" "Php"))) "Php" "Philippiens" 50  '((1 . 30) (2 . 30) (3 . 21) (4 . 23)))
(org-verse-add-book '((rx (or "Colossiens" "Col"))) "Col" "Colossiens" 51  '((1 . 29) (2 . 23) (3 . 25) (4 . 18)))
(org-verse-add-book '((rx (or "1 Thessaloniciens" "1Th"))) "1Th" "1 Thessaloniciens" 52  '((1 . 10) (2 . 20) (3 . 13) (4 . 18) (5 . 28)))
(org-verse-add-book '((rx (or "2 Thessaloniciens" "2Th"))) "2Th" "2 Thessaloniciens" 53  '((1 . 12) (2 . 17) (3 . 18)))
(org-verse-add-book '((rx (or "1 Timothée" "1Tm" "1Tim"))) "1Tm" "1 Timothée" 54  '((1 . 20) (2 . 15) (3 . 16) (4 . 16) (5 . 25) (6 . 21)))
(org-verse-add-book '((rx (or "2 Timothée" "2Tm" "2Tim"))) "2Tm" "2 Thimothée" 55  '((1 . 18) (2 . 26) (3 . 17) (4 . 22)))
(org-verse-add-book '((rx (or "Tite" "Tt"))) "Tt" "Tite" 56  '((1 . 16) (2 . 15) (3 . 15)))
(org-verse-add-book '((rx (or "Philémon" "Phm"))) "Phm" "Philémon" 57  '((1 . 25)))
(org-verse-add-book '((rx (or "Hébreux" "Hé" "He"))) "Hé" "Hébreux" 58  '((1 . 14) (2 . 18) (3 . 19)(4 . 16) (5 . 14) (6 . 20) (7 . 28) (8 . 13) (9 . 28) (10 . 39) (11 . 40) (12 . 29)(13 . 25)))
(org-verse-add-book '((rx (or "Jacques" "Jc"))) "Jc" "Jacques" 59  '((1 . 27) (2 . 26) (3 . 18) (4 . 17) (5 . 20)))
(org-verse-add-book '((rx (or "1 Pierre" "1Pierre" "1P"))) "1P" "1 Pierre" 60  '((1 . 25) (2 . 25) (3 . 22) (4 . 19) (5 . 14)))
(org-verse-add-book '((rx (or "2 Pierre" "2Pierre" "2P"))) "2P" "2 Pierre" 61  '((1 . 21) (2 . 22) (3 . 18)))
(org-verse-add-book '((rx (or "1 Jean" "1J"))) "1J" "1 Jean" 62  '((1 . 10) (2 . 29) (3 . 24) (4 . 21) (5 . 21)))
(org-verse-add-book '((rx (or "2 Jean" "2J"))) "2J" "2 Jean" 63  '((1 . 13)))
(org-verse-add-book '((rx (or "3 Jean" "3J"))) "3J" "3 Jean" 64  '((1. 14)))
(org-verse-add-book '((rx (or "Jude"))) "Jude" "Jude" 65  '((1 . 25)))
(org-verse-add-book '((rx (or "Révélation" "Rév" "Ré" "Re" "Rv"))) "Ré" "Révélation" 66  '((1 . 20) (2 . 29) (3 . 22) (4 . 11) (5 . 14) (6 . 17) (7 . 17) (8 . 13) (9 . 21) (10 . 11) (11 . 19) (13 . 17) (13 . 18) (14 . 20) (15 . 8) (16 . 21) (17 . 18) (18 . 24) (19 . 21) (20 . 15) (21 . 27) (22 . 21)))



;; ZK ----------------------------------------------------------------

(defvar org-verse-history nil)

(defcustom org-verse-select-file-function #'org-verse--select-file
  "Function for performing completing read.
Must take an optional prompt and a list of files"
  :type 'function)

(defun org-verse--grep-file-list (str)
  "Return a list of files containing regexp STR."
  (let* ((files (shell-command-to-string (concat
                                          "grep -lir --include \\*."
                                          org-verse-file-extension
                                          " -e "
                                          (shell-quote-argument str)
                                          " "
                                          org-verse-directory
                                          " 2>/dev/null"))))
    (split-string files "\n" t)))


(defun org-verse--select-file (&optional prompt list)
  "Wrapper around `completing-read' to select org-verse-file.
Offers candidates from 'org-verse-directory', or from LIST when
supplied. Can take a PROMPT argument."
  (let* ((files (if list list
                  ;;(org-verse--directory-files t)
									(directory-files org-verse-directory t (concat "\\." org-verse-file-extension "$"))
									)))
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
    "file with your search or verse"))


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



;; Backlinks

(defvar org-verse-link-buton-action #'find-file-other-window
  "Action for `org-verse-link--find-file'.")

(defun org-verse-link--find-file (button)
  "Action for BUTTON to `find-file'."
  (funcall org-verse-link-buton-action (buffer-substring (button-start button) (button-end button))))


(define-button-type 'org-verse-link-backlink-button
  'follow-link t
  'action #'org-verse-link--find-file
  'face 'unspecified)


(defun org-verse--prepare-backlinks (files &optional title)
  "Prepare backlinks' including FILES.
Use optional TITLE for a prettier heading."
	
	(magit-insert-section (backlinks t)
		(magit-insert-heading
			(insert (propertize (format "%s\n" "Backlinks") 'face 'magit-section-heading)))
		
		(magit-insert-section-body
			(mapc (lambda (f)
							(insert (propertize  (file-name-nondirectory f) 'face 'italic))
							(make-button (point-at-bol) (point-at-eol) :type 'org-verse-link-backlink-button)
							(newline))
						files))))

(defun org-verse--prepare-notes ()
  "Prepare notes' including links."
	(magit-insert-section (notes)
		(magit-insert-heading
			(insert (propertize (format "%s" "Notes") 'face 'magit-section-heading)))
		(magit-insert-section-body (insert (format "%s" "Notes")))))


;; COMPLETION -------------------------------------------------------------------------------
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
      (message \"Rockin' and rollin' to %s\" hostname))"

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
  (interactive)
  (let* ((book (org-verse-completing-read-by 'org-verse--book-name "Livre: " org-verse--canon))
        (chapter (string-to-number (org-verse-completing-read "Chapitre: "
                                                          (seq-map 'int-to-string
                                                                   (mapcar 'car  (org-verse--book-verses book))))))
        (verses (org-verse-completing-read "Verset: "
                                       (seq-map 'int-to-string
                                                (mapcan 'list
                                                        (cl-loop for i from 1 to (cdr (assoc chapter (org-verse--book-verses book)))
                                                                 collect i))))))
    (insert (format "%s %s:%s" (org-verse--book-abbrev book) chapter verses))))

;; DATABASE ESQLITE -----------------------------
(defvar org-verse--db-file-name "/bible.db"
	"Name of the db containing all the verses.")

(defconst org-verse--db-process nil
  "The process containing the opened db stream.")

(defun org-verse--db-stream ()
  "Return the db stream or create and open it if doesn't exist."
  (unless org-verse--db-process
    (let ((db-path (concat  *user-home-directory*
														org-verse--db-file-name)))
      (setf org-verse--db-process
            (esqlite-stream-open db-path))))
  org-verse--db-process)

(defun org-verse--db-read (query)
  "Call the QUERY on the database and return the result."
  (esqlite-stream-read (org-verse--db-stream) query))

(defun org-verse--serialize (livre chapter verse)
	"Serialize reference from LIVRE, CHAPTER and VERSE."
	(let ((ref '()))
		(cl-loop for x in org-verse--canon
						 when (s-matches-p (eval (car (org-verse--book-pattern x))) livre)
						 do	 (setq ref (list :book (org-verse--book-number x) :chapter chapter :verse verse )))
		ref))

(defun org-verse--db-get-verse (book chapter verse)
  "Retrieve the ref for BOOK, CHAPTER and VERSE."
	(aif
			(org-verse--db-read
			 (format "SELECT verse FROM \"%s\" WHERE book=\"%s\" AND chapter=\"%s\" AND nbverse=\"%s\""
							 org-verse-db-table-name book chapter verse))
			(car (car it))))

;; SIDEBAR BUFFER VERSE --------------------------------------
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

(defun org-verse-sidebar-refresh (refverse book chapter verses)
  "Refresh the sidebar with REFVERSE, BOOK, CHAPTER and VERSES."
  (with-current-buffer org-verse-buffer
    (with-silent-modifications
			(setq header-line-format (list :propertize "VERSET" 'face 'bold))
			(erase-buffer)
			(visual-line-mode 1)
			
			(let ((ref (org-verse--serialize book chapter verses))
						(inhibit-read-only t))
				
				(magit-insert-section (root)
					(magit-insert-section (verse)
						(magit-insert-heading
							(insert (propertize (format "%s\n" refverse) 'face 'magit-section-heading)))
						
						(magit-insert-section-body
							;; insert verse
							(insert (org-verse--db-get-verse (plist-get ref :book) (plist-get ref :chapter) (plist-get ref :verse)))))
					
					(insert "\n\n")
					
					(if-let ((files (org-verse--grep-file-list refverse)))
							(org-verse--prepare-backlinks files)
						(user-error "No refs with this verse"))
					
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

;; BUTTONIZE BUFFER ----------------------------------------
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
  'help-echo "Clic le boutton pour lire le verset."
  'help-args "test")

(defun org-verse-buttonize-buffer()
  "Turn all verse into button."
  ;; For some reason, overlays accumulate if a buffer
  ;; is visited another time, making emacs slower and slower.
  ;; Hack is to remove them all first.
  ;; remove-overlays does not seem to exist for older emacsen (<23.x.x?)
	(interactive)
  (if (fboundp 'remove-overlays)
      (remove-overlays))
  
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp org-verse-pattern nil t)
      ;;recuperer le contenu de la recherche pour le mettre en titre
      ;;https://github.com/Kinneyzhang/gkroam/blob/b40555f45a844b8fefc419cd43dc9bf63205a0b4/gkroam.el#L708
      (let ((title (match-string-no-properties 0))
						(book (match-string-no-properties 1))
            (chapter (match-string-no-properties 2))
            (verses (match-string-no-properties 3)))
				;;créer les bouttons
				(make-text-button (match-beginning 0)
                          (match-end 0)
                          :type 'org-verse-button
                          ;;inserer le titre recuperer plus haut
                          'title title
													'book book
                          'chapter chapter
                          'verses verses)))))


(defun org-verse--turn-off ()
  "Tear down `org-verse-mode'."
  (when org-verse--keywords
    (font-lock-remove-keywords nil org-verse--keywords)
    (kill-local-variable 'org-verse--keywords)))

(defun org-verse--turn-on ()
  "Set up `org-verse-mode'."
  (let ((regexp org-verse-lexical))
    (when regexp
      (org-verse-buttonize-buffer)
      (set (make-local-variable 'org-verse--keywords) regexp))))


(defconst org-verse-mode-keymap (make-keymap))
(define-key org-verse-mode-keymap (kbd "C-c /") #'org-verse-buttonize-buffer)

;;;###autoload
(define-minor-mode org-verse-mode "Highlight bible verses."
  :init-value nil
  :lighter " verse"
  :keymap org-verse-mode-keymap
  :group 'verse
  (org-verse--turn-off)
	(if org-verse-mode
			(progn
				(org-verse--turn-on)
				(add-hook 'after-save-hook #'org-verse-buttonize-buffer)))

  (when font-lock-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

;;;###autoload
(add-hook 'org-verse-mode-hook
					(function
					 (lambda ()
						 (setq case-fold-search t))))

;;;###autoload
(add-hook 'org-mode-hook 'org-verse-mode)


;;package bootstrap
(provide 'org-verse)

(cl-eval-when (load eval)
	(require 'org-verse-capture))
;;; org-verse.el ends here
