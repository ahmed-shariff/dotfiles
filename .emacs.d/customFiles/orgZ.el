;;; orgZ.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;my custom org setup
;;; Code:
(require 'org)
(require 'org-capture)
(require 'org-capture-pop-frame)
;(ido-mode)

(setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p!/@)" "WAIT(w@/!)""|" "DONE(d!)" "CANCELED(c@)")
			  (sequence "ROUNTINE(R)" "|" "ROUNTINE_COMPLETE(r@)" )))

(setq org-agenda-files '("~/Documents/org/Home.org"
			 "~/Documents/org/journal.org"
			 "~/Documents/org/notes.org"
			 "~/Documents/org/work"))
;;(set-register ("~/Documents/org/uniwork.org"
;;			     "~/Documents/org/uni_research.org")

(setq org-agenda-custom-commands
      '(("c" . "My custom queries")
	("ci" tags-todo "LEVEL=1&+exp/!INPROGRESS"
	 ((org-agenda-files `("~/Research/FoodClassification/experiment_log.org"))
	  (org-agenda-filter-by-top-headline)))
	("ct" tags-todo "LEVEL=1&+exp/!TODO|WAIT"
	 ((org-agenda-files `("~/Research/FoodClassification/experiment_log.org"))
	  (org-agenda-filter-by-top-headline)))
	("ca" tags-todo "LEVEL=1&+exp"
	 ((org-agenda-files `("~/Research/FoodClassification/experiment_log.org"))
	  (org-agenda-filter-by-top-headline)))
	("cd" tags-todo "LEVEL=1&+exp/!DONE"
	 ((org-agenda-files `("~/Research/FoodClassification/experiment_log.org"))
	  (org-agenda-filter-by-top-headline)))))
	
(use-package org-ref
  :config
  (setq reftex-default-bibliography '("~/Research/personal-stuff/citations.bib")))

(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))
(setq org-src-tab-acts-natively t)
(org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)
							 (plantuml . t)
							 (emacs-lisp . t)
							 (python . t)
							 (jupyter . t)))

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
						     (:session . "py")
						     (:kernel . "python3")
						     (:tangle . "yes")
						     (:exports . "both")))
;; (defun my-org-confirm-babel-evaluate (lang bdy)
;;   "Function to eval plantuml blocks.
;; LANG
;; BDY"
;;   (not (string= lang "plantuml")))
(setq org-confirm-babel-evaluate nil);;'my-org-confirm-babel-evaluate)
(setq org-latex-image-default-width "")
(setq org-startup-with-inline-images t)

(setq org-tag-persistent-alist '(("@work" . ?w) ("@home" . ?h) ("@mobile" . ?m)))

(setq org-default-notes-file "~/Documents/org/notes.org")

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))
			   (org-c-refile-targets . (:maxlevel . 6))))

(setq org-capture-templates
      '(("i" "hmmmm....somthing!*light bulb*->TO THE NOTES"
	 entry (file+datetree "~/Documents/org/notes.org")
	 "* NOTE %^g\n\tAdded: %U\n\t%?")
	("t" "A thing i have to do(a wonderfull epiphany? 3:))->TO THE NOTES"
	 entry (file+datetree "~/Documents/org/notes.org")
	 "* TODO %^{Description} %^g\n\tAdded: %U\n\t%?")
	("j" "Journal entry")
	("jg" "Journal entry general"
	 entry (file+datetree "~/Documents/org/journal.org")
	 "* %?")
	("jw" "Journal entry work"
	 entry (file+datetree "~/Documents/org/work/hci.org")
	 "* %?")
	("js" "Journal entry work-scrum"
	 entry (file+datetree "~/Documents/org/work/hci-scrum.org")
	 "* Y:\n1. %?\n* T:\n1. "
	 :jump-to-captured t)
	("jt" "Journal sub entry"
	 entry (file+datetree "~/Documents/org/work/hci.org")
	 "1. %?")
	("e" "Experiment setup information")
	("ej" "Add Journal entry"
	 entry (file+olp+datetree "~/Documents/org/work/hci.org")
	 "* [[file:experiments_log.org::#%^{EXP_ID}][%\\1]] %? :%\\1:")
	("el" "Add experiment"
	 entry (file "~/Documents/org/work/experiments_log.org")
	 "\n\n* TODO <<%^{ID}>> %^{Experiment} [%] :@work:exp:%^{Project id}:\n  :PROPERTIES:
  :CUSTOM_ID:       %\\1
  :PROJECT: [[file:projects.org::#%\\3][%\\3]]
  :PROJECT_ID: %\\3
  :END:\n- %^{Description}\n\n** Notes\n\n** TODO %?\n** TODO Conclusions"
	 :jump-to-captured t)
	("b" "Org brain")
	("bp" "Add research paper"
	 entry (function (lambda () (org-brain-goto "research_papers")));(file "~/Documents/org/brain/research_papers.org")
	 "* (%^{YEAR}) %^{TITLE}\n  :PROPERTIES:\n  :ID:  %(org-id-new)\n  :YEAR: %\\1 \n  :END:
  \n  - [[%^{LINK}]]"
	 :jump-to-captured t)))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; ;; from https://stackoverflow.com/questions/13967876/how-to-restrict-a-function-to-a-subtree-in-emacs-org-mode
;; (defun my-ido-find-org-attach ()
;;   "Find files in org-attachment directory"
;;   (interactive)
;;   (let* ((enable-recursive-minibuffers t)
;;          (files (find-lisp-find-files org-attach-directory "."))
;;          (file-assoc-list
;;           (mapcar (lambda (x)
;;                     (cons (file-name-nondirectory x)
;;                           x))
;;                   files))
;;          (filename-list
;;           (remove-duplicates (mapcar #'car file-assoc-list)
;;                              :test #'string=))
;;          (filename (ido-completing-read "Org attachments: " filename-list nil t))
;;          (longname (cdr (assoc filename file-assoc-list))))
;;     (ido-set-current-directory
;;      (if (file-directory-p longname)
;;          longname
;;        (file-name-directory longname)))
;;     (setq ido-exit 'refresh
;;           ido-text-init ido-text
;;           ido-rotate-temp t)
;;     (exit-minibuffer)))

; taken from https://emacs.stackexchange.com/questions/48533/calling-a-fuction-after-org-capturing
;; (defun amsha/org-capture-finalize ()
;;   (let ((key  (plist-get org-capture-plist :key))
;;         (desc (plist-get org-capture-plist :description)))
;;     (unless org-note-abort
;;       (message "Template with key %s and description “%s” run successfully" org-capture-plist desc))))

;; (add-hook 'org-capture-mode-hook 'amsha/org-capture-finalize)


(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map "\C-c!" 'org-time-stamp-inactive)
	    (flyspell-mode t)))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done 'note)
(setq org-log-into-drawer t)
(setq org-deadline-warning-days 2)
(setq org-clock-idle-time 10)
(setq org-return-follows-link t)
(setq org-return-follows-link t)
(setq org-refile-use-outline-path "file")
(setq org-outline-path-complete-in-steps t)(setq org-completion-use-ido t)
(setq org-attach-directory "~/Documents/org/documents/")
(provide 'orgZ)
;;; orgZ.el ends here
