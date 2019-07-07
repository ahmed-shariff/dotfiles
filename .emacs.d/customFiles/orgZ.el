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
						     (:tangle . "yes")))
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
	("j" "Journal entry"
	 entry (file+datetree "~/Documents/org/journal.org")
	 "* %?")
	("e" "Experiment setup information")
	("ej" "Add Journal entry"
	 entry (file+olp+datetree "~/Research/FoodClassification/experiment_log_notes.org")
	 "* [[file:experiment_log.org::#%^{EXP_ID}][%\\1]] %? :%\\1:")
	("el" "Add experiment"
	 entry (file "~/Research/FoodClassification/experiment_log.org")
	 "\n* TODO <<%^{ID}>> %^{Experiment} [%] :@work:exp:%^g\n:PROPERTIES:
  :ID:       %\\1
  :END:\n- %^{Description}\n\n** Notes\n\n** TODO Experiments [/]\n%?\n** TODO Conclusions")
	("b" "Org brain")
	("bp" "Add research paper"
	 entry (file "~/Documents/org/brain/research_papers.org")
	 "* (%^{YEAR}) [[%^{LINK}][%^{TITLE}]]\n  :PROPERTIES:\n  :ID:  %(org-id-new)\n  :YEAR: %\\1 \n  :END:
  \n  - %?")))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

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

(provide 'orgZ)
;;; orgZ.el ends here
