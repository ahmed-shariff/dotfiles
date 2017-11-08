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
;(ido-mode)

(setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p!/@)" "WAIT(w@/!)""|" "DONE(d!)" "CANCELED(c@)")
			  (sequence "ROUNTINE(R)" "|" "ROUNTINE_COMPLETE(r@)" )))

(setq org-agenda-files '("~/Documents/org/Home.org"
			 "~/Documents/org/journal.org"
			 "~/Documents/org/notes.org"
			 "~/Documents/org/work"))
;;(set-register ("~/Documents/org/uniwork.org"
;;			     "~/Documents/org/uni_research.org")

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
	 "* %?")))

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
