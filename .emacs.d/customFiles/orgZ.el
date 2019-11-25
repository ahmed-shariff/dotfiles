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

(setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p!/@)" "WAIT(w@/!)""|" "DONE(d!)" "CANCELED(c@)" "LATER(l@)")
			  (sequence "ROUNTINE(R)" "|" "ROUNTINE_COMPLETE(r@)" )))

(setq org-agenda-files '("~/Documents/org/Home.org"
			 "~/Documents/org/journal.org"
			 "~/Documents/org/notes.org"
			 "~/Documents/org/brain/work"))
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

(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))
(setq org-src-tab-acts-natively t)
(org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)
							 (plantuml . t)
							 (emacs-lisp . t)
							 (python . t)
							 (jupyter . t)
							 (shell .t)))

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
	 entry (file+datetree "~/Documents/org/brain/work/hci.org")
	 "* %?")
	("js" "Journal entry work-scrum"
	 entry (file+datetree "~/Documents/org/brain/work/hci-scrum.org")
	 "* Y:\n1. %?\n* T:\n1. "
	 :jump-to-captured t)
	("jt" "Journal sub entry"
	 entry (file+datetree "~/Documents/org/brain/work/hci.org")
	 "1. %?")
	("e" "Experiment setup information")
	("ej" "Add Journal entry"
	 entry (file+olp+datetree "~/Documents/org/brain/work/hci.org")
	 "* [[file:experiments_log.org::#%^{EXP_ID}][%\\1]] %? :%\\1:")
	("el" "Add experiment"
	 entry (file "~/Documents/org/brain/work/experiments_log.org")
	 "\n\n* TODO <<%^{ID}>> %^{Experiment} [%] :@work:exp_%^{Project id}:\n  :PROPERTIES:
  :CUSTOM_ID:       %\\1
  :PROJECT: [[file:projects.org::#%\\3][%\\3]]
  :PROJECT_ID: %\\3
  :END:\n- %^{Description}\n\n** Notes\n\n** TODO %?\n** TODO Conclusions"
	 :jump-to-captured t)
	("b" "Org brain")
	("bp" "Add research paper"
	 entry (function (lambda () (org-brain-goto "research_papers")));(file "~/Documents/org/brain/research_papers.org")
	 "* (%^{YEAR}) %^{TITLE}\n  :PROPERTIES:\n  :LINK: %^{LINK\}n  :ID:  %(org-id-new)\n  :YEAR: %\\1 \n  :END:
  \n  - %^{LINK}"
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

(require 'org-bullets)

(use-package org-ref
  ; :requires (doi-utils org-ref-pdf org-ref-url-utils org-ref-bibtex org-ref-latex org-ref-arxiv)
  :config
  (setq reftex-default-bibliography '("~/Documents/org/bibliography/references.bib")
	org-ref-bibliography-notes "~/Documents/org/brain/research_papers.org"
	org-ref-default-bibliography '("~/Documents/org/bibliography/references.bib")
	org-ref-pdf-directory "~/Documents/org/bibliography/pdfs/"
	bibtex-completion-bibliography "~/Documents/org/bibliography/references.bib"
	bibtex-completion-library-path "~/Documents/org/bibliography/pdfs/"
	bibtex-completion-notes-path "~/Documents/org/brain/research_papers.org"
	org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
	org-ref-completion-library "org-ref-ivy"
	bibtex-completion-notes-template-one-file
	(format
	 "\n* (${year}) ${title} [${author}]\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :Keywords: ${keywords}\n  :YEAR: ${year}\n  :END:\n\n  - cite:${=key=}")
	doi-utils-open-pdf-after-download nil
	org-ref-note-title-format "* (%y) %t\n  :PROPERTIES:\n  :Custom_ID: %k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n  :END:")
  (defun my/org-ref-notes-function (candidates)
    (let ((key (helm-marked-candidates)))
      (funcall org-ref-notes-function (car key))))

  (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
  (helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 7))

(use-package org-brain
  :init
  (setq org-brain-path "~/Documents/org/brain")
  :bind ("C-c v" . org-brain-visualize)
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 50)
  (defun org-brain-insert-resource-icon (link)
    "Insert an icon, based on content of org-mode LINK."
    (insert (format "%s "
                    (cond ((string-prefix-p "http" link)
                           (cond ((string-match "wikipedia\\.org" link)
                                  (all-the-icons-faicon "wikipedia-w"))
				 ((string-match "github\\.com" link)
                                  (all-the-icons-octicon "mark-github"))
				 ((string-match "vimeo\\.com" link)
                                  (all-the-icons-faicon "vimeo"))
				 ((string-match "youtube\\.com" link)
                                  (all-the-icons-faicon "youtube"))
				 (t
                                  (all-the-icons-faicon "globe"))))
                          ((string-prefix-p "brain:" link)
                           (all-the-icons-fileicon "brain"))
                          (t
                           (all-the-icons-icon-for-file link))))))
  (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)
  (defface aa2u-face '((t . nil))
    "Face for aa2u box drawing characters")
  (advice-add #'aa2u-1c :filter-return
              (lambda (str) (propertize str 'face 'aa2u-face)))
  (defun aa2u-org-brain-buffer ()
    (let ((inhibit-read-only t))
      (make-local-variable 'face-remapping-alist)
      (add-to-list 'face-remapping-alist
                   '(aa2u-face . org-brain-wires))
      (ignore-errors (aa2u (point-min) (point-max)))))
  (with-eval-after-load 'org-brain
    (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;experimnet begin
;; (defun org-brain-insert-visualize-button-tags (old-func &rest args)
;;   "."
;;   (unless (org-brain-filep (car args))
;;     (message "-- %s" (org-brain-keywords (car args)))) ;(cadar args)))
;;   (apply old-func args))
;; (advice-add 'org-brain-insert-visualize-button :around #'org-brain-insert-visualize-button-tags)

;; (advice-remove 'org-brain-insert-visualize-button #'org-brain-insert-visualize-button-tags)
;; (defun org-brain--vis-children-with-tags (entry)
;;   "Insert children of ENTRY.
;; Helper function for `org-brain-visualize'.
;; Appends the first letter of the todo state of the entry"
;;   (let ((tags (org-brain-get-tags entry t)))
;;     (when-let ((children (org-brain-children entry))
;;                (fill-col (if (member org-brain-each-child-on-own-line-tag
;;                                      (org-brain-get-tags entry))
;;                              0
;;                            (eval org-brain-child-linebreak-sexp))))
;;       (insert "\n\n")
;;       (dolist (child (if (member org-brain-no-sort-children-tag tags)
;;                          children
;;                        (sort children org-brain-visualize-sort-function)))
;;         (let ((child-title (org-brain-title child))
;;               (face (if (member entry (org-brain-local-parent child))
;;                         'org-brain-local-child
;;                       'org-brain-child)))
;;           (when (> (+ (current-column) (length child-title)) fill-col)
;;             (insert "\n"))
;; 	  (let ((kwd-setting
;; 		 (unless (org-brain-filep child)
;; 		   (org-with-point-at
;; 		       (org-brain-entry-marker child)
;; 		     (let ((kwd (org-entry-get (point) "TODO")))
;; 		       (if kwd
;; 			   (list kwd (org-get-todo-face kwd))
;; 			 nil))))))
;; 	    (when kwd-setting
;; 	      (insert (propertize (substring (first kwd-setting) 0 1) 'face (second kwd-setting)) " ")))
;;           (org-brain-insert-visualize-button child face)
;;           (insert "  "))))))
;; (advice-add 'org-brain--vis-children :override #'org-brain--vis-children-with-tags)

;;(advice-remove 'org-brain--vis-children #'org-brain--vis-children-with-tags)

(defun org-brain-insert-visualize-button-with-tags (entry &optional face)
  "Insert a button, running `org-brain-visualize' on ENTRY when clicked.
FACE is sent to `org-brain-display-face' and sets the face of the button.
Appends the todo state of the entry being visualized."
  (let ((annotation (org-brain-get-edge-annotation org-brain--vis-entry
                                                   entry
                                                   org-brain--vis-entry-keywords)))
    (let ((kwd-setting
	   (unless (org-brain-filep entry)
	     (org-with-point-at
		 (org-brain-entry-marker entry)
	       (let ((kwd (org-entry-get (point) "TODO")))
		 (if kwd
		     (list kwd (org-get-todo-face kwd))
		   nil))))))
      (when kwd-setting
	(insert (propertize (substring (first kwd-setting) 0 1) 'face (second kwd-setting)) " ")))
    (insert-text-button
     (org-brain-title entry)
     'action (lambda (_x) (org-brain-visualize entry))
     'id (org-brain-entry-identifier entry)
     'follow-link t
     'help-echo annotation
     'aa2u-text t
     'face (org-brain-display-face entry face annotation))))
(advice-add 'org-brain-insert-visualize-button :override #'org-brain-insert-visualize-button-with-tags)

;; (advice-remove 'org-brain-insert-visualize-button #'org-brain-insert-visualize-button-with-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;experimnet end

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(setq org-noter-property-doc-file "INTERLEAVE_PDF"
      org-noter-property-note-location "INTERLEAVE_PAGE_NOTE")

(require 'org-download)
(setq org-download-screenshot-method "scrot")
(defun pdf-crop-image (event &optional switch-back)
  "EVENT SWITCH-BACK."
  (interactive "@e")
  (setq current-b (buffer-name))
  (progn (pdf-view-mouse-set-region-rectangle event)
	 (message "%s" pdf-view-active-region)
	 (pdf-view-extract-region-image pdf-view-active-region
					(pdf-view-current-page)
					(pdf-view-image-size)
					(get-buffer-create "teste")
					nil)))
	 ;; (set-buffer "teste")
	 ;; (switch-to-buffer "taste")))
	 ;; (write-file "/tmp/screenshot.png" nil)))
	 ;; (kill-buffer "screenshot.png")
	 ;; (set-buffer current-b)
	 ;; (org-noter-insert-note)
	 ;; (org-download-screenshot)
	 ;; (if switch-back
	 ;;     (switch-to-buffer-other-frame current-b))))
(define-key pdf-view-mode-map [C-M-down-mouse-1] 'pdf-crop-image)
(setq org-export-allow-bind-keywords t
      org-latex-image-default-option "scale=0.6")

(defun set-property-for-level-in-region (level property value)
  "."
  (interactive "nLevel: \nsPropertyb: \nsValue: ")
  (message "%s %s %s %s %s" level property value (region-beginning) (region-end))
  (org-map-entries
   (lambda ()
     (org-entry-put (point) (upcase property) value))
     ;;(message "%s" (org-entry-properties)))
   (format "LEVEL=%s" level)
   'region))
     

(defun research-papers-configure ()
  "."
  (interactive)
  (org-map-entries (lambda ()
		     (let ((link (org-entry-get (point) "LINK"))
			   (cite-key (org-entry-get (point) "Custom_ID"))
			   (dir org-ref-pdf-directory)
			   (tags (org-get-tags)))
		       (org-entry-put (point) "ATTACH_DIR" dir)
		       (org-id-get-create)
		       (if (and link cite-key)
			   (let ((out-file-name (concatenate 'string cite-key ".pdf")))
			     (when (condition-case nil
				       (amsha/downlad-raname-move-file link out-file-name dir)
				     (file-already-exists t))
			       (org-entry-put (point) "Attachment" out-file-name)
			       (setq tags (append tags '("ATTACH")))
			       (org-entry-put (point) "INTERLEAVE_PDF" (expand-file-name out-file-name dir))))
			 (progn
			   (setq tags (if link (delete "NO_LINK" tags) (append tags '("NO_LINK"))))
			   (setq tags (if link (delete "NO_CITE_KEY" tags) (append tags '("NO_CITE_KEY"))))))
		       (setq tags
			     (if (org-entry-get (point) "BRAIN_PARENTS")
			         (delete "NO_PARENTS" tags)
				 (append tags '("NO_PARENTS"))))
		       (org-set-tags (delete "nosiblings" (delete-dups tags)))))
		   "LEVEL=1")
  (org-brain-update-id-locations))

(defun copy-related-research-papers (parent-id)
  "PARENT-ID."
  (interactive "sParent-id: ")
  (let ((out-dir (expand-file-name parent-id "~/Downloads")))
    (condition-case nil 
	(make-directory out-dir)
      (file-already-exists
       (progn
	 (message "Deleting directory and creating anew: %s" out-dir) 
	 (delete-directory out-dir t)
	 (make-directory out-dir))))
    (message "Copying files to %s" out-dir)
    (org-map-entries (lambda ()
		       (let ((file-path (org-entry-get (point) "INTERLEAVE_PDF")))
			 (when (and file-path
				    (equalp parent-id (org-entry-get (point) "BRAIN_PARENTS")))
			   (copy-file file-path
				      (expand-file-name (file-name-nondirectory file-path)
							out-dir))))))
    (dired out-dir)))


(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map "\C-c!" 'org-time-stamp-inactive)
	    (define-key org-mode-map "\C-coo" 'org-noter)
	    (define-key org-mode-map "\C-cop" 'org-brain-add-parent)
	    (define-key org-mode-map "\C-coc" 'research-papers-configure)
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
