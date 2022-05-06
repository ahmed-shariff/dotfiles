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
;; (require 'org-capture-pop-frame)
;(ido-mode)

(use-package org-capture-pop-frame
  :straight (org-capture-pop-frame :type git :host github :repo "tumashu/org-capture-pop-frame"
                                   :fork (:host github :repo "ahmed-shariff/org-capture-pop-frame")))

(use-package org-protocol
  :ensure nil
  :straight nil
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("add doi link"
                 :protocol "add-doi-pdf"
                 :function add-doi-and-pdf
                 :kill-client t))

  (defun add-doi-and-pdf (data)
    "DATA exepcts to be an alist with keys :url and :filename."
    (message "Trying to add: %s" data)
    (let* ((url (plist-get data :url))
           (file-name (format "file:///%s" (expand-file-name (plist-get data :filename))))
           doi)
      (save-match-data 
        (if (and (string-match "\\(10\\.[0-9]\\{4\\}\\(/\\|%2F\\)\\([a-z]\\|[0-9]\\|_\\|-\\|\\.\\)+\\)" url)
                 (setq doi (s-replace-regexp
                            "\\.$" ""
                            (s-replace-regexp
                             "\\.pdf$" ""
                             (s-replace "%2F" "/" (match-string 1 url))))))
            (progn
              (save-excursion
                (ignore-errors (doi-add-bibtex-entry doi (car bibtex-completion-bibliography)))
                (doi-utils-open-bibtex doi)
                (org-ref-open-bibtex-notes)
                (org-set-property "LINK" file-name)
                (research-papers-configure)))
          (if (string-match "arxiv\\.org.*pdf$" url)
              (arxiv-add-bibtex-entry-with-note url (car bibtex-completion-bibliography))
            (progn
              (push file-name kill-ring)
              (message "No valid DOI: Adding %s to kill ring" file-name))))))
    (find-file bibtex-completion-notes-path)
    ;; returning nil to avoid a file buffer being opened
    nil))

(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-startup-folded 'content)

(setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p!/@)" "WAIT(w@/!)" "IDEA(i)" "|" "DONE(d!)" "CANCELED(c@)" "LATER(l@)")
			  (sequence "ROUNTINE(R)" "|" "ROUNTINE_COMPLETE(r@)" )))

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

(setq org-babel-load-languages
      (append org-babel-load-languages '((ruby . t)
				         (plantuml . t)
				         (emacs-lisp . t)
				         (python . t)
				         (shell . t))))

(when (gethash 'use-jupyter configurations t)
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
						       (:session . "py")
						       (:kernel . "python3")
						       (:tangle . "yes")
						       (:exports . "both")))
  (use-package ox-ipynb
    :straight (ox-ipynb :type git :host github :repo "jkitchin/ox-ipynb"))
  (push '(jupyter . t) org-babel-load-languages))


;; from https://emacs.stackexchange.com/questions/42471/how-to-export-markdown-from-org-mode-with-syntax
(defun org-md-example-block-with-syntax (example-block _content info)
  "Transcode element EXAMPLE-BLOCK as ```lang ...'''."
  (format "```%s\n%s\n```"
          (org-element-property :language example-block)
          (org-remove-indentation
           (org-export-format-code-default example-block info))))

(advice-add 'org-md-example-block :override #'org-md-example-block-with-syntax)

;; (defun org-md-link-github-syntax (link desc info)
;;   "Transcode LINK object into Markdown format.
;; DESC is the description part of the link, or the empty string.
;; INFO is a plist holding contextual information.  See
;; `org-export-data'."
;;   (let* ((link-org-files-as-md
;; 	  (lambda (raw-path)
;; 	    ;; Treat links to `file.org' as links to `file.md'.
;; 	    (if (string= ".org" (downcase (file-name-extension raw-path ".")))
;; 		(concat (file-name-sans-extension raw-path) ".md")
;; 	      raw-path)))
;; 	 (type (org-element-property :type link))
;; 	 (raw-path (org-element-property :path link))
;; 	 (path (cond
;; 		((member type '("http" "https" "ftp" "mailto"))
;; 		 (concat type ":" raw-path))
;; 		((string-equal  type "file")
;; 		 (org-export-file-uri (funcall link-org-files-as-md raw-path)))
;; 		(t raw-path))))
;;     (cond
;;      ;; Link type is handled by a special function.
;;      ((org-export-custom-protocol-maybe link desc 'md info))
;;      ((member type '("custom-id" "id" "fuzzy"))
;;       (let ((destination (if (string= type "fuzzy")
;; 			     (org-export-resolve-fuzzy-link link info)
;; 			   (org-export-resolve-id-link link info))))
;; 	(pcase (org-element-type destination)
;; 	  (`plain-text			; External file.
;; 	   (let ((path (funcall link-org-files-as-md destination)))
;; 	     (if (not desc) (format "<%s>" path)
;; 	       (format "[%s](%s)" desc path))))
;; 	  (`headline
;; 	    ;; Description.
;; 	    (let ((-description (cond ((org-string-nw-p desc))
;; 		                      ((org-export-numbered-headline-p destination info)
;; 		                       (mapconcat #'number-to-string
;; 			                          (org-export-get-headline-number destination info)
;; 			                          "."))
;; 		                      (t (org-export-data (org-element-property :title destination)
;; 				                          info))))
;; 	          ;; Reference.
;;                   (-ref (->>
;;                             (org-export-data (org-element-property :title destination)
;; 				             info)
;;                           (s-replace-regexp "[^[:alnum:]-\(\_\|\s\)]" "" )
;;                           (s-replace-regexp "\s" "-")
;;                           (s-downcase))))
;;               (format
;; 	       "[%s](#%s)" -description -ref)))
;; 	  (_
;; 	   (let ((description
;; 		  (or (org-string-nw-p desc)
;; 		      (let ((number (org-export-get-ordinal destination info)))
;; 			(cond
;; 			 ((not number) nil)
;; 			 ((atom number) (number-to-string number))
;; 			 (t (mapconcat #'number-to-string number ".")))))))
;; 	     (when description
;; 	       (format "[%s](#%s)"
;; 		       description
;; 		       (org-export-get-reference destination info))))))))
;;      ((org-export-inline-image-p link org-html-inline-image-rules)
;;       (let ((path (cond ((not (string-equal type "file"))
;; 			 (concat type ":" raw-path))
;; 			((not (file-name-absolute-p raw-path)) raw-path)
;; 			(t (expand-file-name raw-path))))
;; 	    (caption (org-export-data
;; 		      (org-export-get-caption
;; 		       (org-export-get-parent-element link))
;; 		      info)))
;; 	(format "![img](%s)"
;; 		(if (not (org-string-nw-p caption)) path
;; 		  (format "%s \"%s\"" path caption)))))
;;      ((string= type "coderef")
;;       (format (org-export-get-coderef-format path desc)
;; 	      (org-export-resolve-coderef path info)))
;;      ((string= type "radio")
;;       (let ((destination (org-export-resolve-radio-link link info)))
;; 	(if (not destination) desc
;; 	  (format "<a href=\"#%s\">%s</a>"
;; 		  (org-export-get-reference destination info)
;; 		  desc))))
;;      (t (if (not desc) (format "<%s>" path)
;; 	  (format "[%s](%s)" desc path))))))

;; (advice-add 'org-md-link :override #'org-md-link-github-syntax)

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

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

(setq org-refile-targets '((org-agenda-files :maxlevel . 6)))
			   ;(org-c-refile-targets :maxlevel . 6)))

(defun org-ask-title-location (&optional prompt)
  "From  https://stackoverflow.com/questions/9005843/interactively-enter-headline-under-which-to-place-an-entry-using-capture."
  (let* ((prompt (or prompt "Project "))
	 (org-refile-targets '((nil :maxlevel . 1)))
         (hd (condition-case nil
                 (car (let ((in (org-refile-get-location prompt nil t)))
			(message "%s" in)
			in))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))))

;;  ***************************************************************

(defun org-ask-id (file prompt property)
  "."
  (save-window-excursion
    (find-file file)
    (org-ask-title-location prompt)
    (org-entry-get (point) property)))

(defun orgz--org-templates-get-project-id ()
  "Get the new project id."
  (number-to-string
   (1+ (apply #'max
              (org-ql-select (expand-file-name "work/projects.org" org-brain-path) `(level 1)
                :action (lambda ()
                          (condition-case nil
                              (string-to-number (cadr (s-match "<<\\([0-9]+\\)>>" (org-brain-title (org-brain-entry-at-pt)))))
                            (wrong-type-argument 0))))))))

(defun orgz--org-templates-get-sprint-id ()
  "Get the new sprint id."
  (number-to-string
   (1+
    (apply #'max
           (org-ql-select
             (expand-file-name "work/projects.org" org-brain-path)
             `(parent ,(s-replace-regexp "/$" "" (car org-refile-history)))
             :action  (lambda ()
                        (condition-case nil
                            (string-to-number (cadr (s-match "Sprint \\([0-9]+\\):"
                                                             (org-brain-title (org-brain-entry-at-pt)))))
                          (wrong-type-argument 0))))))))

(defun org-ask-experiment-id ()
  "."
  (save-window-excursion
    (find-file "~/Documents/org/brain/work/experiments_log.org")
    (org-ask-title-location  "Experiment ")
    (org-entry-get (point) "CUSTOM_ID"))
  ;;(org-ask-id "~/Documents/org/brain/work/experiments_log.org"  "Experiment " "CUSTOM_ID"))
)

(defun org-ask-project-id ()
  "."
  (org-ask-id "~/Documents/org/brain/work/projects.org"  "Project " "CUSTOM_ID"))

(defun org-ask-task-board ()
  "Move the cursor to a location in a task board."
  (let* ((project-boards (mapcar (lambda (file) (cons (format "%-10s %s"
                                                              (propertize (f-base (f-parent (f-parent file)))  'face 'marginalia-documentation)
                                                              (file-name-base file))
                                                       file))
                                 (--keep
                                  (when (not (s-contains-p "#" it)) it)
                                  (f-glob "*/project_boards/*.org" org-brain-path))))
         (board-file (cdr (assoc (completing-read "Select poject board: " project-boards) project-boards))))
    (find-file board-file)
    (goto-char (point-max))))

(defun board-task-location ()
  "Return a org title with board task after prompting for it."
  (let* ((project-boards (--keep (when (not (s-contains-p "#" it)) it)
                                 (f-glob "*/project_boards/*.org" org-brain-path)))
         (targets
          (org-ql-select project-boards `(level 1)
            :action (lambda ()
                      (let* ((headline-plist (cadr (org-element-headline-parser (point))))
                             (title (org-entry-get (point) "ITEM"))
                             (full-file-name (buffer-file-name))
                             (file-name (format "%s/%s" (f-base (f-parent (f-parent full-file-name))) (file-name-base full-file-name)))
                             (todo-state (or (plist-get headline-plist :todo-keyword) "")))
                        (list (format "%-10s  %-30s %s"
                                      (propertize todo-state 'face (org-get-todo-face todo-state))
                                      (propertize file-name 'face 'marginalia-documentation)
                                      title)
                              title
                              (org-id-get-create))))))
         (target (progn
                   (assoc (completing-read "Select task: " (org-brain--targets-with-metadata targets) nil t) targets))))
    (format "* [[id:%s][%s]]  %%?"
            (nth 2 target)
            (nth 1 target))))
    
          
;; (defun org-ask-location ()
;;   org-project-sprint-target-heading) 

(setq org-capture-templates
      '(("i" "hmmmm....somthing!*light bulb*->TO THE NOTES"
	 entry (file+olp+datetree "~/Documents/org/notes.org")
	 "* NOTE %^g\n\tAdded: %U\n\t%?")
	("t" "A thing i have to do(a wonderfull epiphany? 3:))->TO THE NOTES"
	 entry (file "~/Documents/org/notes.org")
	 "* TODO %^{Description} %^g\n\tAdded: %U\n\t%?")
	("j" "Journal entry")
	("jg" "Journal entry general"
	 entry (file+olp+datetree "~/Documents/org/journal.org")
	 "* %?")
	("jw" "Journal entry work"
	 entry (file+olp+datetree "~/Documents/org/brain/work/notes.org")
	 "* %?")
	("js" "Journal entry work-scrum"
	 entry (file+olp+datetree "~/Documents/org/brain/work/scrum.org")
	 "* Y:\n1. %?\n* T:\n1. "
	 :jump-to-captured t)
	("jt" "Journal sub entry"
	 entry (file+olp+datetree "~/Documents/org/brain/work/notes.org")
	 "1. %?")
	("e" "Experiment setup information")
	("ej" "Add Journal entry")
        ("ejt" "for task"
	 entry (file+olp+datetree "~/Documents/org/brain/work/notes.org")
	 "%(board-task-location)")
	("eje" "for experiment"
	 entry (file+olp+datetree "~/Documents/org/brain/work/notes.org")
         "* [[file:experiments_log.org::#%^{EXP_ID}][%\\1]] %? :e%\\1:")
        ("el" "Add experiment"
	 entry (file "~/Documents/org/brain/work/experiments_log.org")
	 "\n\n* TODO <<%^{ID}>> %^{Experiment} [%] :@work:exp_%^{Project_id}:\n  :PROPERTIES:
  :CUSTOM_ID:       %\\1
  :PROJECT: [[file:projects.org::#%\\3][%\\3]]
  :PROJECT_ID: %\\3
  :SPRINT: %^{Sprint ID}
  :END:\n- %^{Description}\n\n** Notes\n\n** TODO %?\n** TODO Conclusions"
	 :jump-to-captured t)
	("es" "Add sprint"
	 entry (file+function "~/Documents/org/brain/work/projects.org" org-ask-title-location)
	 "** TODO Sprint %(orgz--org-templates-get-sprint-id): %^{TITLE}
   :PROPERTIES:
   :EXPORT_TOC: nil
   :EXPORT_TITLE: %\\1
   :EXPORT_OPTIONS: H:2
   :EXPORT_AUTHOR:
   :START_DATE: %u
   :END_DATE:
   :ID:       %(org-id-new)
   :END:
*** From previous:
    - %?
*** Sprint goal:
*** Related experiments:
*** Remarks:
" :jump-to-captured t)
	("ep" "Add project"
	 entry (file "~/Documents/org/brain/work/projects.org")
	 "* TODO <<%(orgz--org-templates-get-project-id)>> %^{TITLE}
  :PROPERTIES:
  :CUSTOM_ID: %(orgz--org-templates-get-project-id)
  :ID:       %(org-id-new)
  :END:
** %\\1 literature
   :PROPERTIES:
   :ID:       %(org-id-new)
   :END:
%?
"
	 :jump-to-captured t)
        ("et" "Add task"
	 entry (function org-ask-task-board)
	 "* TODO %^{TITLE}
  :PROPERTIES:
  :ID:       %(org-id-new)
  :END:
%?
"
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

(use-package bibtex-completion)


;; On windows when the `cygwin1.dll mismatch issue` issue happens, This is solved by manually running the command seen in the *compilation* buffer
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename org-brain-path))
  (org-roam-dailies-directory "dailies")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "%(board-task-location)"
      :target (file+datetree
               "~/Documents/org/brain/work/notes.org"
               "day"))
     ("p" "personal" entry "%(board-task-location)"
      :target (file+datetree
               "~/Documents/org/brain/personal/notes.org"
               "day"))))
  (org-roam-capture-templates
   '(("d" "default" entry "* ${title}%?
  :PROPERTIES:
  :ID:       %(org-id-new)
  :END:"
      :target
      (file+head+olp
       (lambda () (completing-read "File: "
                                   (f-glob "*.org" (file-truename org-brain-path))
                                   nil nil))
       nil nil)
      :prepend t
      :kill-buffer t)
     ("n" "new note" plain "%?"
     :target (file+head "~/Documents/org/brain/roam-notes/${slug}-%<%Y%m%d%H%M%S>.org"
                        "#+title: ${title}\n")
     :unnarrowed t)))
  (org-roam-node-display-template (concat (propertize " ${file:50}" 'face 'hl-line) (propertize "    ${title:90}   " 'face 'bold) (propertize "${tags:30}  " 'face 'org-tag)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ;; org-roam-bibtex
         ("C-c n b" . orb-insert-link))
  :config
  ;; (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)

  (defun org-roam-backlinks-get-brain-children (node)
    "Get org-brain-children as backlinks.
Modified `org-roam-backlink-get'."
    (let* ((node-id (org-roam-node-id node))
           (backlinks
            ;; Getting brain-children and convert them to roam backlinks.
            (mapcar (lambda (entry)
                      (--> (org-brain-entry-marker entry)
                           (with-current-buffer (marker-buffer it)
                             (goto-char (marker-position it))
                             (list (org-id-get)
                                   node-id
                                   (point)
                                   ;; This can error if link is not under any headline
                                   ;; copied from `org-roam-db-insert-link'
                                   (list
                                    :outline
                                    (ignore-errors
                                      (org-get-outline-path 'with-self 'use-cache)))))))
                    (org-brain-children (save-excursion
                                          (org-id-goto node-id)
                                          (org-brain-entry-at-pt))))))
      (cl-loop for backlink in backlinks
               collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
                         (org-roam-populate
                          (org-roam-backlink-create
                           :source-node (org-roam-node-create :id source-id)
                           :target-node (org-roam-node-create :id dest-id)
                           :point pos
                           :properties properties))))))

  (defun org-roam-brain-children-section (node)
    "The brain children section for NODE.
Copied  from `org-roam-backlink-get'."
    (when-let ((backlinks (seq-sort #'org-roam-backlinks-sort (org-roam-backlinks-get-brain-children node))))
      (magit-insert-section (org-roam-backlinks)
        (magit-insert-heading "Brain children:")
        (dolist (backlink backlinks)
          (org-roam-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink)))
        (insert ?\n))))

  (push #'org-roam-brain-children-section org-roam-mode-sections)

  (defun org-roam-subtree-aware-preview-function ()
    "Same as `org-roam-preview-default-function', but gets entire subtree in research_papers or notes."
    (if (member (org-roam-node-file (org-roam-node-from-id
                                     ;; move up the tree until an el with id is found
                                     (do () ((org-id-get) (org-id-get)) (org-up-element))))
                (list
                 (file-truename bibtex-completion-notes-path) (file-truename "~/Documents/org/brain/work/notes.org") (file-truename "~/Documents/org/brain/personal/notes.org")))
        (let ((beg (progn (org-roam-end-of-meta-data t)
                          (point)))
              (end (progn (org-end-of-subtree)
                          (point))))
          (-reduce 
           (lambda (str el)
             (s-replace-regexp (format "\n *:%s:.*$" el) "" str))
           ;; remove properties not interested. If prop drawer is empty at the end, remove drawer itself
           (list (string-trim (buffer-substring-no-properties beg end)) "INTERLEAVE_PAGE_NOTE" "BRAIN_CHILDREN" "BRAIN_PARENTS" "PROPERTIES:\n *:END")))
      (org-roam-preview-default-function)))

  (setq org-roam-preview-function 'org-roam-subtree-aware-preview-function)

  (defun org-roam-list-notes (filters)
    "Filter based on the list of ids (FILTER) in the notes files."
    (interactive (list (org-brain-choose-entries "Filter topics:" 'all)))

    (let* ((entries (-non-nil (-replace-where #'org-brain-filep (lambda (el) nil) filters)))
           (names (s-join "," (--map (cadr it) entries)))
           (title (format "(%s)" names))
           (buffer (get-buffer-create (format "*notes: %s*" names)))
           (ids (apply #'vector (--map (caddr it) entries))))
      ;; copied  from `org-roam-buffer-render-contents'
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-roam-mode)
          (setq-local default-directory org-roam-buffer-current-directory)
          (setq-local org-roam-directory org-roam-buffer-current-directory)
          (org-roam-buffer-set-header-line-format title)
          (magit-insert-section (org-roam)
            (magit-insert-heading)
            (dolist (entry 
                     (org-roam-db-query
                      [:select [links:source links:pos links:properties]
                               :from links :inner :join nodes :on (= links:source nodes:id)
                               :where (and (in links:dest $v1) (in nodes:file $v2))]
                      ids
                      (vector
                       (file-truename "~/Documents/org/brain/personal/notes.org")
                       (file-truename "~/Documents/org/brain/work/notes.org"))))
              (pcase-let ((`(,source ,pos ,properties) entry))
                (org-roam-node-insert-section :source-node (org-roam-node-from-id source) :point pos :properties properties))
              (insert ?\n))
            (run-hooks 'org-roam-buffer-postrender-functions)
            (goto-char 0))))
      (display-buffer buffer))))

(use-package org-roam-bibtex
  :after org-roam
  :custom
  (orb-roam-ref-format "org-ref-v3")
  (orb-insert-interface "ivy-bibtex"))

(use-package ivy-bibtex
  :after (org-ref)
  :config
  (require 'org-ref-ivy)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
        org-ref-insert-cite-function 'org-ref-cite-insert-ivy
        org-ref-insert-label-function 'org-ref-insert-label-link
        org-ref-insert-ref-function 'org-ref-insert-ref-link
        org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))

(use-package ox-pandoc)

(use-package org-ref
  :after ox-pandoc
  :demand
  :bind (:map org-mode-map
              ("C-c ]" . org-ref-insert-link))
  ; :requires (doi-utils org-ref-pdf org-ref-url-utils org-ref-bibtex org-ref-latex org-ref-arxiv)
  :config
  (setq bibtex-completion-notes-path "~/Documents/org/brain/research_papers.org"
	bibtex-completion-bibliography '("~/Documents/org/bibliography/references.bib")
        bibtex-completion-library-path "~/Documents/org/bibliography/pdfs/"
        reftex-default-bibliography bibtex-completion-bibliography
	
	org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
	bibtex-completion-notes-template-one-file
	(format
	 "\n* (${year}) ${title} [${author}]\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :Keywords: ${keywords}\n  :LINK: ${pdf}\n  :YEAR: ${year}\n  :END:\n\n  - cite:${=key=}")
	doi-utils-open-pdf-after-download nil
        doi-utils-download-pdf nil)

  (defun org-ref-get-bibtex-key-under-cursor-with-latex (old-func &rest)
    (if (derived-mode-p 'latex-mode)
        (bibtex-completion-get-key-latex)
      (funcall old-func)))

  (advice-add 'org-ref-get-bibtex-key-under-cursor :around #'org-ref-get-bibtex-key-under-cursor-with-latex)

  (defun org-ref-latex-click ()
    "Open bibtex hydra in latex buffer."
    (interactive)
    (org-ref-citation-hydra/body))

  (defun org-ref-copy-formated-from-query (query)
    "QUERY."
    (interactive (list (read (read-string "Query:"))))
    (let ((buffer (get-buffer-create (format "*temp-org-ref-copy-formated-from-query-%s*" (random)))))
      (with-current-buffer buffer 
          (--map (insert (format "\n\n%s" it))
                 (org-ql-select bibtex-completion-notes-path query
                   :action (lambda ()
                             ;; from org-ref-open-url-at-point
                             (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
                                    (key (org-entry-get (point) "Custom_ID"))
	                            (results (org-ref-get-bibtex-key-and-file key))
                                    (bibfile (cdr results))
                                    bib-url)
                               (save-excursion
                                 (with-temp-buffer
                                   (insert-file-contents bibfile)
                                   (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                                   (bibtex-search-entry key)
                                   ;; I like this better than bibtex-url which does not always find
                                   ;; the urls
                                   (catch 'done
                                     (let ((url (s-trim (bibtex-autokey-get-field "url"))))
                                       (unless (s-blank? url)
                                         (setq bib-url url)
                                         (throw 'done nil)))

                                     (let ((doi (s-trim (bibtex-autokey-get-field "doi"))))
                                       (unless (s-blank? doi)
                                         (if (string-match "^http" doi)
                                             (setq bib-url doi)
                                           (setq bib-url (format "http://dx.doi.org/%s" doi)))
                                         (throw 'done nil))))))
                               (format "%s \n URL: %s"
                                       (bibtex-completion-apa-format-reference key)
                                       bib-url))))))
      (switch-to-buffer buffer))))

(defun doi-add-bibtex-entry-with-note ()
  "."
  (interactive)
  (call-interactively #'doi-utils-add-bibtex-entry-from-doi)
  (find-file (car bibtex-completion-bibliography))
  (org-ref-open-bibtex-notes)
  (org-set-property "LINK" (completing-read "LINK: " nil)))

(use-package org-noter ;;:quelpa (org-noter :fetcher github :repo "ahmed-shariff/org-noter")
  :straight (org-noter :type git :host github :repo "weirdNox/org-noter"
                       :fork (:host github :repo "ahmed-shariff/org-noter"))
  :config
  (setq org-noter-property-doc-file "INTERLEAVE_PDF"
        org-noter-property-note-location "INTERLEAVE_PAGE_NOTE"))

(use-package org-brain ;;:quelpa (org-brain :fetcher github :repo "ahmed-shariff/org-brain" :branch "fix322/symlink_fix")
  :straight (org-brain :type git :host github :repo "Kungsgeten/org-brain"
                       :fork (:host github :repo "ahmed-shariff/org-brain"))
  :demand
  :init
  (setq org-brain-path "~/Documents/org/brain")
  :bind (("C-c v" . org-brain-visualize)
	 :map org-brain-visualize-mode-map
	 ("\C-coo" . org-brain-open-org-noter)
	 ("\C-cop" . org-brain-add-parent-topic)
         ("\C-cob" . org-brain-goto-button-at-pt)
         ("\C-cos". org-brain-print-parents))
  :config
  ;;(define-key org-brain-visualize-mode-map "")
  (defmacro org-brain-function-on-entry (fn)
  "Macro that generates a function which takes an entry and executes the fn while on a file entry."
  `(lambda (entry)
     (if (org-brain-filep entry)
         ""
       (org-with-point-at (org-brain-entry-marker entry)
         (or (funcall ,fn entry)
	     "")))))

  (setq org-id-track-globally t
        org-id-locations-file "~/.emacs.d/.org-id-locations"
        org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-brain-completion-system (lambda (&rest args) (s-join org-brain-entry-separator (apply #'completing-read-multiple args))))
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 50)
  ;; (setq org-brain-vis-title-prepend-functions (list
  ;;                                              (org-brain-function-on-entry 'org-brain-entry-todo-state-colored)
  ;;                                              (org-brain-function-on-entry 'org-brain-entry-priority)))
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
  (defun org-brain--targets-with-metadata (collection)
    "To use with completing read to allow having additional annotations with marginalia."
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata
            (category . org-brain-node))
        (complete-with-action action collection string predicate))))

  (defun org-brain-completing-read--metadata (args)
    (append (list (nth 0 args)
                  (org-brain--targets-with-metadata (nth 1 args)))
            (cddr args)))
  
  (advice-add #'org-brain-completing-read :filter-args 'org-brain-completing-read--metadata)
  ;; (advice-remove #'org-brain-completing-read 'org-brain-completing-read--metadata)
  
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

(defun org-brain-open-org-noter ()
  (interactive)
  (let ((entry (condition-case nil
		   (car (org-brain-button-at-point))
		 (user-error (org-brain-entry-at-pt)))))
    (if (org-brain-filep entry)
	(user-error "Noter cannot be opened for file entry")
      (org-with-point-at (org-brain-entry-marker entry)
	(if (string= "research_papers" (file-name-base (org-entry-get (point) "FILE")))
	    (org-noter)
	  (user-error "Noter only for the entries in research_paper"))))))

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

(defun org-brain-insert-visualize-button-with-tags (entry &optional face edge)
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

(defun org-brain-entry-todo-state-colored (entry)
  "Get todo state of ENTRY with colors."
  (let ((kwd (org-entry-get (point) "TODO")))
    (if kwd
	(propertize (substring kwd 0 1) 'face (org-get-todo-face kwd))
      nil)))

(defun org-brain-entry-priority (entry)
  "Get priority state of ENTRY with colors."
  (let ((priority (org-priority-to-value (org-priority-show))))
    (cond
     ((= priority 2000) "[#A]")
     (t nil))))


;; (advice-add 'org-brain-insert-visualize-button :override #'org-brain-insert-visualize-button-with-tags)

;; (advice-remove 'org-brain-insert-visualize-button #'org-brain-insert-visualize-button-with-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;experimnet end

(use-package org-ql
  :straight (org-ql :type git :host github :repo "alphapapa/org-ql" :fork t)
  :commands org-ql-defpred
  :bind (:map org-agenda-mode-map
         ("C-c o o" . org-ql-view-noter)))

(defun org-ql-view-noter ()
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (when (s-equals-p (buffer-name buffer) "research_papers.org")
      (with-current-buffer buffer
        (goto-char pos)
        (org-noter)))))

(org-ql-defpred brain-parent (&rest args)
  ""
  :body
  (let ((parents (org-entry-get-multivalued-property (point) "BRAIN_PARENTS"))
        (pred (car args))
        (parent-ids (cdr args)))
    (when parents
      (funcall
       (cond
        ((equal pred 'or) #'-any-p)
        ((equal pred 'and) #'-all-p)
        nil)
       (lambda (parent) (member parent parents)) parent-ids))))

(org-ql-defpred search-pdf-regexp (regexp)
  ""
  :body
  (let ((text-file (org-entry-get (point) "PDF_TEXT_FILE")))
    (when text-file
      (with-temp-buffer
        (insert-file-contents text-file)
        (s-match-strings-all regexp (buffer-string))))))


;; TODO: allow mulitiple combinations of brain-parent to be used (eg: (and (or ..) (or ..)))
(defun org-brain-query-papers-by-topic ()
  "CONNECTOR."
  (interactive)
  (let* ((selection-list (org-brain--all-targets))
         (topics 
          (completing-read-multiple "Query topic: " selection-list
                                    ;; (lambda (entry)
			            ;;   (or (s-starts-with-p "research topics::" entry)
                                    ;;       (s-starts-with-p "misc_topics::" entry)
			            ;;       (s-matches-p "work/projects::.*literature" entry)
			            ;;       (s-starts-with-p "publication::" entry)))
                                    nil
                                    t))
         (connector (if (> (length topics) 1)
                        (pcase (completing-read "connector: " '(and or) nil t)
                          ("or" 'or)
                          ("and" 'and))
                      'and))
         (topic-ids (list (append `(brain-parent (quote ,connector)) (mapcar (lambda (topic) (cdr (assoc topic selection-list))) topics))))
         (query (append '(and (level <= 1)) topic-ids)))
    (org-ql-search '("~/Documents/org/brain/research_papers.org")  query)))

(defun org-brain-query-papers-by-pdf-string (regexp)
  "REGEXP."
  (interactive "sRegexp: ")
  (let* ((query `(and (level <= 1) (search-pdf-regexp ,regexp))))
      (org-ql-search '("~/Documents/org/brain/research_papers.org")  query)))

(defun amsha/get-sprints (states)
  "Return sprints based on STATUS."
  (org-ql-select (expand-file-name "work/projects.org" org-brain-path)
    `(and (level 2) (todo ,@states) (h* "Sprint"))
    :action (lambda () (cons
                        (format "%-10s - %-40s: %s"
                                (let ((todo-state (org-entry-get (point) "TODO")))
                                  (propertize todo-state 'face (org-get-todo-face todo-state)))
                                (save-excursion
                                  (org-up-heading-safe)
                                  (s-replace-regexp
                                   "^<<[0-9]+>> " ""
                                   (org-no-properties (org-get-heading t t t t))))
                                (org-no-properties (org-get-heading t t t t)))
                        (org-id-get)))))

(defun org-brain-query-boards ()
  "List the in progress items in the project boards directory.
Either show all or filter based on a sprint."
  (interactive)
  (let* ((files (f-glob "*/project_boards/*.org" org-brain-path))
         (selection-list (append '(("ALL"))
                                 (amsha/get-sprints '("INPROGRESS" "TODO"))
                                 (--map (list (format "%s/project_boards::%s"
                                                      (f-base (f-parent (f-parent it)))
                                                      (file-name-base it)) it) files)))
         (predicate '(and (todo "INPROGRESS" "TODO")))
         (selection (assoc (completing-read "Project topic: " selection-list nil t) selection-list)))
    (cond
     ((s-contains-p "/project_boards::" (car selection))
      (setq files (cdr selection)))
     ((not (string= (car selection) "ALL"))
      (setq predicate
            (append predicate
                    `((member ,(cdr selection)
                              (org-entry-get-multivalued-property (point) "BRAIN_PARENTS")))))))
    (org-ql-search files predicate
      :super-groups (mapcar (lambda (x) (list :file-path (car (s-match "[^/]*/[^/]*/[^/]*\\.org" x)))) files)
      :title (car selection))))

(defun amsha/org-brain-children-topics (entry)
  "list parents of all the children of an ENTRY."
  (interactive (list (org-brain-choose-entry "Entry: " 'all nil t (org-brain-title (org-brain-entry-at-pt)))))
  (let (topics other-parents)
    (mapcar (lambda (child-entry)
              (-let (((-topics . -other-parents) (org-brain-parents-by-topics child-entry)))
                (setq topics (append topics -topics)
                      other-parents (append other-parents -other-parents))))
            (org-brain-children entry))
    (setq topics (-uniq topics)
          other-parents (-uniq other-parents))
    (org-brain-print-parents topics other-parents)
    (cons topics other-parents)))

(defun org-ql-query-topics ()
  "List all parent topics of all results from QUERY.
Currently written to work in org-ql butter."
  (interactive)
  (when (and org-ql-view-query org-ql-view-buffers-files)
    (let* (topics other-parents)
      (org-ql-select org-ql-view-buffers-files org-ql-view-query
        :action (lambda () (-let (((-topics . -other-parents) (org-brain-parents-by-topics (org-brain-entry-at-pt))))
                             (setq topics (append topics -topics)
                                   other-parents (append other-parents -other-parents)))))
      (setq topics (-uniq topics)
            other-parents (-uniq other-parents))
      (org-brain-print-parents topics other-parents))))

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(use-package org-download
  :straight (org-download :type git :host github :repo "abo-abo/org-download"
                          :fork (:host github :repo "ahmed-shariff/org-download"))
  :custom
  (org-download-screenshot-method (if (equalp system-type 'windows-nt) "magick convert clipboard: %s" "scrot")))
  
(defun pdf-crop-image (event &optional switch-back)
  "EVENT SWITCH-BACK."
  (interactive "@e")
  (setq current-b (buffer-name))
  (progn (pdf-view-mouse-set-region-rectangle event)
	 (pdf-view-extract-region-image pdf-view-active-region
					(pdf-view-current-page)
					(pdf-view-image-size)
					(get-buffer-create "teste")
					nil)))

(defun pdf-crop-image-and-save (event &optional switch-back)
  "EVENT SWITCH-BACK."
  (interactive "@e")
  (setq current-b (buffer-name))
  (progn (pdf-view-mouse-set-region-rectangle event)
	 (pdf-view-extract-region-image pdf-view-active-region
					(pdf-view-current-page)
					(pdf-view-image-size)
					(get-buffer-create "teste.jpg")
					nil)
         (set-buffer "teste.jpg")
	 (switch-to-buffer "taste.jpg")
         (with-current-buffer "taste.jpg"
           (mark-whole-buffer)
           (kill-ring-save (point-min) (point-max))
           (write-file "~/" t)
           (yank))))
	 ;; (kill-buffer "screenshot.png")
	 ;; (set-buffer current-b)
	 ;; (org-noter-insert-note)
	 ;; (org-download-screenshot)
	 ;; (if switch-back
;;     (switch-to-buffer-other-frame current-b))))

(defun amsha/format-multiline-from-kill-ring ()
  "Format a mulitline kill in the kill-ring into a single line."
  (format "\"%s\"" (replace-regexp-in-string "\n" " " (replace-regexp-in-string "- " "" (car kill-ring)))))

(defun amsha/paste-formatted-multiline-from-kill-ring ()
  "Paste a multiline segment (generally copied from pdf) as a single line."
  (interactive)
  (insert (amsha/format-multiline-from-kill-ring)))
  

(defun amsha/org-noter-copy-text-as-note ()
  "While in org noter, copy a highlighted text in as a heading with some minor additional formatting."
  (interactive)
  (let ((precise-info (org-noter--get-precise-info)))
    (pdf-view-kill-ring-save)
    (let ((org-noter-default-heading-title (amsha/format-multiline-from-kill-ring)))
      (let ((org-noter-insert-note-no-questions t))
	(org-noter-insert-note precise-info))
      ;; sometime the replace doesn't seem to work, so redoing it?
      (org-map-entries (lambda ()
			 (let (beg end)
			   (org-back-to-heading)
			   (setq beg (point))
			   (outline-end-of-heading)
			   (setq end (point))
			   (save-excursion
			     (kill-region beg end)
			     (insert (replace-regexp-in-string "- " "" (car kill-ring))))))
		       "LEVEL=2")
      (message "%s" (current-buffer)))))

(when (gethash 'use-pdf-tools configurations t)
  (define-key pdf-view-mode-map (kbd "C-c i") 'amsha/org-noter-copy-text-as-note)
  (define-key pdf-view-mode-map [C-M-down-mouse-1] 'pdf-crop-image)
  (define-key pdf-view-mode-map [C-M-S-down-mouse-1] 'pdf-crop-image-and-save))

(setq org-export-allow-bind-keywords t
      org-latex-image-default-option "scale=0.6")

(defun set-property-for-level-in-region (level property value)
  "."
  (interactive "nLevel: \nsPropertyb: \nsValue: ")
  (org-map-entries
   (lambda ()
     (org-entry-put (point) (upcase property) value))
     ;;(message "%s" (org-entry-properties)))
   (format "LEVEL=%s" level)
   'region))
     
(defun amsha/rename-full-path (file-name)
  "FILE-NAME."
  (replace-regexp-in-string "^\\([a-z]:\\)?\\(/.*\\)/Documents" "~/Documents" file-name))

(defun research-papers-configure ()
  "."
  (interactive)
  (with-current-buffer (find-file-noselect bibtex-completion-notes-path)
    (org-map-entries (lambda ()
		       (let* ((link-string (org-entry-get (point) "LINK"))
                              (link (if (string-empty-p link-string)
                                        nil
                                      link-string))
			      (cite-key (org-entry-get (point) "Custom_ID"))
			      (dir bibtex-completion-library-path)
			      (tags (org-get-tags))
			      (out-file-name (s-concat cite-key ".pdf"))
			      (full-path (amsha/rename-full-path (expand-file-name out-file-name dir))))
		         (org-entry-put (point) "ATTACH_DIR" dir)
		         (org-id-get-create)
		         (when (not (member "ATTACH" tags))
                           (if (or (file-exists-p full-path)
                                   (and link
                                        cite-key
                                        (condition-case nil
                                            (amsha/downlad-raname-move-file link out-file-name dir)
                                          (file-already-exists t))))
                               (progn
                                 (org-entry-put (point) "Attachment" out-file-name)
                                 (setq tags (append tags '("ATTACH")))
                                 (org-entry-put (point) "INTERLEAVE_PDF" full-path))
                             (progn
                               (setq tags (if link (delete "NO_LINK" tags) (append tags '("NO_LINK"))))
                               (setq tags (if cite-key (delete "NO_CITE_KEY" tags) (append tags '("NO_CITE_KEY")))))))
                         (when (file-exists-p full-path)
                           (let ((text-file-name (expand-file-name (format "%s.txt" (file-name-base full-path)) (file-name-directory full-path))))
                             (unless (file-exists-p text-file-name)
                               (condition-case nil
                                   (progn
                                     (with-temp-buffer
                                       (insert (amsha/pdf-to-text full-path))
                                       (write-file text-file-name))
                                     (org-entry-put (point) "PDF_TEXT_FILE" (amsha/rename-full-path text-file-name)))
                                 (error (message "Error: failed to read pdf file: %s" full-path)
                                        (setq tags (append tags '("PDF_ERROR"))))))))
		         (setq tags
			       (if (org-entry-get (point) "BRAIN_PARENTS")
			           (delete "NO_PARENTS" tags)
			         (append tags '("NO_PARENTS"))))
		         (org-set-tags (delete "nosiblings" (delete-dups tags)))
                         (when (and cite-key (not (org-entry-get nil "ROAM_REFS")))
                           (org-entry-put nil "ROAM_REFS" (format "cite:&%s" cite-key)))))
		     "LEVEL=1")))
;; (org-brain-update-id-locations)
;; (org-roam-db-sync))

(defun amsha/doi-utils-get-pdf-url-uml (old-function &rest rest)
  "Making sure the urls that are being recived by org-ref is made to use uml links."
  (let ((url (apply old-function rest)))
    (when url
      (amsha/get-uml-link url))))

(advice-add #'doi-utils-get-pdf-url :around #'amsha/doi-utils-get-pdf-url-uml)

(defun copy-related-research-papers (parent-id)
  "PARENT-ID."
  (interactive (list
		(save-excursion
		  (org-brain-goto (org-brain-choose-entry "Select research topic: " 'all (lambda (entry)
                                                                                           (or
                                                                                            (s-matches-p "work/projects::.*literature" (car entry))
                                                                                            (s-starts-with-p "research topics::" (car entry))))))
		  (org-entry-get (point) "ID"))))
  (let ((out-dir (expand-file-name parent-id "~/Downloads")))
    (condition-case nil
	(make-directory out-dir)
      (file-already-exists
       (progn
	 (message "Deleting directory and creating anew: %s" out-dir) 
	 (delete-directory out-dir t)
	 (make-directory out-dir))))
    (message "Copying files to %s" out-dir)
    (save-excursion
      (org-brain-goto "research_papers")
      (org-map-entries (lambda ()
		         (let ((file-path (org-entry-get (point) "INTERLEAVE_PDF")))
			   (when (and file-path
				      (member parent-id
					      (org-entry-get-multivalued-property (point) "BRAIN_PARENTS")))
			     (message "Copied %s" (file-name-nondirectory file-path)) 
			     (copy-file file-path
				        (expand-file-name (file-name-nondirectory file-path)
							  out-dir)))))))
    (dired out-dir)))


(defun copy-related-research-papers-org-ql (query)
  "QUERY."
  (interactive "xQuery: ")
  (let ((out-dir (expand-file-name (secure-hash 'md5 (format "%s" (current-time))) "~/Downloads")))
    (condition-case nil
	(make-directory out-dir)
      (file-already-exists
       (progn
	 (message "Deleting directory and creating anew: %s" out-dir) 
	 (delete-directory out-dir t)
	 (make-directory out-dir))))
    (message "Copying files to %s" out-dir)
    (org-ql-select (buffer-file-name (marker-buffer (org-brain-entry-marker "research_papers")))
      query
      :action (lambda ()
		(when-let ((file-path (org-entry-get (point) "INTERLEAVE_PDF")))
		  (message "Copied %s" (file-name-nondirectory file-path)) 
		  (copy-file file-path
			     (expand-file-name (file-name-nondirectory file-path)
					       out-dir)))))
    (dired out-dir)))


(defvar copy-notes-and-bib-function-org-buffer nil)
(defvar copy-notes-and-bib-function-bib-buffer nil)

(defun copy-notes-and-bib-function ()
  "NOTES-BUFFER BIB-BUFFER PREDICATE."
  (let* ((temp-name (secure-hash 'md5 (format "%s" (current-time))))
         (org-file-buffer (generate-new-buffer (format "*org-paper-notes-%s*" temp-name)))
         (bib-file-buffer (generate-new-buffer (format "*paper-notes-bib-%s*" temp-name))))
    (with-current-buffer org-file-buffer
      (org-mode)
      (insert "#+OPTIONS: H:0\n\n"))
    (with-current-buffer bib-file-buffer
      (bibtex-mode))
    (setq copy-notes-and-bib-function-org-buffer org-file-buffer)
    (setq copy-notes-and-bib-function-bib-buffer bib-file-buffer)
    (lambda ()
      (let* ((entry-key (org-entry-get (point) "Custom_ID")))
        (message "copying %s >> " entry-key)
        (save-excursion
          (find-file (car bibtex-completion-bibliography))
          (bibtex-search-entry entry-key)
          (bibtex-copy-entry-as-kill)
          (with-current-buffer bib-file-buffer
            (bibtex-yank))))
      (org-copy-subtree)
      (with-current-buffer org-file-buffer
        (when (not (looking-at "^")) (insert "\n"))
	(org-paste-subtree 1)))))

(defun copy-notes-and-bib-function-switch-to-buffers ()
  (interactive)
  (when copy-notes-and-bib-function-org-buffer
    (switch-to-buffer copy-notes-and-bib-function-org-buffer))
  (when copy-notes-and-bib-function-bib-buffer
    (switch-to-buffer copy-notes-and-bib-function-bib-buffer)))

(defun org-ql-copy-query-notes-and-bib (query)
  "copy notes and bib file to a seperate buffer based on org-ql query QUERY."
  (interactive "xQuery: ")
  (org-ql-select (buffer-file-name (marker-buffer (org-brain-entry-marker "research_papers")))
    query
    :action (copy-notes-and-bib-function))
  (copy-notes-and-bib-function-switch-to-buffers))

(defvar org-agenda-bulk-action-started nil)
(defvar org-agenda-bulk-action-post-execution-function nil)

(defun org-agenda-bulk-action-wrapper (original &rest args)
  (setq org-agenda-bulk-action-started t)
  (condition-case nil
      (apply original args)
    (error nil))
  (setq org-agenda-bulk-action-started nil)
  (when org-agenda-bulk-action-post-execution-function
    (funcall org-agenda-bulk-action-post-execution-function))
  (setq org-agenda-bulk-action-post-execution-function nil))

(advice-add 'org-agenda-bulk-action :around #'org-agenda-bulk-action-wrapper)

(defvar org-agenda-copy-query-notes-and-bib-func nil)

(defun org-agenda-copy-query-notes-and-bib ()
  "To be used with the org-agenda-bulk-action."
  (unless org-agenda-copy-query-notes-and-bib-func
    (setq org-agenda-copy-query-notes-and-bib-func (copy-notes-and-bib-function))
    (setq org-agenda-bulk-action-post-execution-function (lambda ()
                                                           (setq org-agenda-copy-query-notes-and-bib-func nil)
                                                           (copy-notes-and-bib-function-switch-to-buffers))))
  (org-with-point-at (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error))
    (funcall org-agenda-copy-query-notes-and-bib-func)))

(require 'org-ref-arxiv)
(defun arxiv-add-bibtex-entry-with-note (arxiv-link bibfile)
  "Add bibtex entry for ARXIV-LINK to BIBFILE."
  (interactive
   (list (read-string "arxiv: ")
         ;;  now get the bibfile to add it to
         (completing-read
          "Bibfile: "
          bibtex-completion-bibliography)))
  (save-window-excursion
    (find-file bibfile)
    (let* ((arxiv-number (s-chop-suffix ".pdf" (car (last (s-split "/" arxiv-link)))))
	   (search-point (word-search-forward arxiv-number nil t)))
      (if search-point
	  (progn
	    (goto-char search-point)
	    (message "%s already exists in the database" arxiv-number))
	(progn
	  (goto-char (point-max))
	  (when (not (looking-at "^")) (insert "\n"))
	  (insert (arxiv-get-bibtex-entry-via-arxiv-api arxiv-number))
	  (org-ref-clean-bibtex-entry)
	  (save-excursion
	    (when (f-file? bibtex-completion-notes-path)
	      (find-file-noselect bibtex-completion-notes-path)
	      (save-buffer))
	    (let ((bibtex-completion-bibliography (list (buffer-file-name)))
		  (keys (progn		  
			  (bibtex-beginning-of-entry)
			  (list (cdr (assoc "=key=" (bibtex-parse-entry)))))))
	      (bibtex-completion-edit-notes keys))
	    (goto-char (point-max))
	    (when (not (looking-at "^")) (insert "\n"))
	    (save-buffer))
	  (save-excursion
	    (find-file-other-window bibtex-completion-notes-path)
	    (goto-char (point-max))
	    (org-set-property "LINK" arxiv-link)
	    (research-papers-configure)))))))

(defun org-brain-add-parent-topic ()
  "."
  (interactive)
  (let ((embark-quit-after-action nil)
        (entry (org-brain-entry-at-pt)))
    (org-brain-add-parent entry (org-brain-choose-entries "Add parent topic: " 'all)) ;;(lambda (entry)
    ;; (or (s-starts-with-p "People::" (car entry))
    ;;     (s-starts-with-p "research topics::" (car entry))
    ;;     (s-starts-with-p "misc_topics::" (car entry))
    ;;     (s-matches-p "work/projects::.*literature" (car entry))
    ;;     (s-starts-with-p "publication::" (car entry))))))
    (org-brain-print-parents entry)))

(defun org-brain-goto-button-at-pt ()
  "."
  (interactive)
  (org-brain-goto (car (org-brain-button-at-point))))

(defun org-brain-parents-by-topics (&optional entry)
  "."
  (interactive)
  (when (null entry)
    (setq entry (condition-case nil
		    (car (org-brain-button-at-point))
		  (user-error (org-brain-entry-at-pt)))))
  (let ((parents (org-brain-parents entry))
        (topics '())
        (other-parents '()))
    (mapcar (lambda (entry)
              (if (and (listp entry) (s-equals-p (car entry) "research topics"))
                  (push (org-brain-vis-title entry) topics)            
                (push (org-brain-vis-title entry) other-parents)))
            parents)
    (cons topics other-parents)))

(defun org-brain-print-parents (&optional topics other-parents)
  "."
  (interactive)
  (when (and (null topics)
             (null other-parents))
    (-setq (topics . other-parents) (org-brain-parents-by-topics)))
  (message "%s \n********************************\n\t\t%s" (mapconcat 'identity (-list topics) "\n") (mapconcat 'identity (-list other-parents) "\n\t\t")))

(defun org-brain-delete-interleve-entry ()
  "Deletes the pdf entry of an org brain bib at point at point."
  (interactive)
  (when (y-or-n-p "Sure you want to delete the pdf file and the interleve entry here? ")
    (save-excursion
      (goto-char (org-entry-beginning-position))
      (delete-file (org-entry-get (point) "INTERLEAVE_PDF") nil)
      (when-let* ((pdf-text-file (org-entry-get (point) "PDF_TEXT_FILE"))
                  (pdf-text-file-exists (file-exists-p pdf-text-file)))
        (delete-file pdf-text-file nil))
      (org-entry-delete (point) "Attachment")
      (org-entry-delete (point) "INTERLEAVE_PDF")
      (org-entry-delete (point) "PDF_TEXT_FILE")
      (org-set-tags (delete "PDF_ERROR" (delete "nosiblings" (delete "ATTACH" (org-get-tags))))))))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map "\C-c!" 'org-time-stamp-inactive)
	    (define-key org-mode-map "\C-coo" 'org-noter)
	    (define-key org-mode-map "\C-cop" 'org-brain-add-parent-topic)
	    (define-key org-mode-map "\C-coc" 'research-papers-configure)
            (define-key org-mode-map "\C-cos" 'org-brain-print-parents)
            (define-key org-mode-map (kbd "C-'") nil)
	    (flyspell-mode t)))
(add-hook 'org-mode-hook 'visual-line-mode)

(defun amsha/pdf-to-text (file-name)
  "FILE-NAME."
  (let* ((normalized-file-name (expand-file-name file-name)))
    (format
     "%s"
     (replace-regexp-in-string
      "\n" " "
      (replace-regexp-in-string "- " ""
                                (mapconcat (lambda (page-number)
                                             (pdf-info-gettext (number-to-string page-number) '(0 0 1 1) nil normalized-file-name))
                                           (number-sequence 1 (pdf-info-number-of-pages normalized-file-name)) " "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;experimnet starts
;; (defvar org-brain-insert-visualize-button-predicate nil)

;; (defun org-brain-insert-visualize-button-predicate-drop-research-papers (entry)
;;   (not (member (file-name-sans-extension (file-truename org-ref-bibliography-notes)) (org-brain-parents entry))))

;; (defun org-brain-insert-visualize-button-with-predicate (entry &optional face category)
;;   "Insert a button, running `org-brain-visualize' on ENTRY when clicked.
;; FACE is sent to `org-brain-display-face' and sets the face of the button.
;; CATEGORY is used to set the `brain-category` text property."
;;   (when (and org-brain-insert-visualize-button-predicate
;; 	     (funcall org-brain-insert-visualize-button-predicate entry))
;;     (let ((annotation (org-brain-get-edge-annotation org-brain--vis-entry
;; 						     entry
;; 						     org-brain--vis-entry-keywords)))
;;       (insert-text-button
;;        (org-brain-vis-title entry)
;;        'action (lambda (_x) (org-brain-visualize entry))
;;        'id (org-brain-entry-identifier entry)
;;        'follow-link t
;;        'brain-category (or category 'default)
;;        'help-echo annotation
;;        'aa2u-text t
;;        'face (org-brain-display-face entry face annotation)))))

;; (defun orb-brain-visualize-show-research-papers ()
;;   "."
;;   (interactive)
;;   (setq org-brain-insert-visualize-button-predicate
;; 	(if org-brain-insert-visualize-button-predicate
;; 	    nil
;; 	  (function org-brain-insert-visualize-button-predicate-drop-research-papers))))

;; (advice-add 'org-brain-insert-visualize-button :override #'org-brain-insert-visualize-button-with-predicate)

;; (advice-remove 'org-brain-insert-visualize-button #'org-brain-insert-visualize-button-with-predicate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;experimnet end

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
(setq org-clock-continuously t
      org-clock-idle-time 10)

(provide 'orgZ)
;;; orgZ.el ends here
