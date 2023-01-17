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
(require 'org-tempo)
;; (require 'org-capture-pop-frame)
;(ido-mode)

(defvar okm-base-directory (file-truename "~/Documents/org/brain") "org knowladge management base direcory.")
(defvar okm-research-papers-id "34854c23-cf0a-40ba-b0c6-c9e5b3bb3030" "The id of the research_papers file.") ;; research_papers id TODO: think of a better way to do this?
(defvar okm-parent-property-name "BRAIN_PARENTS" "Property name containing parent ids.")
(defvar okm-parent-id-type-name "brain-parent" "ID type name used to refer to parent.")

(defun sync-org ()
  "Sync the org directory"
  (interactive)
  (magit--with-safe-default-directory "~/Documents/org"
    (let ((has-diff (magit-git-string "diff" "--exit-code")))
      (message "sync-org: %s"
             (-if-let* ((_1 (progn (message "sync-org: Pulling")
                                   (when has-diff
                                       (magit-stash-save (format "sync-pulling %s" (git-message)) 'index 'worktree nil 'refresh nil 'noerror))
                                   (let ((res (magit-with-editor
                                                (magit-git-string-ng "pull"))))
                                     (when has-diff
                                       (magit-stash-pop "stash@{0}")) ;; FIXME: Better way to get this?
                                     (and res
					  ;; Checking for status copied from `magit-discard-hunk'
                                          (--any it (--map (not (equal '(?U ?U) (cddar (magit-file-status it)))) (magit-modified-files)))))))
                        (_2 (magit-with-toplevel
                              (if has-diff
                                  (progn 
                                    (message "sync-org: commiting")
                                    (magit-stage-1 "-u")
                                    ;; Anything in the following dir's not in gitignore should be added
                                    (magit-git-string-p "add" "brain/research_papers")
                                    (magit-git-string-p "add" "brain/roam-notes")
                                    (magit-git-string-p "add" "brain/work/figures")
                                    (magit-git-string-ng "commit" "-m" (git-message)))
                                (message "sync-org: Nothing to commit")
                                'no-diff)))
                        (_3 (if has-diff
                                (progn
                                  (message "sync-org: Pushing")
                                  (magit-run-git-with-editor "push"))
                              t)))
                 "success"
               "failed")))))

(use-package org-capture-pop-frame
  :straight (org-capture-pop-frame :type git :host github :repo "tumashu/org-capture-pop-frame"
                                   :fork (:host github :repo "ahmed-shariff/org-capture-pop-frame"))
  :config
  (setf (alist-get 'width ocpf-frame-parameters) 170)
  (setf (alist-get 'height ocpf-frame-parameters) 50))

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
        (cond
         ((and (string-match "\\(10\\.[0-9]\\{4\\}\\(/\\|%2F\\)\\([a-z]\\|[0-9]\\|_\\|-\\|\\.\\)+\\)" url)
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
                ;; make sure at the top most level
                (while (not (>= 1 (org-outline-level)))
                  (org-up-element))
                ;; add link if not already set
                (-if-let* ((link (org-entry-get (point) "LINK"))
                           (l (> (length link) 0)))
                    nil  ;; do nothing
                  (org-set-property "LINK" file-name))
                (research-papers-configure t))))
         ;; handle arxiv links
         ((string-match "arxiv\\.org.*pdf$" url)
          (arxiv-add-bibtex-entry-with-note url (car bibtex-completion-bibliography)))
         (t (progn
              (push file-name kill-ring)
              (message "No valid DOI: Adding %s to kill ring" file-name))))))
    ;; (find-file bibtex-completion-notes-path)
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
                                         (R . t)
				         (shell . t))))

(when (gethash 'use-jupyter configurations t)
  (use-package jupyter
    :custom
    (org-babel-jupyter-resource-directory "jupyter-output")
    :config
    (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
						    (:session . "py")
						    (:kernel . "python3")
						    (:tangle . "jupyter-python/tangled.py")
						    (:exports . "both")))
    (push '(jupyter . t) org-babel-load-languages))
      
  (use-package ox-ipynb
    :straight (ox-ipynb :type git :host github :repo "jkitchin/ox-ipynb")))

;; from https://emacs.stackexchange.com/questions/44664/apply-ansi-color-escape-sequences-for-org-babel-results
(defun org-babel-ansi-color-result ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'org-babel-ansi-color-result)


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

(use-package orglink
  :config
  (add-to-list 'orglink-activate-in-modes 'prog-mode)
  (global-orglink-mode))

(defun org-id-get-closest ()
  "move up the tree until an el with id is found"
  (ignore-error user-error (cl-do () ((org-id-get) (org-id-get)) (org-up-element))))


;;**********************bulk action wrappers***********************
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

;;  ***************************************************************

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

(defun okm--ask-id (file prompt property)
  "."
  (save-window-excursion
    (find-file file)
    (org-ask-title-location prompt)
    (org-entry-get (point) property)))

(defun okm--org-templates-get-project-id ()
  "Get the new project id."
  (number-to-string
   (1+ (apply #'max
              (org-ql-select (expand-file-name "work/projects.org" okm-base-directory) `(level 1)
                :action (lambda ()
                          (condition-case nil
                              (string-to-number (cadr (s-match "<<\\([0-9]+\\)>>" (org-get-heading t t t t))))
                            (wrong-type-argument 0))))))))

(defun okm--org-templates-get-sprint-id ()
  "Get the new sprint id."
  (number-to-string
   (1+
    (apply #'max
           (org-ql-select
             (expand-file-name "work/projects.org" okm-base-directory)
             `(parent ,(s-replace-regexp "/$" "" (car org-refile-history)))
             :action  (lambda ()
                        (condition-case nil
                            (string-to-number (cadr (s-match "Sprint \\([0-9]+\\):"
                                                             (org-get-heading t t t t))))
                          (wrong-type-argument 0))))))))

(defun okm-ask-experiment-id ()
  "."
  (save-window-excursion
    (find-file "~/Documents/org/brain/work/experiments_log.org")
    (org-ask-title-location  "Experiment ")
    (org-entry-get (point) "CUSTOM_ID"))
  ;;(okm--ask-id "~/Documents/org/brain/work/experiments_log.org"  "Experiment " "CUSTOM_ID"))
)

(defun okm-ask-project-id ()
  "."
  (okm--ask-id "~/Documents/org/brain/work/projects.org"  "Project " "CUSTOM_ID"))

(defun okm-ask-task-board ()
  "Move the cursor to a location in a task board."
  (let* ((project-boards (mapcar (lambda (file) (cons (format "%-10s %s"
                                                              (propertize (f-base (f-parent (f-parent file)))  'face 'marginalia-documentation)
                                                              (file-name-base file))
                                                       file))
                                 (--keep
                                  (when (not (s-contains-p "#" it)) it)
                                  (f-glob "*/project_boards/*.org" okm-base-directory))))
         (board-file (cdr (assoc (completing-read "Select poject board: " project-boards) project-boards))))
    (find-file board-file)
    (goto-char (point-max))))

(defun okm-board-task-location ()
  "Return a org title with board task after prompting for it."
  (let* ((project-boards (--keep (when (not (s-contains-p "#" it)) it)
                                 (f-glob "*/project_boards/*.org" okm-base-directory)))
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
                   (assoc (completing-read "Select task: " targets nil t) targets))))
    (format "**** [[id:%s][%s]]  %%?"
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
	 "%(okm-board-task-location)")
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
	 "** TODO Sprint %(okm--org-templates-get-sprint-id): %^{TITLE}
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
	 "* TODO <<%(okm--org-templates-get-project-id)>> %^{TITLE}
  :PROPERTIES:
  :CUSTOM_ID: %(okm--org-templates-get-project-id)
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
	 entry (function okm-ask-task-board)
	 "* TODO %^{TITLE}
  :PROPERTIES:
  :ID:       %(org-id-new)
  :END:
%?
"
	 :jump-to-captured t)
	("b" "Org brain")
	("bp" "Add research paper"
	 entry (function (lambda () (org-id-goto okm-research-papers-id)));(file "~/Documents/org/brain/research_papers.org")
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

;;(require 'org-bullets)
(use-package org-modern
    :straight (:type git :host github :repo "minad/org-modern")
    :custom (org-modern-star '("◉" " ○" "  ◈" "   ◇" "    •" "     ◦" "      ▸" "       ▹"))
    :hook ((org-mode . org-modern-mode)
           (org-agenda-finalize . org-modern-agenda))
    :custom-face
    (org-modern-label ((t :height 0.9
                        (:box
                         (:line-width
                          (1 . -1)
                          :color "#777c80" :style nil))))))

(use-package org-transclusion
  :after org
  :defer 2
  :bind (:map org-mode-map
              ("C-c o t" . org-transclusion-hydra/body))
  :custom-face
  (org-transclusion-fringe ((t (:background "Brown" :weight ultra-bold :width extra-expanded))))
  (org-transclusion-source-fringe ((t (:background "Brown" :weight ultra-bold :width extra-expanded))))
  (org-transclusion ((t (:background "Brown" :weight ultra-bold :width extra-expanded))))
  :config
  (defhydra org-transclusion-hydra (:color blue)
    "Transclusion functions"
    ("m" org-transclusion-make-from-link "Make link")
    ("a" org-transclusion-add "Add")
    ("A" org-transclusion-add-all "Add all")
    ("t" org-transclusion-mode "org-transclusion-mode")))

(use-package bibtex-completion
  :custom
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  :config
  (setf (alist-get 'org-mode bibtex-completion-format-citation-functions) (lambda (keys) (s-join "," (--map (format "cite:&%s" it) keys)))))


;; On windows when the `cygwin1.dll mismatch issue` issue happens, This is solved by manually running the command seen in the *compilation* buffer
(use-package org-roam
  :after org
  :defer 2
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename okm-base-directory))
  (org-roam-dailies-directory "dailies")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "%(okm-board-task-location)"
      :target (file+datetree
               "~/Documents/org/brain/work/notes.org"
               "day"))
     ("p" "personal" entry "%(okm-board-task-location)"
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
                                   (f-glob "*.org" (file-truename okm-base-directory))
                                   nil nil))
       nil nil)
      :prepend t
      :kill-buffer t)
     ("n" "new note" plain "%?"
     :target (file+head "~/Documents/org/brain/roam-notes/${slug}-%<%Y%m%d%H%M%S>.org"
                        "#+title: ${title}\n")
     :unnarrowed t)))
  (org-roam-node-display-template "${title}")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ;;("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n y" . org-roam-db-sync)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ;; org-roam-bibtex
         ("C-c n b" . orb-insert-link))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)

;;   (defmacro org-roam-backlinks-get-brain-relation (relation-function node)
;;     "Use `relation-function' to get the relations as backlinks for the given org-roam `node'.
;; `relation-function' is any function that takes a org-brain entry and
;; return a list of org-brain entries."
;;     `(let* ((node-id (org-roam-node-id ,node))
;;            (backlinks
;;             ;; Getting brain-relation and convert them to roam backlinks.
;;             (mapcar (lambda (entry)
;;                       (--> (org-brain-entry-marker entry)
;;                            (with-current-buffer (marker-buffer it)
;;                              (goto-char (marker-position it))
;;                              (list (org-id-get)
;;                                    node-id
;;                                    (point)
;;                                    ;; This can error if link is not under any headline
;;                                    ;; copied from `org-roam-db-insert-link'
;;                                    (list
;;                                     :outline
;;                                     (ignore-errors
;;                                       (org-get-outline-path 'with-self 'use-cache)))))))
;;                     (,relation-function (save-excursion
;;                                           (org-id-goto node-id)
;;                                           (org-brain-entry-at-pt))))))
;;       (cl-loop for backlink in backlinks
;;                collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
;;                          (org-roam-populate
;;                           (org-roam-backlink-create
;;                            :source-node (org-roam-node-create :id source-id)
;;                            :target-node (org-roam-node-create :id dest-id)
;;                            :point pos
;;                            :properties properties))))))

  (defun org-roam-brain-children-section (node)
    "The brain children section for NODE.
Copied  from `org-roam-backlink-get'."
    (when-let ((backlinks (seq-sort #'org-roam-backlinks-sort (okm-backlinks-get node))))
      (magit-insert-section (org-roam-brain-children)
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
    (if (--> (org-roam-node-at-point)
             (org-roam-node-file it)
             (or (member it
                         (list
                          (file-truename "~/Documents/org/brain/work/notes.org") (file-truename "~/Documents/org/brain/personal/notes.org")))
                 (f-ancestor-of-p bibtex-completion-notes-path it)))
        (let ((beg (progn (org-roam-end-of-meta-data t)
                          (point)))
              (end (progn (org-previous-visible-heading 1)
                          (org-end-of-subtree)
                          (point))))
          (-reduce 
           (lambda (str el)
             ;; remove properties not interested. If prop drawer is empty at the end, remove drawer itself
             (s-replace-regexp (format "\n *:%s:.*$" el) "" str))
           ;; remove links
           (list (s-replace-regexp "\\[id:\\([a-z]\\|[0-9]\\)\\{8\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{12\\}\\]"
                                   ""
                                   (string-trim (buffer-substring-no-properties beg end)))
                 "INTERLEAVE_PAGE_NOTE" "BRAIN_CHILDREN" okm-parent-property-name "PROPERTIES:\n *:END")))
      (org-roam-preview-default-function)))

  (setq org-roam-preview-function #'org-roam-subtree-aware-preview-function)

  (defun org-roam-node-read-multiple (&optional prompt)
    "Like org-roam-node-read, but with mulitiple read excluding the template used by roam."
    (let ((nodes (mapcar (lambda (node)
                           (cons (org-roam-node-title node) node))
                         (org-roam-node-list))))
      (-non-nil
       (--map (org-roam-node-from-title-or-alias it)
              (completing-read-multiple
               (or prompt "Node(s):")
               (lambda (string pred action)
                 (if (eq action 'metadata)
                     '(metadata
                       ;; (annotation-function . consult-notes-org-roam-annotate)
                       (category . org-roam-node))
                   (complete-with-action
                    action
                    nodes
                    string
                    pred))))))))

  (defun okm-render-org-roam-buffer (sections title buffer-name)
    "Render SECTIONS (list of functions) in an org-roam buffer."
    ;; copied  from `org-roam-buffer-render-contents'
    (with-current-buffer (get-buffer-create buffer-name)
        (let ((inhibit-read-only t)
              (org-roam-buffer-current-directory org-roam-directory))
          (setq-local default-directory org-roam-buffer-current-directory)
          (setq-local org-roam-directory org-roam-buffer-current-directory)
          (erase-buffer)
          (org-roam-mode)
          (org-roam-buffer-set-header-line-format title)
          (insert ?\n)
          (dolist (section sections)
            (funcall section))
          (goto-char 0))
        (display-buffer (current-buffer))))

  (defmacro okm-magit-section-for-nodes (nodes)
    "Returns a function that can be passed as a section for `okm-render-org-roam-buffer' with the NODES.
Each node is a 3 elements list: (source-node-id point properties)."
    `(lambda ()
       (magit-insert-section (org-roam)
         (magit-insert-heading)
         (dolist (entry
                  ;;(seq-uniq  ;; removing duplicates as the whole subtree will be getting displayed
                  ,nodes)
           ;;(lambda (e1 e2) (equal (car e1) (car e2)))))
           (pcase-let ((`(,source ,pos ,properties) entry))
             (org-roam-node-insert-section :source-node (org-roam-node-from-id source) :point pos :properties properties))
           (insert ?\n))
         (run-hooks 'org-roam-buffer-postrender-functions))))
  
  (defun okm-org-roam-list-notes (entries)
    "Filter based on the list of ids (FILTER) in the notes files."
    (interactive (list ;;(org-roam-node-read nil nil nil 'require-match "Filter on Nodes:")))
                  (org-roam-node-read-multiple)))
    (let* ((entries (--map (if (stringp it) (org-roam-node-from-title-or-alias it) it) entries))
           (names (s-join "," (--map (org-roam-node-title it) entries)))
           (title (format "(%s)" names))
           (buffer-name (format "*notes: %s*" names))
           (ids (apply #'vector (--map (org-roam-node-id it) entries))))
      (okm-render-org-roam-buffer
       (list (okm-magit-section-for-nodes (org-roam-db-query
                                           [:select [links:source links:pos links:properties]
                                                    :from links :inner :join nodes :on (= links:source nodes:id)
                                                    :where (and (in links:dest $v1) (in nodes:file $v2))]
                                           ids
                                           (vector
                                            (file-truename "~/Documents/org/brain/personal/notes.org")
                                            (file-truename "~/Documents/org/brain/work/notes.org")))))
       title buffer-name)))

  (defun okm-org-roam-buffer-nodes (nodes)
    "convert nodes to list of nodes compatible for `okm-magit-section-for-nodes'."
    (--map (list (org-roam-node-id it) (org-roam-node-point it) (org-roam-node-properties it)) nodes))

  (defun okm-org-roam-buffer-for-nodes (nodes title buffer-name)
    "View nodes in org-roam buffer"
    (okm-render-org-roam-buffer
       (list
        (okm-magit-section-for-nodes (okm-org-roam-buffer-nodes nodes)))
       title buffer-name))

  (defun okm-roam-view-query (source-or-query)
    "View source or query in org-roam buffer."
    (interactive "xQuery: ")
    (okm-org-roam-buffer-for-nodes (org-roam-ql-view--get-nodes-from-query source-or-query) (format "Query view: %s" source-or-query) "*org-roam query view*"))

  (defun okm-roam-buffer-from-ql-buffer ()
    "Convert a org-ql reusult to a roam-buffer."
    (interactive)
    (unless org-ql-view-buffers-files
      (user-error "Not an Org QL View buffer"))
    ;; Copied from `org-agenda-finalize'
    (let (mrk nodes)
      (save-excursion
	(goto-char (point-min))
	(while (equal (forward-line) 0)
	  (when (setq mrk (get-text-property (point) 'org-hd-marker))
            (org-with-point-at mrk
              ;; pick only nodes
              (-if-let (id (org-id-get))
                  (push (org-roam-node-from-id id) nodes)
                (user-error "Non roam-node headings in query."))))))
      (okm-org-roam-buffer-for-nodes nodes
                                     org-ql-view-title
                                     (format "*From ql: %s*" org-ql-view-title))))

  (defun org-roam-node-annotator (cand)
    "Annotate org-roam-nodes in completions"
    (when-let* ((node (condition-case err
                          (org-roam-node-from-title-or-alias (org-no-properties cand))
                        (error nil)))
                (file (concat (s-replace ".org" "" (f-relative (org-roam-node-file node) okm-base-directory))
                              (when (> (org-roam-node-level node) 1) (concat "::" (string-join (org-roam-node-olp node) " > "))))))
      (marginalia--fields
       (file :face 'shadow :truncate 1.0)
       ((marginalia--time (org-roam-node-file-mtime node)) :face 'org-cite))))

  (add-to-list 'marginalia-annotator-registry
               '(org-roam-node org-roam-node-annotator marginalia-annotate-face builtin none)))

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :bind (("C-c n n" . consult-notes-search-in-all-notes)
         ("C-c n v" . consult-notes-visit-relation))
  :commands (consult-notes
             consult-notes-search-in-all-notes
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (setq consult-notes-sources `(("Org"  ?o  ,okm-base-directory)) ;; Set notes dir(s), see below
        consult-notes-org-roam-template org-roam-node-display-template ;; To make sure I can use my marginalia approach.
        consult-notes-org-roam-annotate-function nil)
  (consult-notes-org-roam-mode) ;; Set org-roam integration

  (defun consult-notes-visit-relation (node)
    "Navigate to related node of `node'."
    (interactive (list (or (when (eq major-mode 'org-mode) (org-roam-node-at-point)) (org-roam-node-read nil nil nil t "Select node: "))))
    (consult--multi (list
                     (plist-multi-put (copy-seq consult-notes-org-roam--nodes)
                                      :name (propertize "Backlinks" 'face 'consult-notes-sep)
                                      :narrow ?b
                                      :items (lambda () (--map (org-roam-node-title (org-roam-backlink-source-node it))
                                                               (org-roam-backlinks-get node :unique t))))
                     (plist-multi-put (copy-seq consult-notes-org-roam--nodes)
                                      :name (propertize "Brain Children" 'face 'consult-notes-sep)
                                      :narrow ?c
                                      :items (lambda () (--map (org-roam-node-title (org-roam-node-from-id it))
                                                               (okm-get-children (org-roam-node-id node)))))
                     (plist-multi-put (copy-seq consult-notes-org-roam--nodes)
                                       :name (propertize "Forwardlink" 'face 'consult-notes-sep)
                                       :narrow ?f
                                       :items (lambda () (-map #'car (org-roam-db-query
                                                                      [:select :distinct nodes:title
                                                                               :from links :inner :join nodes :on (= links:dest nodes:id)
                                                                               :where (in links:source $v1)]
                                                                      (vector (org-roam-node-id node))))))
                     (plist-multi-put (copy-seq consult-notes-org-roam--nodes)
                                      :name (propertize "Brain Parents" 'face 'consult-notes-sep)
                                      :narrow ?p
                                      :items (lambda () (--map (org-roam-node-title (org-roam-node-from-id it))
                                                               (okm-get-parents (org-roam-node-id node)))))
                     )
                    :require-match t
                    :prompt "Related nodes:")))


(use-package org-roam-bibtex
  :after (org-roam consult-bibtex)
  :defer 2
  :custom
  (orb-roam-ref-format "org-ref-v3")
  (orb-insert-interface "generic")
  :config
  (defun orb-inesert-consult-bibtex (&optional arg)
    "Overriding `ORB-INSERT-GENERIC' to use `consult-bibtex'."
    (orb-insert-edit-note (consult-bibtex--read-entry)))

  (advice-add 'orb-insert-generic :override #'orb-inesert-consult-bibtex))

(use-package consult-bibtex
  :straight '(consult-bibtex :host github
                             :repo "mohkale/consult-bibtex")
  ;; (use-package ivy-bibtex
  ;;   :after (org-ref)
  :defer 2
  :config
  ;; (require 'org-ref-ivy)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
        org-ref-insert-cite-function '(lambda () (consult-bibtex (consult-bibtex--read-entry)))
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
  (setq bibtex-completion-notes-path (file-truename "~/Documents/org/brain/research_papers/")
	bibtex-completion-bibliography '("~/Documents/org/bibliography/references.bib")
        bibtex-completion-library-path "~/Documents/org/bibliography/pdfs/"
        reftex-default-bibliography bibtex-completion-bibliography
	
	org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
	bibtex-completion-notes-template-one-file
	(format
	 "* (${year}) ${title} [${author}]\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :Keywords: ${keywords}\n  :LINK: ${pdf}\n  :YEAR: ${year}\n  :RPC-TAGS: NO_LINK NO_PARENTS NO_CITE_KEY\n  :BRAIN_PARENTS: %s:%s\n  :END:\n\n  - cite:${=key=}"
         okm-parent-id-type-name okm-research-papers-id) 
        bibtex-completion-notes-template-multiple-files bibtex-completion-notes-template-one-file
        ;;":PROPERTIES:\n:Custom_ID: ${=key=}\n:Keywords: ${keywords}\n:LINK: ${pdf}\n:YEAR: ${year}\n:RPC-TAGS: :NO_LINK NO_PARENTS NO_CITE_KEY\n:END:\n\n#+TITLE: (${year}) ${title} [${author}]\n\n"
	doi-utils-open-pdf-after-download nil
        doi-utils-download-pdf nil)

  (defun org-ref-get-bibtex-key-under-cursor-with-latex (old-func)
    (if (derived-mode-p 'latex-mode)
        (bibtex-completion-get-key-latex)
      (funcall old-func)))

  (advice-add 'org-ref-get-bibtex-key-under-cursor :around #'org-ref-get-bibtex-key-under-cursor-with-latex)
  ;; (advice-remove 'org-ref-get-bibtex-key-under-cursor #'org-ref-get-bibtex-key-under-cursor-with-latex)

  (require 'org-ref-latex)

  (defun org-ref-latex-get-bibliography-or-default (return-val)
    "Use `bibtex-completion-bibliography' if `org-ref-latex-get-bibliography' returns nil."
    (or return-val bibtex-completion-bibliography))

  (advice-add 'org-ref-latex-get-bibliography :filter-return #'org-ref-latex-get-bibliography-or-default)

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
      (switch-to-buffer buffer)))

  (defhydra+ org-ref-citation-hydra ()
    ("t" (lambda ()
           (interactive)
           (save-excursion
             (org-ref-open-notes-at-point)
             (org-noter)))
     "open noter" :column "Open")))

(defun doi-add-bibtex-entry-with-note ()
  "."
  (interactive)
  (call-interactively #'doi-utils-add-bibtex-entry-from-doi)
  (find-file (car bibtex-completion-bibliography))
  (org-ref-open-bibtex-notes)
  (org-set-property "LINK" (completing-read "LINK: " nil nil nil (when (s-starts-with-p "file://" (car kill-ring))
                                                                   (car kill-ring))))
  (research-papers-configure))

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
  (setq org-brain-path (file-truename "~/Documents/org/brain"))
  :bind (("C-c v" . org-brain-visualize)
	 :map org-brain-visualize-mode-map
	 ("\C-coo" . org-brain-open-org-noter)
	 ("\C-cop" . okm-add-parent-topic)
         ("\C-cob" . org-brain-goto-button-at-pt)
         ("\C-cos". okm-print-parents))
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
  ;; (defun org-brain-insert-resource-icon (link)
  ;;   "Insert an icon, based on content of org-mode LINK."
  ;;   (insert (format "%s "
  ;;                   (cond ((string-prefix-p "http" link)
  ;;                          (cond ((string-match "wikipedia\\.org" link)
  ;;                                 (all-the-icons-faicon "wikipedia-w"))
  ;;       			 ((string-match "github\\.com" link)
  ;;                                 (all-the-icons-octicon "mark-github"))
  ;;       			 ((string-match "vimeo\\.com" link)
  ;;                                 (all-the-icons-faicon "vimeo"))
  ;;       			 ((string-match "youtube\\.com" link)
  ;;                                 (all-the-icons-faicon "youtube"))
  ;;       			 (t
  ;;                                 (all-the-icons-faicon "globe"))))
  ;;                         ((string-prefix-p "brain:" link)
  ;;                          (all-the-icons-fileicon "brain"))
  ;;                         (t
  ;;                          (all-the-icons-icon-for-file link))))))
  ;; (defun org-brain--targets-with-metadata (collection)
  ;;   "To use with completing read to allow having additional annotations with marginalia."
  ;;   (lambda (string predicate action)
  ;;     (if (eq action 'metadata)
  ;;         `(metadata
  ;;           (category . org-brain-node))
  ;;       (complete-with-action action collection string predicate))))

  ;; (defun org-brain-completing-read--metadata (args)
  ;;   (append (list (nth 0 args)
  ;;                 (org-brain--targets-with-metadata (nth 1 args)))
  ;;           (cddr args)))
  
  ;; (advice-add #'org-brain-completing-read :filter-args 'org-brain-completing-read--metadata)
  ;; (advice-remove #'org-brain-completing-read 'org-brain-completing-read--metadata)
  
  ;; (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)
  ;; (defface aa2u-face '((t . nil))
  ;;   "Face for aa2u box drawing characters")
  ;; (advice-add #'aa2u-1c :filter-return
  ;;             (lambda (str) (propertize str 'face 'aa2u-face)))
  ;; (defun aa2u-org-brain-buffer ()
  ;;   (let ((inhibit-read-only t))
  ;;     (make-local-variable 'face-remapping-alist)
  ;;     (add-to-list 'face-remapping-alist
  ;;                  '(aa2u-face . org-brain-wires))
  ;;     (ignore-errors (aa2u (point-min) (point-max)))))  
  ;; (with-eval-after-load 'org-brain
  ;;   (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))
  )

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

;; (defun org-brain-insert-visualize-button-with-tags (entry &optional face edge)
;;   "Insert a button, running `org-brain-visualize' on ENTRY when clicked.
;; FACE is sent to `org-brain-display-face' and sets the face of the button.
;; Appends the todo state of the entry being visualized."
;;   (let ((annotation (org-brain-get-edge-annotation org-brain--vis-entry
;;                                                    entry
;;                                                    org-brain--vis-entry-keywords)))
;;     (let ((kwd-setting
;; 	   (unless (org-brain-filep entry)
;; 	     (org-with-point-at
;; 		 (org-brain-entry-marker entry)
;; 	       (let ((kwd (org-entry-get (point) "TODO")))
;; 		 (if kwd
;; 		     (list kwd (org-get-todo-face kwd))
;; 		   nil))))))
;;       (when kwd-setting
;; 	(insert (propertize (substring (first kwd-setting) 0 1) 'face (second kwd-setting)) " ")))
;;     (insert-text-button
;;      (org-brain-title entry)
;;      'action (lambda (_x) (org-brain-visualize entry))
;;      'id (org-brain-entry-identifier entry)
;;      'follow-link t
;;      'help-echo annotation
;;      'aa2u-text t
;;      'face (org-brain-display-face entry face annotation))))

;; (defun org-brain-entry-todo-state-colored (entry)
;;   "Get todo state of ENTRY with colors."
;;   (let ((kwd (org-entry-get (point) "TODO")))
;;     (if kwd
;; 	(propertize (substring kwd 0 1) 'face (org-get-todo-face kwd))
;;       nil)))

;; (defun org-brain-entry-priority (entry)
;;   "Get priority state of ENTRY with colors."
;;   (let ((priority (org-priority-to-value (org-priority-show))))
;;     (cond
;;      ((= priority 2000) "[#A]")
;;      (t nil))))


;; (advice-add 'org-brain-insert-visualize-button :override #'org-brain-insert-visualize-button-with-tags)

;; (advice-remove 'org-brain-insert-visualize-button #'org-brain-insert-visualize-button-with-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;experimnet end

(use-package org-ql
  :straight (org-ql :type git :host github :repo "alphapapa/org-ql" :fork t)
  :bind (:map org-agenda-mode-map
              ("C-c o s" . org-ql-view-topics)
              ("C-c o o" . org-ql-view-noter)
              ("C-c o p" . org-ql-add-parents))
  :commands org-ql-defpred
  :config
  (org-agenda-action org-ql-view-noter
    (org-noter))
  (org-agenda-action org-ql-view-topics
    (okm-print-parents))
  (org-agenda-action org-ql-add-parents
    (okm-add-parent-topic)))

(defmacro org-agenda-action (name &rest body)
  (declare (indent defun))
  `(defun ,name ()
     (interactive)
     (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                        (org-agenda-error)))
            (buffer (marker-buffer marker))
            (pos (marker-position marker)))
       (when (okm-is-research-paper (buffer-file-name buffer))
         (with-current-buffer buffer
           (goto-char pos)
           ,@body)))))

(org-ql-defpred okm-parent (&rest args)
  "args - (pred ...parent-ids).
`pred' can be 'or or 'and."
  :body
  (let ((parents (okm-get-parents));;(org-entry-get-multivalued-property (point) okm-parent-property-name))
        (pred (car args))
        (parent-ids (--map (if (consp it) (cdr it) it) (cdr args))))
    (when parents
      (funcall
       (cond
        ((equal pred 'or) #'-any-p)
        ((equal pred 'and) #'-all-p)
        nil)
       (lambda (parent) (member parent parents)) parent-ids))))

(org-ql-defpred pdf-regexp (regexp)
  ""
  :body
  (let ((text-file (org-entry-get (point) "PDF_TEXT_FILE")))
    (when text-file
      (with-temp-buffer
        (insert-file-contents text-file)
        (s-match-strings-all regexp (buffer-string))))))

(defun okm-is-research-paper (path)
  (f-descendant-of-p (file-truename path) (file-truename (f-join okm-base-directory "research_papers"))))

(require 'okm-ql-view)

(defun okm-view-ql-or-roam-prompt (nodes title &optional query choice)
  "View nodes, in one of (org-ql-buffer org-roam-buffer). Prompt which if not specified."
  (unless choice
    (setq choice (intern-soft (completing-read "Choice: " '(org-roam org-ql) nil t))))
  (pcase choice
    ('org-ql (org-roam-ql-view nodes title query))
    ('org-roam (okm-org-roam-buffer-for-nodes nodes title (format "*%s*" title)))))

(defun okm-query-papers-by-topics (&optional topic-ids)
  "Query papers based on topics."
  (interactive)
  (let* ((topics (if topic-ids
                     (-map #'org-roam-node-from-id topic-ids)
                   (org-roam-node-read-multiple "Query topics: ")))
         (topic-ids (if topic-ids
                        topic-ids
                      (--map (org-roam-node-id it) topics)))
         ;; (topic-ids '("02f57fb4-4d54-41ba-88a1-2a969e926100" "89edeac4-8d67-402c-ab83-5e45cd7b97e6"))
         (grouped-results (-map
                           (lambda (el)
                             (-map #'cadr (cdr el)))
                           ;; assuming the grouping will result in all individual ids from topics being there
                           (-group-by #'car
                                      (org-roam-db-query [:select [ links:dest nodes:id ]
                                                                  :from links :inner :join nodes :on (= links:source nodes:id)
                                                                  :where (in links:dest $v1)
                                                                  :and (= links:type "brain-parent")]
                                                         (apply #'vector topic-ids))))))
    (okm-view-ql-or-roam-prompt
     (-filter
      (lambda (node)
        (okm-is-research-paper (org-roam-node-file node)))
      (-map #'org-roam-node-from-id
            (-uniq
             (if (eq (length grouped-results) 1)
                 (car grouped-results)
               (-reduce (pcase (completing-read "connector: " '(and or) nil t)
                          ("or" #'-union)
                          ("and" #'-intersection))
                        grouped-results)))))
     (format "(%s)" (s-join ", " (-map #'org-roam-node-title topics))))))

;; TODO: allow mulitiple combinations of brain-parent to be used (eg: (and (or ..) (or ..)))
(defun okm-query-papers-by-topic-with-ql ()
  "CONNECTOR."
  (interactive)
  (let* ((topics 
          (org-roam-node-read-multiple "Query topics: "))
         (connector (if (> (length topics) 1)
                        (pcase (completing-read "connector: " '(and or) nil t)
                          ("or" 'or)
                          ("and" 'and))
                      'and))
         (topic-ids (list (append `(okm-parent (quote ,connector))
                                  (mapcar
                                   (lambda (topic)
                                     `(quote ,(cons (org-roam-node-title topic) (org-roam-node-id topic))))
                                   topics))))
         (query (append '(and (level <= 1)) topic-ids))
         (after-change-major-mode-hook nil))
    (org-ql-search (f-glob "*.org" (f-join okm-base-directory "research_papers"))  query)))

;; (defun okm-query-papers-by-pdf-string (regexp)
;;   "query with org-ql REGEXP."
;;   (interactive "sRegexp: ")
;;   (let* ((query `(and (level <= 1) (pdf-regexp ,regexp))))
;;     (org-ql-search '("~/Documents/org/brain/research_papers/")  query)))

(defun okm-search-papers-by-pdf-string (regexp)
  "Search without opening org files."
  (interactive "xRegexp: ")
  (okm-view-ql-or-roam-prompt
   (-non-nil
    (--map
     (org-roam-node-from-id (caar (org-roam-db-query [:select node-id :from refs :where (= ref $s1)] (f-base it))))
     (-filter (lambda (f)
                (with-temp-buffer
                  (insert-file-contents f)
                  (cl-typecase regexp
                    (string 
                     (s-match-strings-all regexp (buffer-string)))
                    (symbol
                     (s-match-strings-all (symbol-name regexp) (buffer-string)))
                    (list
                     (--all-p (s-match-strings-all it (buffer-string)) regexp))
                    (t (error "Unknown type?")))))
              (f-glob "*.txt" bibtex-completion-library-path))))
   regexp `(pdf-regexp ,regexp)))

(defun amsha/get-sprints (states)
  "Return sprints based on STATUS."
  (org-ql-select (expand-file-name "work/projects.org" okm-base-directory)
    `(and (level 2) (todo ,@states) (h* "Sprint"))
    :action (lambda () (let ((todo-state (org-entry-get (point) "TODO")))
                         (cons
                          (format "%s %-10s - %-40s: %s"
                                  (--> "‣"
                                       (propertize it 'face (org-get-todo-face "INPROGRESS")))
                                  (propertize todo-state 'face (org-get-todo-face todo-state))
                                  (save-excursion
                                    (org-up-heading-safe)
                                    (s-replace-regexp
                                     "^<<[0-9]+>> " ""
                                     (--> (org-no-properties (org-get-heading t t t t))
                                          (propertize it 'face 'shadow))))
                                  (org-no-properties (org-get-heading t t t t)))
                          (org-id-get))))))

(defun okm-org-roam-is-parent (parent-id &optional child-id)
  "Return non-nil if PARENT-ID is in CHILD-ID's :brain-parent: property."
  (unless child-id
    (org-id-get-closest))
  (org-roam-db-query
   [:select :distinct [dest]
            :from links
            :where (= type $s1)
            :and (= dest $s2)
            :and (= source $s3)]
   okm-parent-id-type-name parent-id child-id))

(defun okm-org-get-parent-ids ()
  "."
  (--map (s-replace (format "%s:" okm-parent-id-type-name) "" it) (org-entry-get-multivalued-property (point) okm-parent-property-name)))

;; copied from `org-roam-backlinks-get'
(cl-defun okm-backlinks-get (node &key unique)
  "Return the brain-parent backlinks for NODE.

 When UNIQUE is nil, show all positions where references are found.
 When UNIQUE is t, limit to unique sources."
  (let* ((sql (if unique
                  [:select :distinct [source dest pos properties]
                   :from links
                   :where (= dest $s1)
                   :and (= type $s2)
                   :group :by source
                   :having (funcall min pos)]
                [:select [source dest pos properties]
                 :from links
                 :where (= dest $s1)
                 :and (= type $s2)]))
         (backlinks (org-roam-db-query sql (org-roam-node-id node) okm-parent-id-type-name)))
    (cl-loop for backlink in backlinks
             collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
                       (org-roam-populate
                        (org-roam-backlink-create
                         :source-node (org-roam-node-create :id source-id)
                         :target-node (org-roam-node-create :id dest-id)
                         :point pos
                         :properties properties))))))

(defun okm-query-boards ()
  "List the in progress items in the project boards directory.
Either show all or filter based on a sprint."
  (interactive)
  (let* ((files (f-glob "*/project_boards/*.org" okm-base-directory))
         (selection-list (append '(("ALL"))
                                 (amsha/get-sprints '("INPROGRESS" "TODO"))
                                 (--map (list (format "%s/project_boards::%s"
                                                      (f-base (f-parent (f-parent it)))
                                                      (file-name-base it)) it) files)))
         (predicate '(and (todo "INPROGRESS" "TODO")))
         (selection (--map (assoc it selection-list) (completing-read-multiple "Project topic: " selection-list nil t)))
         (selection-car (--map (car it) selection)))
    ;; (cond
    ;;  ;; if ALL is in the list, we have nothing more to do
    ;;  ((member "ALL" selection-car)
    ;;   nil)
    ;;  ((s-contains-p "/project_boards::" (car selection))
    ;;   (setq files (cdr selection)))
    ;;  ((not (string= (car selection) "ALL"))
    ;;   (setq predicate
    ;;         (append predicate
    ;;                 `((member ,(cdr selection)
    ;;                           (org-entry-get-multivalued-property (point) okm-parent-property-name)))))))
    (unless (member "ALL" selection-car)
      (let ((-files '()))
        (--map (cond
                ((s-contains-p "/project_boards::" (car it))
                 (push (cadr it) -files))
                ((not (string= (car it) "ALL"))
                 (setq predicate (append predicate`((member ,(cdr it)
                                                            (okm-org-get-parent-ids)))))))
               selection)
        (when -files
          (setq files -files))))
    (org-ql-search files predicate
      :super-groups (mapcar (lambda (x) (list :file-path (car (s-match "[^/]*/[^/]*/[^/]*\\.org" x)))) files)
      :title (format "%s" selection))))

(defun amsha/org-brain-children-topics (entry)
  "list parents of all the children of an ENTRY."
  (interactive (list (org-roam-node-read-multiple)))
  (let (topics other-parents)
    (mapcar (lambda (child-entry)
              (-let (((-topics . -other-parents) (okm-parents-by-topics (org-roam-node-id (org-roam-backlink-target-node child-entry)))))
                (setq topics (append topics -topics)
                      other-parents (append other-parents -other-parents))))
            (org-roam-backlinks-get entry))
    (setq topics (-uniq topics)
          other-parents (-uniq other-parents))
    (okm-print-parents topics other-parents)
    (cons topics other-parents)))

(defun org-ql-query-topics ()
  "List all parent topics of all results from QUERY.
Currently written to work in org-ql buffer."
  (interactive)
  (when (and org-ql-view-query org-ql-view-buffers-files)
    (let* (topics other-parents)
      (org-ql-select org-ql-view-buffers-files org-ql-view-query
        :action (lambda () (-let (((-topics . -other-parents) (okm-parents-by-topics (org-id-get))))
                             (setq topics (append topics -topics)
                                   other-parents (append other-parents -other-parents)))))
      (setq topics (-map #'org-roam-node-from-title-or-alias (-uniq topics))
            other-parents (-map #'org-roam-node-from-title-or-alias (-uniq other-parents)))
      ;;(okm-print-parents topics other-parents))))
      (let ((all-topics (append topics other-parents)))
        (org-roam-ql-view (-uniq all-topics) "Query parents" `(member (org-id-get) (list ,@(-map #'org-roam-node-id all-topics)))
                          (--map (list :file-path it) (list "research topics.org" "People.org" "Projects.org")))))))

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(use-package org-download
  :straight (org-download :type git :host github :repo "abo-abo/org-download"
                          :fork (:host github :repo "ahmed-shariff/org-download"))
  :custom
  (org-download-image-dir (f-join okm-base-directory "work/figures/"))
  (org-download-screenshot-method (if (eq system-type 'windows-nt) "magick convert clipboard: %s" "scrot"))

  :config
  (defun org-download--dir-2-use-id (oldfun &rest args)
    "Use ID if one of the parents has an ID or use the default behaviour of dir-2"
    (-if-let (id (save-excursion
                   (org-id-get-closest)))
        id
      (funcall oldfun args)))

  (defun org-download--dir-1-get-relative-path (&rest args)
    "Get relative path to `org-download-image-dir'."
    (or (when org-download-image-dir
          (f-relative org-download-image-dir))
        "."))
  (advice-add 'org-download--dir-1 :override #'org-download--dir-1-get-relative-path)
  (advice-add 'org-download--dir-2 :around #'org-download--dir-2-use-id))
 
  
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
  (format "`%s`" (replace-regexp-in-string "\n" " " (replace-regexp-in-string "- " "" (car kill-ring)))))

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

(defun research-papers-configure (&optional force-files)
  "With one C-u force process the file.
With C-u C-u prefix process all research-papers that have changed.
With C-u C-u C-u prefix, force run all research-papers."
  (interactive)
  (let ((force-files (or (member (car current-prefix-arg) `(4 ;; force just this file
                                                            64)) ;; force all files
                         force-files))
        (files-processed 0))
    (dolist-with-progress-reporter (f (if (and (not (null current-prefix-arg))
                                               (listp current-prefix-arg)
                                               (< 4 (car current-prefix-arg)))
                                          (f-files bibtex-completion-notes-path (lambda (f) (not (s-starts-with-p "." (f-base f)))))
                                        (--> (buffer-file-name)
                                             (when (and it (okm-is-research-paper it))
                                               (list it)))))
        "Processing research papers ..."
      (when (or force-files
                (not (string= (org-roam-db--file-hash f)
                              (caar (org-roam-db-query [:select hash :from files
                                                                :where (= file $s1)]
                                                       f)))))
        (org-roam-with-file f nil
	  (let* ((changes '())
                 (pom (point))
                 (link-string (org-entry-get pom "LINK"))
                 (link (if (string-empty-p link-string)
                           nil
                         link-string))
	         (cite-key (org-entry-get pom "Custom_ID"))
	         (dir bibtex-completion-library-path)
	         (tags (org-entry-get-multivalued-property pom "RPC-TAGS"))
	         (out-file-name (when cite-key (s-concat cite-key ".pdf")))
	         (full-path (when out-file-name (amsha/rename-full-path (expand-file-name out-file-name dir)))))

            (unless (org-entry-get pom "ATTACH_DIR")
	      (org-entry-put pom "ATTACH_DIR" dir)
              (push 'attach-dir changes))
	    (unless (org-id-get)
              (org-id-get-create)
              (push 'org-id changes))

            (setq tags (if link (delete "NO_LINK" tags) (append tags '("NO_LINK"))))
            (setq tags (if cite-key (delete "NO_CITE_KEY" tags) (append tags '("NO_CITE_KEY"))))

	    (when (and (not (member "ATTACH" tags))
                       full-path
                       (or (file-exists-p full-path)
                           (and link
                                cite-key
                                (condition-case nil
                                    (amsha/downlad-raname-move-file link out-file-name dir)
                                  (file-already-exists t)))))
              (progn
                (org-entry-put pom "Attachment" out-file-name)
                (setq tags (append tags '("ATTACH")))
                (org-entry-put pom "INTERLEAVE_PDF" full-path)
                (push 'interleve-pdf changes)))

            (when (and full-path (file-exists-p full-path))
              (let ((text-file-name (expand-file-name (format "%s.txt" (file-name-base full-path)) (file-name-directory full-path))))
                (unless (file-exists-p text-file-name)
                  (condition-case nil
                      (progn
                        (with-temp-buffer
                          (insert (amsha/pdf-to-text full-path))
                          (write-file text-file-name))
                        (org-entry-put pom "PDF_TEXT_FILE" (amsha/rename-full-path text-file-name)))
                    (error (message "Error: failed to read pdf file: %s" full-path)
                           (setq tags (append tags '("PDF_ERROR")))))
                  (push 'txt-file changes))))
            (setq tags
		  (if (remove okm-research-papers-id (okm-get-parents))
		      (delete "NO_PARENTS" tags)
		    (append tags '("NO_PARENTS"))))
            (when (cl-set-exclusive-or (org-entry-get-multivalued-property pom "RPC-TAGS") (delete-dups tags))
	      (apply #'org-entry-put-multivalued-property pom "RPC-TAGS" (delete "nosiblings" (delete-dups tags)))
              (push 'rpc-tags changes))
            (when (and cite-key (not (org-entry-get nil "ROAM_REFS")))
              (org-entry-put nil "ROAM_REFS" (format "cite:&%s" cite-key))
              (push 'roam-ref changes))
            (when changes
              (cl-incf files-processed)
              (save-buffer))))))
    (when files-processed
      (projectile-save-project-buffers))
      ;; (org-roam-db-sync)
    (message "Updated %s files" files-processed)
  ))
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
					      (org-entry-get-multivalued-property (point) okm-parent-property-name)))
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

(defun okm-add-parents (parents &optional entry-id)
  "Add PARENTS, which are expected to be ids to the entry with ENTRY-ID or in entry at point."
  (unless entry-id
    (setq entry-id (org-id-get-closest)))
  (cl-assert entry-id nil "entry-id cannot be nil/not under a valid entry.")
  (save-excursion
    (org-id-goto entry-id)
    (apply #'org-entry-put-multivalued-property (point) okm-parent-property-name
           (-uniq (append (org-entry-get-multivalued-property (point) okm-parent-property-name)
                   (--map (format "%s:%s" okm-parent-id-type-name it) parents))))))

(defun okm-get-parents (&optional entry-id)
  "Get the parent IDs for entry with id entry-id or in current entry.
Parent-child relation is defined by the brain-parent links."
  (unless entry-id
    (setq entry-id (org-id-get-closest)))
  (cl-assert entry-id nil "entry-id cannot be nil/not under a valid entry.")
  (save-excursion
    (org-id-goto entry-id)
    (--map (s-replace (concat okm-parent-id-type-name ":") "" it) (org-entry-get-multivalued-property (point) okm-parent-property-name))))

(defun okm-get-children (&optional entry-id)
  "Get the child IDs for entry with id entry-id or in the current entry.
Parent-child relation is defined by the brain-parent links."
  (unless entry-id
    (setq entry-id (org-id-get-closest)))
  (cl-assert entry-id nil "entry-id cannot be nil/not under a valid entry.")
  (--map (org-roam-node-id (org-roam-backlink-source-node it)) (okm-backlinks-get (org-roam-node-from-id entry-id) :unique t)))

(defun okm-add-parent-topic (&optional parents entry)
  "PARENTS should be a list of IDs. ENTRY should be an ID."
  (interactive)
  ;; forgoing interactive args to allow this to be called interactively.
  (unless parents
    (setq parents (--map (org-roam-node-id it) (org-roam-node-read-multiple "Add parents:"))))
  ;; (or (s-starts-with-p "People::" (car entry))
  ;;     (s-starts-with-p "research topics::" (car entry))
  ;;     (s-starts-with-p "misc_topics::" (car entry))
  ;;     (s-matches-p "work/projects::.*literature" (car entry))
  ;;     (s-starts-with-p "publication::" (car entry))))))
  (unless entry
    (setq entry (org-id-get-closest)))
  (let ((embark-quit-after-action nil))
    (okm-add-parents parents entry)
    (okm-print-parents entry)))

(defvar org-agenda-okm-add-parents--parents nil)

(defun org-agenda-okm-add-parents ()
  "To be used with the org-agenda-bulk-action."
  (unless org-agenda-okm-add-parents--parents
    (setq org-agenda-okm-add-parents--parents (-map #'org-roam-node-id (org-roam-node-read-multiple "Add parents: ")))
          org-agenda-bulk-action-post-execution-function (lambda ()
                                                           (setq org-agenda-okm-add-parents--parents nil)))
  (org-with-point-at (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error))
    (okm-add-parent-topic org-agenda-okm-add-parents--parents (org-id-get))))

(defun org-brain-goto-button-at-pt ()
  "."
  (interactive)
  (org-brain-goto (car (org-brain-button-at-point))))

(defun okm-parents-by-topics (&optional entry-id)
  "ENTRY-ID should be an ID."
  (interactive)
  (unless entry-id
    (setq entry-id (org-id-get-closest)))
  (let ((parents (--map (org-roam-node-from-id it) (okm-get-parents entry-id)))
        (research-topics-file (file-truename (f-join okm-base-directory "research topics.org")))
        (topics '())
        (other-parents '()))
    (mapcar (lambda (entry)
              (if (f-equal-p (file-truename (org-roam-node-file entry)) research-topics-file)
                  (push (org-roam-node-title entry) topics)
                (push (org-roam-node-title entry) other-parents)))
            parents)
    (cons topics other-parents)))

(defun okm-print-parents (&optional topics other-parents)
  "."
  (interactive)
  (when (and (null topics)
             (null other-parents))
    (-setq (topics . other-parents) (okm-parents-by-topics)))
  (message "%s \n********************************\n\t\t%s" (mapconcat 'identity (-list topics) "\n") (mapconcat 'identity (-list other-parents) "\n\t\t")))

(defun okm-delete-interleve-entry ()
  "Deletes the pdf entry of an okm entry bib at point at point."
  (interactive)
  (when (y-or-n-p "Sure you want to delete the pdf file and the interleve entry here? ")
    (save-excursion
      (goto-char (org-entry-beginning-position))
      (when-let* ((pdf-file (org-entry-get (point) "INTERLEAVE_PDF"))
                  (pdf-exists (file-exists-p pdf-file)))
        (delete-file pdf-file nil))
      (when-let* ((pdf-text-file (org-entry-get (point) "PDF_TEXT_FILE"))
                  (pdf-text-file-exists (file-exists-p pdf-text-file)))
        (delete-file pdf-text-file nil))
      (org-entry-delete (point) "Attachment")
      (org-entry-delete (point) "INTERLEAVE_PDF")
      (org-entry-delete (point) "PDF_TEXT_FILE")
      (org-entry-put-multivalued-property
       (point) "RPC-TAGS"
       (delete "PDF_ERROR"
               (delete "nosiblings"
                       (delete "ATTACH"
                               (delete-dups (org-entry-get-multivalued-property (point) "RPC-TAGS")))))))))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map "\C-c!" 'org-time-stamp-inactive)
	    (define-key org-mode-map "\C-coo" 'org-noter)
	    (define-key org-mode-map "\C-cop" 'okm-add-parent-topic)
	    (define-key org-mode-map "\C-coc" 'research-papers-configure)
            (define-key org-mode-map "\C-cos" 'okm-print-parents)
            (define-key org-mode-map "\C-coa" 'org-asana-hydra/body)
            (define-key org-mode-map (kbd "C-'") nil)
            (define-key org-mode-map "\C-c/" nil)
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

(use-package asana
  :straight (asana :type git :host github :repo "ahmed-shariff/emacs-asana")
  :config
  (setq asana-token (gethash 'asana-token configurations))

  (defun org-asana-get-project-id ()
    "Return the ASANA_ID from the file, which is the project ID."
    (when (equalp major-mode 'org-mode)
      (save-excursion
        (org-entry-get 0 "ASANA_ID"))))

  (defun org-asana-update-section-ids ()
    ""
    (interactive)
    (-when-let (sections (asana-get (concat "/projects/" (org-asana-get-project-id) "/sections")))
      (save-excursion
        (org-entry-put 0 "ASANA_SECTIONS" (format "%s" (--map (cons (asana-assocdr 'name it) (asana-assocdr 'gid it)) sections))))))

  (defun org-asana-get-todo-id (todo-state)
    ""
    (when todo-state (format "%s" (asana-assocdr (read todo-state) (read (org-entry-get 0 "ASANA_SECTIONS"))))))

  (defun org-asana-get-todo-state (todo-id)
    ""
    (format "%s" (asana-rassocar (read todo-id) (read (org-entry-get 0 "ASANA_SECTIONS")))))

  (defun org-asana-get-tasks ()
    ""
    (-when-let* ((project-id (org-asana-get-project-id))
                 (task-ids (--map (asana-assocdr 'gid it) (asana-get (concat "/projects/" project-id "/tasks")))))
      task-ids))
  

  (defun org-asana-push-new-tasks ()
    (interactive)
    (org-map-entries (lambda ()
                       (unless (org-entry-get (point) "ASANA_ID")
                         (-when-let (task-id (asana-assocdr 'gid (asana-post "/tasks" `(("projects" . (,(org-asana-get-project-id)))
                                                                                        ("name" . ,(org-entry-get (point) "ITEM"))))))
                           (asana-post (concat "/sections/"
                                               (org-asana-get-todo-id
                                                (org-entry-get (point) "TODO"))
                                               "/addTask")
                                       `(("task" . ,task-id)))
                           (org-entry-put (point) "ASANA_ID" task-id))))
                     "LEVEL=1"))

  (defun org-asana--get-todo-state-from-task (task)
    (org-asana-get-todo-state
     (asana-assocdr 'gid
                    (asana-assocdr 'section
                                   (car (asana-assocdr 'memberships task))))))

  (defun org-asana-pull-new-tasks ()
    "Gets all tasks from the asnaa project board and create headings for tasks not already in the project org file."
    (interactive)
    (let ((existing-tasks (-non-nil (org-map-entries (lambda () (org-entry-get (point) "ASANA_ID")) "LEVEL=1"))))
      (-map (lambda (task-id)
              (unless (member task-id existing-tasks)
                (-when-let (task (asana-get (format "/tasks/%s" task-id)))
                  (save-excursion
                    (goto-char (point-max))
                    (insert (format "\n* %s %s"
                                    (org-asana--get-todo-state-from-task task)
                                    (asana-assocdr 'name task)))
                    (org-id-get-create)
                    (org-entry-put (point) "ASANA_ID" (asana-assocdr 'gid task))))))
            (org-asana-get-tasks))))

  (defun org-asana-push-states ()
    (interactive)
    (let ((local-tasks (org-ql-select (current-buffer) '(level 1) :action (lambda () (cons (org-entry-get (point) "ASANA_ID") (org-id-get))))))
      (-map (lambda (task-id)
              (-when-let (task (asana-get (format "/tasks/%s" task-id)))
                (save-excursion
                  (org-id-goto (asana-assocdr task-id local-tasks))
                  (unless (equalp (org-entry-get (point) "TODO") (org-asana--get-todo-state-from-task task))
                    (asana-post (concat "/sections/"
                                        (org-asana-get-todo-id
                                         (org-entry-get (point) "TODO"))
                                        "/addTask")
                                `(("task" . ,task-id))))
                  (-when-let* ((local-scheduled (org-entry-get (point) "SCHEDULED"))
                               (local-scheduled (parse-time-string local-scheduled)) ;; (SEC MIN HOUR DAY MON YEAR DOW DST TZ)
                               (local-due-on (format "%s-%02d-%02d" (nth 5 local-scheduled) (nth 4 local-scheduled) (nth 3 local-scheduled))))
                    (unless (equalp (asana-assocdr 'due_on task) local-due-on)
                      (asana-put (format "/tasks/%s" task-id)
                                 `(("due_on" . ,local-due-on)))))
                  (when (and (org-entry-get (point) "CLOSED") (asana-assocdr 'due_on task))
                    (asana-put (format "/tasks/%s" task-id)
                               `(("completed" . t)))))))
            (org-asana-get-tasks))))

  (defun org-asana-pull-states ()
    "Pull all tasks from the project org file and update the states (todo and schedule)."
    (interactive)
    (let ((local-tasks (org-ql-select (current-buffer) '(level 1) :action (lambda () (cons (org-entry-get (point) "ASANA_ID") (org-id-get))))))
      (-map (lambda (task-id)
              (-when-let (task (asana-get (format "/tasks/%s" task-id)))
                (save-excursion
                  (org-id-goto (asana-assocdr task-id local-tasks))
                  ;; setting schedule info before todo state to avoid todostate changes being overwritten
                  (-if-let (due-on (asana-assocdr 'due_on task))
                      (unless (asana-assocdr 'completed task)
                        (org-schedule :time due-on)))
                  (let ((new-state (org-asana--get-todo-state-from-task task)))
                    (unless (equalp (org-entry-get (point) "TODO") new-state)
                      (org-entry-put (point) "TODO" new-state)
                      (when (member new-state org-done-keywords)
                        (org-entry-put (point) "SCHEDULED" nil)))))))
            (org-asana-get-tasks))))

  (defhydra org-asana-hydra (:color blue :hint nil :post (message "Asana call complete"))
    "Asana actions."
    ("pn" org-asana-push-new-tasks "Push new tasks" :column "Push")
    ("ps" org-asana-push-states "Push states" :column "Push")
    ("p" (lambda ()
           (interactive)
           (org-asana-push-new-tasks)
           (org-asana-push-states)) "Push updates (tasks & states)" :column "Push")
    ("fn" org-asana-pull-new-tasks "Pull new tasks" :column "Pull")
    ("fs" org-asana-pull-states "Pull states" :column "Pull")
    ("f" (lambda ()
           (interactive)
           (org-asana-pull-new-tasks)
           (org-asana-pull-states)) "Pull updates (tasks & states)" :column "Pull")
    ("s" org-asana-update-section-ids "Updates todo section ids" :column "Pull")))


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

(defun org-archive--compute-location-to-dir (computed-location)
  "See ORG-ARCHIVE--COMPUTE-LOCATION, makes the locations move to dir named archive."
  (cons (let* ((f (car computed-location))
               (f (f-join (f-dirname f) "archive" (f-filename f))))
          (unless (f-exists-p f)
            (f-mkdir (f-dirname f)))
          f)
        (cdr computed-location)))

(advice-add 'org-archive--compute-location :filter-return #'org-archive--compute-location-to-dir)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done 'note
      org-log-into-drawer t
      org-deadline-warning-days 2
      org-clock-idle-time 10
      org-return-follows-link t
      org-return-follows-link t
      org-refile-use-outline-path "file"
      org-outline-path-complete-in-steps t
      org-completion-use-ido t
      org-attach-directory "~/Documents/org/documents/"
      org-clock-continuously t
      org-clock-idle-time 10
      org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d"
      org-image-actual-width (list 650)
      org-agenda-files (flatten-tree (list (--map (f-files it (lambda (f)
                                                                (and (f-ext-p f "org")
                                                                     (with-temp-buffer
                                                                       (insert-file f)
                                                                       (if-let (kwds (org-collect-keywords '("filetags")))
                                                                           (not (member "agenda-untrack" (split-string (cadar kwds) ":" 'omit-nulls)))
                                                                         t)))))
                                                           (f-glob "~/Documents/org/brain/*/project_boards"))
                                           (f-files "~/Documents/org/brain/roam-notes"
                                                    (lambda (f)
                                                      (and (f-ext-p f "org")
                                                           (with-temp-buffer
                                                             (insert-file f)
                                                             (when-let (kwds (org-collect-keywords '("filetags")))
                                                               (member "agenda-track" (split-string (cadar kwds) ":" 'omit-nulls)))))))))
      org-export-with-broken-links t)

(provide 'orgZ)
;;; orgZ.el ends here
