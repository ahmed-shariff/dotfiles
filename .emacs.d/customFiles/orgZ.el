;;; orgZ.el --- a simple package                     -*- lexical-binding: t; -*-

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

(org-link-set-parameters okm-parent-id-type-name
                         :follow 'org-roam-id-open
                         :help-echo (lambda (_win _obj pos) (format "Link: %s" (org-roam-node-title
                                                                                (org-roam-node-from-id
                                                                                 (substring
                                                                                  (cadr (get-text-property (point) 'htmlize-link))
                                                                                  (1+ (length okm-parent-id-type-name))))))))


(magit-sync-repo "org" "~/Documents/org" git-message ("brain/research_papers" "brain/roam-notes" "brain/work/figures" "brain/work/notes" "brain/personl/work"))

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
    (let* ((url (s-replace-regexp "\\(https:/\\)[^/]" "https://" (plist-get data :url) nil nil 1))
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
              (with-current-buffer (find-file-noselect (car bibtex-completion-bibliography))
                (goto-char (point-max))
                (when (not (looking-at "^")) (insert "\n\n")))
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
              (save-buffer)
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
      org-startup-folded 'content
      org-id-track-globally t
      org-id-locations-file "~/.emacs.d/.org-id-locations"
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)


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

(repeatize 'org-babel-map)

(setq org-babel-load-languages
      (append org-babel-load-languages '((ruby . t)
				         (plantuml . t)
				         (emacs-lisp . t)
				         (python . t)
                                         (R . t)
				         (shell . t))))

(when (gethash 'use-jupyter configurations t)
  (use-package jupyter
    :straight (jupyter :includes (jupyter-org-client)
                       :files (:defaults "*.el"))
    :bind (:map org-mode-map
                ("C-c o j" . jupyter-org-hydra/body))
    :custom
    (org-babel-jupyter-resource-directory "jupyter-output")
    :config
    (add-to-list 'org-babel-load-languages '(jupyter . t))
    (amsha/reload-org-babel-langs)
    (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
						         (:session . "py")
						         (:kernel . "python3")
						         (:tangle . "jupyter-python/tangled.py")
						         (:exports . "both")))
    (define-key jupyter-org-interaction-mode-map (kbd "C-c h") nil)

    (cl-defmethod jupyter-handle-execute-reply :around ((_client jupyter-org-client) (req jupyter-org-request) msg)
      (org-with-point-at (jupyter-org-request-marker req)
        (cl-call-next-method))))

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

(defun amsha/reload-org-babel-langs ()
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(with-eval-after-load 'org
  (amsha/reload-org-babel-langs))

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
             `(parent (heading ,(s-replace-regexp "/$" "" (car org-refile-history))))
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
  (let* ((targets
          (append
           '(("--None--"))
           (--map
            (let* ((title (org-roam-node-title it))
                   (full-file-name (org-roam-node-file it))
                   (file-name (format "%s/%s" (f-base (f-parent (f-parent full-file-name))) (file-name-base full-file-name)))
                   (todo-state (or (org-roam-node-todo it) "")))
              (list (format "%-10s  %-30s %s"
                            (propertize todo-state 'face (org-get-todo-face todo-state))
                            (propertize file-name 'face 'marginalia-documentation)
                            title)
                    title
                    (org-roam-node-id it)))
            (org-roam-ql-nodes '(and (level 1) (file "./project_boards/."))))))
         (target (progn
                   (assoc (completing-read "Select task: " targets nil t) targets))))
    (if (cdr target)
        (format "**** [[id:%s][%s]]  %%?"
                (nth 2 target)
                (nth 1 target))
      "**** %?")))
    
(defun okm-add-repository ()
:PROPERTIES:
:END:
  "Take a repo link and add that to the file as a node."
  (let* ((link (read-string "Repository url: "))
         (title (cond
                 ((s-match "github" link)
                  (format "github/%s"
                          (s-replace ".git" "" (car (last (s-split "/" link))))))
                 (t (read-string "Title: " link)))))
    (format "%s
  :PROPERTIES:
  :ID:      %s
  :END:
- %s" title (org-id-new) link)))
          
;; (defun org-ask-location ()
;;   org-project-sprint-target-heading) 

(setq org-capture-templates
      '(("i" "hmmmm....somthing!*light bulb*->TO THE NOTES"
	 entry (file+olp+datetree "~/Documents/org/notes.org")
	 "* NOTE %^g\n\tAdded: %U\n\t%?")
	("t" "A thing i have to do(a wonderfull epiphany? 3:))->LIFE HAPPENS"
	 entry (file "~/Documents/org/brain/personal/life.org")
	 "* TODO %^{Description}
  :PROPERTIES:
  :ID:       %(org-id-new)
  :END:")
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
** Related repos:
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
	 :jump-to-captured t)
        ("er" "Add repository"
	 entry (file "~/Documents/org/brain/repositories.org")
	 "* %(okm-add-repository)"
	 :jump-to-captured t)))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let* (org-log-done
         org-log-states
         (inprogress 0)
         (level (1+ (org-outline-level)))
         (target-state (cond
                        ((= n-not-done 0) "DONE")
                        ((> n-done 0) "INPROGRESS")
                        (t "TODO"))))
    (unless (string= (org-get-todo-state) target-state)
      (org-todo target-state))))


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
    :custom
    (org-modern-star '("◉" " ○" "  ◈" "   ◇" "    •" "     ◦" "      ▸" "       ▹"))
    (org-modern-block-fringe nil)
    :hook ((org-mode . org-modern-mode)
           (org-agenda-finalize . org-modern-agenda))
    :custom-face
    (org-modern-label ((t :height 0.9
                        (:box
                         (:line-width
                          (1 . -1)
                          :color "#777c80" :style nil))))))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-transclusion
  :after org
  :defer 2
  :bind (:map org-mode-map
              ("C-c o t" . org-transclusion-hydra/body))
  :custom-face
  (org-transclusion-fringe ((t (:background "Turquoise" :weight ultra-bold :width extra-expanded))))
  (org-transclusion-source-fringe ((t (:background "Brown" :weight ultra-bold :width extra-expanded))))
  (org-transclusion ((t (:background "#222222"))))
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)

  (defun org-transclusion-content-filter-org-only-contents-exclude-id-headline (old-func data)
    "only-contents only excludes the id headlines."
    (if (and (eq (org-element-type data) 'headline)
             (org-element-property :ID data))
        nil
      data))

  (advice-add 'org-transclusion-content-filter-org-only-contents
              :around
              #'org-transclusion-content-filter-org-only-contents-exclude-id-headline)

  ;; (defun org-transclusion-content-insert-add-overlay (beg end)
  ;;   "Add fringe after transclusion."
  ;;   (overlay-put (text-clone-make-overlay beg end (current-buffer))
  ;;                'line-prefix
  ;;                (org-transclusion-propertize-transclusion))
  ;;   (overlay-put (text-clone-make-overlay beg end (current-buffer))
  ;;                'wrap-prefix
  ;;                (org-transclusion-propertize-transclusion)))

  ;; (add-hook 'org-transclusion-after-add-functions #'org-transclusion-content-insert-add-overlay)

  (defun org-transclusion-add-from-org-roam ()
    "Add transclusion from org-roam."
    (interactive)
    (unwind-protect
        (atomic-change-group
          (--> (org-roam-node-read nil nil nil t "Transclud node")
               (insert "#+transclude: "
                       (org-link-make-string
                        (concat "id:" (org-roam-node-id it))
                        (org-roam-node-formatted it))
                       " :only-contents :exclude-elements \"drawer keyword\"")))))

  (defhydra org-transclusion-hydra (:color blue)
    "Transclusion functions"
    ("m" org-transclusion-make-from-link "Make link")
    ("a" org-transclusion-add "Add")
    ("A" org-transclusion-add-all "Add all")
    ("d" org-transclusion-remove "Remove")
    ("D" org-transclusion-remove-all "Remove all")
    ("t" org-transclusion-mode "org-transclusion-mode")
    ("L" org-transclusion-make-from-link "make from link")
    ("R" org-transclusion-add-from-org-roam "from roam")))

(use-package bibtex-completion
  :custom
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  :config
  (setf (alist-get 'org-mode bibtex-completion-format-citation-functions) (lambda (keys) (s-join "," (--map (format "cite:&%s" it) keys)))))

(use-package emacsql
  :straight (emacsql :includes (emacsql-sqlite)
                     :files (:defaults "*.el"))
  :ensure t)

;; On windows when the `cygwin1.dll mismatch issue` issue happens, This is solved by manually running the command seen in the *compilation* buffer
;; Would have to try that on the msys2 console
(use-package org-roam
  :after (org emacsql)
  :defer 2
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :hook (kill-emacs-hook . amsha/backup-org-roam-db)
  :custom
  (org-roam-database-connector 'sqlite-builtin)  ;;sqlite-builtin sqlite
  (org-roam-directory (file-truename okm-base-directory))
  (org-roam-file-extensions '("org" "org_archive"))
  (org-roam-dailies-directory "dailies")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "%(okm-board-task-location)"
      :target (file+head
               "~/Documents/org/brain/work/notes/%<%Y-%m-%d-%A>.org" "#+title: %<%Y-%m-%d-%A>\n\n"))
     ("p" "personal" entry "%(okm-board-task-location)"
      :target (file+head
               "~/Documents/org/brain/personal/notes/%<%Y-%m-%d-%A>.org" "#+title: %<%Y-%m-%d-%A>\n\n"))))
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
     ("t" "A thing i have to do(a wonderfull epiphany? 3:))->LIFE HAPPENS" entry "* TODO ${title}%?
  :PROPERTIES:
  :ID:       %(org-id-new)
  :END:"
      :target (file+head "~/Documents/org/brain/personal/life.org"
                         "#+title: ${title}\n"))
     ("n" "new note" plain "%?"
     :target (file+head "~/Documents/org/brain/roam-notes/${slug}-%<%Y%m%d%H%M%S>.org"
                        "#+title: ${title}\n")
     :unnarrowed t)))
  (org-roam-node-display-template "${title}")
  (org-roam-mode-sections (list #'org-roam-brain-children-section
                                '(org-roam-backlinks-section :unique nil) ;; Setting to nil becuase when t it doesn't work too well with notes
                                #'org-roam-reflinks-section))
  :bind (("C-c n l" . org-roam-buffer-for-node)
         ("C-c n L" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ;;("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n y" . org-roam-db-sync)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ;; org-roam-bibtex
         ("C-c n b" . orb-insert-link)
         :map org-roam-node-map
         ("C-c o s" . org-roam-node-view-topics)
         ("C-c o o" . org-roam-node-view-noter)
         ("C-c o p" . org-roam-node-add-parents)
         :map org-roam-preview-map
         ("C-c o s" . org-roam-node-view-topics)
         ("C-c o o" . org-roam-node-view-noter)
         ("C-c o p" . org-roam-node-add-parents))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)

  (defmacro org-roam-node-action (name &rest body)
    (declare (indent defun))
    `(defun ,name ()
       (interactive)
       (when-let* ((node (org-roam-node-at-point))
                   (buffer (find-file-noselect (org-roam-node-file node)))
                   (pos (org-roam-node-point node)))
           (when (okm-is-research-paper (buffer-file-name buffer))
             (save-window-excursion
               (with-current-buffer buffer
                 (goto-char pos)
                 ,@body))))))

  (org-roam-node-action org-roam-node-view-noter
    (org-noter))
  (org-roam-node-action org-roam-node-view-topics
    (okm-print-parents))
  (org-roam-node-action org-roam-node-add-parents
    (okm-add-parent-topic))

  (defun amsha/backup-org-roam-db ()
    (interactive)
    (copy-file org-roam-db-location (format "%s-backup" org-roam-db-location) t))

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

  (defun org-roam-buffer-for-node (node)
    "Display org-roam-buffer for NODE."
    (interactive (list (org-roam-node-read (when (derived-mode-p 'org-roam-mode)
                                             (when-let (_node (org-roam-node-at-point))
                                               (org-roam-node-title _node))))))
    ;; copied from `org-roam-buffer-toggle' and `org-roam-buffer-persistent-redisplay'
    (display-buffer (get-buffer-create org-roam-buffer))
    (unless (equal node org-roam-buffer-current-node)
      (setq org-roam-buffer-current-node node
            org-roam-buffer-current-directory org-roam-directory)
      (with-current-buffer (get-buffer-create org-roam-buffer)
        (org-roam-buffer-render-contents)
        (add-hook 'kill-buffer-hook #'org-roam-buffer--persistent-cleanup-h nil t))))

  (defun org-roam-brain-children-section (node)
    "The brain children section for NODE.
Copied  from `org-roam-backlink-get'."
    (when-let ((backlinks (seq-sort #'org-roam-backlinks-sort (okm-links-get node))))
      (magit-insert-section (org-roam-brain-children)
        (magit-insert-heading "Brain children:")
        (dolist (backlink backlinks)
          (org-roam-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink)))
        (insert ?\n))))

  (defun org-roam-subtree-aware-preview-function ()
    "Same as `org-roam-preview-default-function', but gets entire subtree in research_papers or notes."
    (if (--> (org-roam-node-at-point)
             (org-roam-node-file it)
             (or (s-matches-p "brain/work/notes" it)
                 (s-matches-p "brain/personal/notes" it)
                 (f-ancestor-of-p bibtex-completion-notes-path it)))
        (let ((beg (progn (if (org-id-get)
                              (org-roam-end-of-meta-data t)
                            (org-beginning-of-line))
                          (point)))
              (end (progn (when (org-id-get)
                            (org-previous-visible-heading 1)
                            (org-beginning-of-line))
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
         ("C-c n N" . consult-ripgrep-roam-notes)
         ("C-c n v" . consult-notes-visit-relation))
  :commands (consult-notes
             consult-notes-search-in-all-notes
             consult-notes-org-roam-find-node
             consult-ripgrep-roam-notes
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
                     (plist-multi-put (copy-sequence consult-notes-org-roam--nodes)
                                      :name (propertize "Backlinks" 'face 'consult-notes-sep)
                                      :narrow ?b
                                      :items (lambda () (--map (org-roam-node-title (org-roam-backlink-source-node it))
                                                               (org-roam-backlinks-get node :unique t))))
                     (plist-multi-put (copy-sequence consult-notes-org-roam--nodes)
                                      :name (propertize "Brain Children" 'face 'consult-notes-sep)
                                      :narrow ?c
                                      :items (lambda () (--map (org-roam-node-title (org-roam-node-from-id it))
                                                               (okm-get-children (org-roam-node-id node)))))
                     (plist-multi-put (copy-sequence consult-notes-org-roam--nodes)
                                       :name (propertize "Forwardlink" 'face 'consult-notes-sep)
                                       :narrow ?f
                                       :items (lambda () (-map #'car (org-roam-db-query
                                                                      [:select :distinct nodes:title
                                                                               :from links :inner :join nodes :on (= links:dest nodes:id)
                                                                               :where (in links:source $v1)]
                                                                      (vector (org-roam-node-id node))))))
                     (plist-multi-put (copy-sequence consult-notes-org-roam--nodes)
                                      :name (propertize "Brain Parents" 'face 'consult-notes-sep)
                                      :narrow ?p
                                      :items (lambda () (--map (org-roam-node-title (org-roam-node-from-id it))
                                                               (okm-get-parents (org-roam-node-id node)))))
                     )
                    :require-match t
                    :prompt "Related nodes:"))
  ;; From https://github.com/minad/consult/issues/597
  (defun consult-ripgrep-roam-notes ()
    "search notes files."
    (interactive)
    (let ((consult-ripgrep-args
           (string-replace
            "."
            (format "%s %s" ;; Absolute paths!
                    (file-truename "~/Documents/org/brain/personal/notes")
                    (file-truename "~/Documents/org/brain/work/notes"))
            consult-ripgrep-args)))
      (consult-ripgrep))))


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
  :after (org-ref)
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
  :requires org-keys
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
  ;; (setf (alist-get 'title doi-utils-json-metadata-extract) '((concat (plist-get results :title)
  ;;                                                                    (-if-let (subtitle (plist-get results :subtitle))
  ;;                                                                        (format ": %s" subtitle)
  ;;                                                                      ""))))
  (defun org-ref-latex-click ()
    "Open bibtex hydra in latex buffer."
    (interactive)
    (org-ref-citation-hydra/body))

  (defun org-ref-get-bibtex-key-under-cursor-with-latex-and-okm (old-func)
    (cond
     ((derived-mode-p 'latex-mode)
      (bibtex-completion-get-key-latex))
     ((and (derived-mode-p 'org-mode) (org-entry-get (point) "Custom_ID"))
      (org-entry-get (point) "Custom_ID"))
     (t
      (funcall old-func))))

  (advice-add 'org-ref-get-bibtex-key-under-cursor :around #'org-ref-get-bibtex-key-under-cursor-with-latex-and-okm)
  ;; (advice-remove 'org-ref-get-bibtex-key-under-cursor #'org-ref-get-bibtex-key-under-cursor-with-latex)

  (require 'org-ref-latex)

  (defun org-ref-latex-get-bibliography-or-default (return-val)
    "Use `bibtex-completion-bibliography' if `org-ref-latex-get-bibliography' returns nil."
    (or return-val bibtex-completion-bibliography))

  (advice-add 'org-ref-latex-get-bibliography :filter-return #'org-ref-latex-get-bibliography-or-default)

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

  ;; Modifying the `org-ref-open-url-at-point'.
  (defun org-ref-get-url-at-point ()
    "Get the url for bibtex key under point."
    (interactive)
    (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	   (results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (bibfile (cdr results)))
      (save-excursion
        (with-temp-buffer
          (insert-file-contents bibfile)
          (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
          (bibtex-search-entry key)
          ;; I like this better than bibtex-url which does not always find
          ;; the urls
          (or
            (let ((url (s-trim (bibtex-autokey-get-field "url"))))
              (unless (s-blank? url)
                url))

            (let ((doi (s-trim (bibtex-autokey-get-field "doi"))))
              (unless (s-blank? doi)
                (if (string-match "^http" doi)
                    doi
                  (format "http://dx.doi.org/%s" doi)))))))))

  (advice-add 'reftex-create-bibtex-file :before (lambda (&rest r) (reftex-reset-scanning-information)))

  (defun amsha/bib-keys-in-file (file)
    "bib keys in file"
    (let (bibtex-keys)
      (with-temp-buffer
        (insert-file file)
        (bibtex-map-entries
         (lambda (key _ _)
           (push key bibtex-keys))))
      bibtex-keys))

  (defun amsha/compare-keys-in-bib-files (file1 file2)
    "Compare and list out how the keys compare."
    (interactive (list (read-file-name "File 1: " default-directory nil t)
                       (read-file-name "File 2: " default-directory nil t)))
    (let ((bibtex-keys-1 (amsha/bib-keys-in-file file1))
          (bibtex-keys-2 (amsha/bib-keys-in-file file2)))
      (with-current-buffer (get-buffer-create "*comparison-of-keys*")
        (erase-buffer)
        (em bibtex-keys-1)
        (em bibtex-keys-2)
        (insert (format "Comparing files `%s` and `%s`\n" file1 file2)
                "\nCommon keys:\n============\n"
                (s-join "\n" (-intersection bibtex-keys-1 bibtex-keys-2))
                (format "\n\nKeys only in %s:\n============\n" file1)
                (s-join "\n" (-difference bibtex-keys-1 bibtex-keys-2))
                (format "\n\nKeys only in %s:\n============\n" file2)
                (s-join "\n" (-difference bibtex-keys-2 bibtex-keys-1))))
      (switch-to-buffer "*comparison-of-keys*")))

  (defun amsha/compare-keys-in-bib-file-and-copy-bib (file1 file2)
    "Copy bib entries in file1 and not in file2 to buffer named *new-bib*"
    (interactive (list (read-file-name "File 1 (copy from): " default-directory nil t)
                       (read-file-name "File 2 (check in): " default-directory nil t)))
    (let ((bibtex-keys-1 (amsha/bib-keys-in-file file1))
          (bibtex-keys-2 (amsha/bib-keys-in-file file2))
          (bib-file-buffer (get-buffer-create "*new-bib*")))
      (with-current-buffer bib-file-buffer
        (erase-buffer))
      (--map
        (save-excursion
          (find-file (car bibtex-completion-bibliography))
          (bibtex-search-entry it)
          (bibtex-copy-entry-as-kill)
          (with-current-buffer bib-file-buffer
            (bibtex-yank)))
        (-difference bibtex-keys-1 bibtex-keys-2))
      (switch-to-buffer "*comparison-of-keys*"))))


;; Moved out of use-pacakge to avoid errors with loading
(with-eval-after-load 'org-ref-citation-links
  (defhydra+ org-ref-citation-hydra ()
    ("t" (lambda ()
           (interactive)
           (save-excursion
             (org-ref-open-notes-at-point)
             (org-noter)))
     "open noter" :column "Open")
    ("l" (lambda ()
           (interactive)
           (-if-let (url (org-ref-get-url-at-point))
               (kill-new url)
             (user-error "No url copied")))
     "Copy url" :column "Copy")))


(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(defun doi-add-bibtex-entry-with-note ()
  "."
  (interactive)
  (call-interactively #'doi-utils-add-bibtex-entry-from-doi)
  (find-file (car bibtex-completion-bibliography))
  (org-ref-open-bibtex-notes)
  (org-set-property "LINK" (completing-read "LINK: " nil nil nil (when (s-starts-with-p "file://" (car kill-ring))
                                                                   (car kill-ring))))
  (research-papers-configure t))

;; (use-package org-noter ;;:quelpa (org-noter :fetcher github :repo "ahmed-shariff/org-noter")
;;   :straight (org-noter :type git :host github :repo "weirdNox/org-noter"
;;                        :fork (:host github :repo "ahmed-shariff/org-noter"))
(use-package org-noter
  :straight (org-noter :host github :type git :repo "org-noter/org-noter"
                       :files ("*.el" "modules/*.el"))
  :config
  (setq org-noter-property-doc-file "INTERLEAVE_PDF"
        org-noter-property-note-location "INTERLEAVE_PAGE_NOTE"
        org-noter-highlight-selected-text t)

  ;; The evil related functions seems to be adding a binding to "q" for `quit-window' in the normal mode
  ;; Its there in the `evil-collection-eldoc-doc-buffer-mode-map' `special-mode-map' and another one?
  ;; For now advicing the `quit-window' to let me kill session when quitting without interuppting the function of "q"
  ;; bound by other maps.
  (defun org-noter-quit-window-kill-session (oldfun &rest rest)
    (if org-noter--session
        (org-noter-kill-session org-noter--session)
      (apply oldfun rest)))

  (advice-add 'quit-window :around #'org-noter-quit-window-kill-session)

  (defun org-noter-pdf--get-selected-text-single-linified (vals)
    "Single linyfy the returned text."
    (when vals (replace-regexp-in-string "\n" " " (replace-regexp-in-string "- " "" vals))))

  (advice-add 'org-noter-pdf--get-selected-text :filter-return #'org-noter-pdf--get-selected-text-single-linified))

;; (use-package org-brain ;;:quelpa (org-brain :fetcher github :repo "ahmed-shariff/org-brain" :branch "fix322/symlink_fix")
;;   :straight (org-brain :type git :host github :repo "Kungsgeten/org-brain"
;;                        :fork (:host github :repo "ahmed-shariff/org-brain"))
;;   :demand
;;   :init
;;   (setq org-brain-path (file-truename "~/Documents/org/brain"))
;;   :bind (("C-c v" . org-brain-visualize)
;; 	 :map org-brain-visualize-mode-map
;; 	 ("\C-coo" . org-brain-open-org-noter)
;; 	 ("\C-cop" . okm-add-parent-topic)
;;          ("\C-cob" . org-brain-goto-button-at-pt)
;;          ("\C-cos". okm-print-parents))
;;   :config
;;   ;;(define-key org-brain-visualize-mode-map "")
;;   (defmacro org-brain-function-on-entry (fn)
;;   "Macro that generates a function which takes an entry and executes the fn while on a file entry."
;;   `(lambda (entry)
;;      (if (org-brain-filep entry)
;;          ""
;;        (org-with-point-at (org-brain-entry-marker entry)
;;          (or (funcall ,fn entry)
;; 	     "")))))

;;   (setq org-id-track-globally t
;;         org-id-locations-file "~/.emacs.d/.org-id-locations"
;;         org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
;;         org-brain-completion-system (lambda (&rest args) (s-join org-brain-entry-separator (apply #'completing-read-multiple args))))
;;   (push '("b" "Brain" plain (function org-brain-goto-end)
;;           "* %i%?" :empty-lines 1)
;;         org-capture-templates)
;;   (setq org-brain-visualize-default-choices 'all)
;;   (setq org-brain-title-max-length 50)
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

;; (defun org-brain-open-org-noter ()
;;   (interactive)
;;   (let ((entry (condition-case nil
;; 		   (car (org-brain-button-at-point))
;; 		 (user-error (org-brain-entry-at-pt)))))
;;     (if (org-brain-filep entry)
;; 	(user-error "Noter cannot be opened for file entry")
;;       (org-with-point-at (org-brain-entry-marker entry)
;; 	(if (string= "research_papers" (file-name-base (org-entry-get (point) "FILE")))
;; 	    (org-noter)
;; 	  (user-error "Noter only for the entries in research_paper"))))))

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

(defun ripgrep (pattern directory type)
  (-non-nil
   (--map
    (pcase-let ((`(,file ,line ,match) (s-split ":" (f-relative it directory))))
      `(,(expand-file-name file directory), (string-to-number line) ,match))
    (--filter
     (< 0 (length it))
     (s-split "\n"
              (with-temp-buffer
                (call-process "rg" nil t "--null"
                              "--line-buffered"
                              "--color=never"
                              "--max-columns=1000"
                              "--max-columns-preview"
                              "--path-separator" "/"
                              "--smart-case"
                              "--no-heading"
                              "--with-filename"
                              "--line-number"
                              "--search-zip"
                              "-j5"
                              (format "-t%s" type)
                              pattern
                              directory)
                (buffer-string)))))))

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
         (save-window-excursion
           (with-current-buffer buffer
             (goto-char pos)
             ,@body))))))

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

(use-package org-roam-ql
  :straight (org-roam-ql :type git :host github :repo "ahmed-shariff/org-roam-ql"
                         :files (:defaults (:exclude "org-roam-ql-ql.el")))
  :after (org-roam)
  :bind ((:map org-roam-mode-map
          ("v" . org-roam-ql-buffer-dispatch)
          :map minibuffer-mode-map
          ("C-c n i" . org-roam-ql-insert-node-title)))
  :custom
  (org-roam-ql-default-org-roam-buffer-query (lambda () `(backlink-to (id ,(org-roam-node-id org-roam-buffer-current-node)) :type nil)))
  :config
  ;; (defun okm-roam-view-query (source-or-query)
  ;;   "View source or query in org-roam buffer."
  ;;   (interactive "xQuery: ")
  ;;   (okm-org-roam-buffer-for-nodes (org-roam-ql-view--get-nodes-from-query source-or-query) (format "Query view: %s" source-or-query) "*org-roam query view*"))
  (org-roam-ql-defexpansion 'child-of
                            "Child of node with a specific title"
                            (lambda (title)
                              (org-roam-ql--expand-backlinks `(title ,title t) :type okm-parent-id-type-name)))
  (org-roam-ql-defexpansion 'regexp-rg
                            "Regex on all org files."
                            (lambda (regexp)
                              (-map
                               #'org-roam-node-from-id
                               (-non-nil (--map
                                          ;; on windows the path has a colon, hence making it relative then expanding again.
                                          (pcase-let ((`(,file ,line ,match) it))
                                            (org-roam-with-file file nil
                                              (goto-line line)
                                              (or (org-id-get-closest)
                                                  (progn
                                                    (goto-char (point-min))
                                                    (org-id-get)))))
                                          (ripgrep regexp org-roam-directory "org"))))))
  ;; (org-roam-ql-defpred 'pdf-string
  ;;                      "Attached PDF has string"
  ;;                      (lambda (node)
  ;;                        (cdr (assoc "PDF_TEXT_FILE" (org-roam-node-properties node))))
  ;;                      #'okm--test-regexp-on-file)

  (defun bibtex-keys-to-nodes (keys)
    "Return a list of org-roam-nodes for corresponding list of KEYS."
    (-map
     #'org-roam-node-from-id
     (-flatten (org-roam-db-query [:select id :from nodes :where (in file $v1)]
                                  (apply #'vector (--map (f-expand (format "%s.org" it) bibtex-completion-notes-path)
                                                         keys))))))


  (org-roam-ql-defexpansion 'pdf-string
                            "Regex on attached pdfs"
                            (lambda (regexp)
                              (bibtex-keys-to-nodes
                               (--map (f-base (car it)) (ripgrep regexp (file-truename bibtex-completion-library-path) "txt")))))

  (org-roam-ql-defexpansion 'last-n-papers
                            "The lasn N entried in research_papers"
                            (lambda (N)
                              (let ((idx -1)
                                    bibtex-keys)
                                (with-temp-buffer
                                  (insert-file (car bibtex-completion-bibliography))
                                  (bibtex-map-entries
                                   (lambda (key _ _)
                                     (setq idx (1+ idx))
                                     (push (cons key idx) bibtex-keys))))
                                (bibtex-keys-to-nodes (-map #'car (-take N bibtex-keys))))))

  (org-ql-defpred org-roam-backlink (&rest nodes) "Return if current node has bacnklink to any of NODES."
    :body
    (let* ((backlink-destinations (apply #'vector (-non-nil
                                                   (--map (cond
                                                          ((org-roam-node-p it) (org-roam-node-id it))
                                                          ((stringp it) (or (org-roam-node-from-id it)
                                                                            (ignore-errors
                                                                              (org-roam-node-id (org-roam-node-from-title-or-alias it)))))
                                                          (t nil))
                                                         nodes))))
           (id (org-id-get)))
      (org-roam-db-query [:select * :from links :where (in dest $v1) :and (= source $s2)] backlink-destinations id)))

  (defun okm-get-all-roam-nodes-in-file (f)
    (org-roam-ql-nodes (list [:select [id] :from nodes :where (= file $s1)] f)))

  (defun okm-org-roam-list-notes (entries)
    "Filter based on the list of ids (FILTER) in the notes files.
If prefix arg used, search whole db."
    (interactive (list ;;(org-roam-node-read nil nil nil 'require-match "Filter on Nodes:")))
                  (org-roam-node-read-multiple)))
    (let* ((entries (-uniq
                     (-flatten
                      (--map (let ((node (if (stringp it) (org-roam-node-from-title-or-alias it) it)))
                               (if (equal (org-roam-node-level node) 0)
                                   (list node (okm-get-all-roam-nodes-in-file (org-roam-node-file node)))
                                 node))
                             entries))))
           (names (s-join "," (--map (org-roam-node-title it) entries)))
           (title (format "(%s)" names))
           (buffer-name (format "*notes: %s*" names))
           (ids (apply #'vector (--map (org-roam-node-id it) entries))))
      ;; NOTE: Not using org-roam-ql-search because it works on nodes, we need it to work on links here.
      ;; i.e., it will show just the first node....
      (org-roam-ql--render-roam-buffer
       (list (--> (apply #'org-roam-db-query
                         (append (list (vector :select :distinct [links:source links:pos links:properties]
                                               :from 'links :inner :join 'nodes :on '(= links:source nodes:id)
                                               :where (if current-prefix-arg
                                                          '(in links:dest $v1)
                                                        '(and (in links:dest $v1)
                                                              (or (like nodes:file $s2)
                                                                  (like nodes:file $s3)))))
                                       ids)
                                 (unless current-prefix-arg
                                   (list "%brain/personal/notes%"
                                         "%brain/work/notes%"))))
                  (lambda ()
                    (magit-insert-section (org-roam)
                      (magit-insert-heading "Notes:")
                      (dolist (entry
                               ;; removing duplicates as the whole subtree will be getting displayed
                               ;;(seq-uniq
                               it)
                        (let* ((source-node (org-roam-node-from-id (car entry)))
                               (pos (cadr entry))
                               (properties (caddr entry))
                               (outline (plist-get properties :outline)))
                          (when outline
                            (setq properties (plist-put properties :outline
                                                        (cl-subseq outline
                                                                   0
                                                                   (1- (length outline))))))
                          (org-roam-node-insert-section :source-node source-node
                                                        :point pos
                                                        :properties properties))
                        (insert ?\n))
                      (run-hooks 'org-roam-buffer-postrender-functions)))))
       title buffer-name nil "title")))

  (defvar okm-org-roam-preview-kills '())

  (defun okm-org-roam-ql-copy-preview ()
    (interactive)
    (let* ((section (plist-get (text-properties-at (point))
                               'magit-section))
           (node (oref (oref section parent) node))
           (content (format "[[id:%s][link]] %s"
                            (org-roam-node-id node)
                            (org-roam-fontify-like-in-org-mode
                             (org-roam-preview-get-contents
                              (oref section file)
                              (oref section point))))))
      (setq okm-org-roam-preview-kills (append okm-org-roam-preview-kills `(,content)))
      (kill-new content)))

  (defun okm-org-roam-preview-kills-yank ()
    (interactive)
    (dolist (content okm-org-roam-preview-kills)
      (insert content "\n\n"))
    (setq okm-org-roam-preview-kills nil))

  (define-key org-roam-preview-map "w" #'okm-org-roam-ql-copy-preview))

(use-package org-roam-ql-ql
  :straight (org-roam-ql-ql :type git :host github :repo "ahmed-shariff/org-roam-ql"
                            :files (:defaults (:exclude "org-roam-ql.el")))
  :after (org-roam org-ql org-roam-ql)
  :config
  (org-roam-ql-ql-init))

(defun okm-query-papers-by-topics (&optional topic-ids)
  "Query papers based on topics."
  (interactive)
  (let* ((topics (if topic-ids
                     (-map #'org-roam-node-from-id topic-ids)
                   (org-roam-node-read-multiple "Query topics: ")))
         (topic-queries (--map `(child-of ,(org-roam-node-title it)) topics)))
    (org-roam-ql-search
     `(and (file "research_papers")
           ,(if (eq 1 (length topic-queries))
               (car topic-queries)
              `(,(pcase (completing-read "connector: " '(and or) nil t)
                   ("or" 'or)
                   ("and" 'and))
            ,@topic-queries)))
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

(defun okm--test-regexp-on-file (f regexp)
  (if (f-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (cl-typecase regexp
          (string
           (s-match-strings-all regexp (buffer-string)))
          (symbol
           (s-match-strings-all (symbol-name regexp) (buffer-string)))
          (list
           (--all-p (s-match-strings-all it (buffer-string)) regexp))
          (t (error "Unknown type?"))))
    (message "ERROR: Missing %s" f)
    nil))

(defun okm-search-papers-by-pdf-string (regexp)
  "Search without opening org files."
  (interactive "xRegexp: ")
  (let ((results (--filter
                  (cdr it)
                  (-map (lambda (f)
                          (cons (f-base f)
                                (okm--test-regexp-on-file f regexp)))
                        (f-glob "*.txt" bibtex-completion-library-path)))))
    (cl-letf (((symbol-function 'org-roam-preview-get-contents)
               (lambda (f point)
                 (propertize (s-join "\n" (--map (format " - %s" it) (assoc (f-base f) results))) 'face 'org-tag))))
      (org-roam-ql-search
       `(pdf-string ,regexp)
       (prin1-to-string regexp)))))

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
                                     (-as-> (--> (org-no-properties (org-get-heading t t t t))
                                                 (propertize it 'face 'shadow))
                                            it
                                            (if (> (length it) 40)
                                                (format "%s..." (substring it 0 37))
                                              it))))
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
(cl-defun okm-links-get (node &optional is-forwardlink &key unique)
  "Return the brain-parent link for NODE.
 If IS-FORWARDLINK is non-nil, get backlink, else get forwardlinks.

 When UNIQUE is nil, show all positions where references are found.
 When UNIQUE is t, limit to unique sources."
  (let* ((where-clause (if is-forwardlink 'source 'dest))
         (group-by-clause (if is-forwardlink 'dest 'source))
         (sql (if unique
                  (vector :select :distinct [source dest pos properties]
                          :from 'links
                          :where `(= ,where-clause $s1)
                          :and '(= type $s2)
                          :group :by group-by-clause
                          :having '(funcall min pos))
                (vector :select [source dest pos properties]
                        :from 'links
                        :where `(= ,where-clause $s1)
                        :and '(= type $s2))))
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

(defun okm-org-ql-query-topics ()
  "List all parent topics of all results from QUERY.
Currently written to work in org-ql buffer."
  (interactive)
  (when (and org-ql-view-query org-ql-view-buffers-files)
    (let* (topics other-parents)
      (org-ql-select org-ql-view-buffers-files org-ql-view-query
        :action (lambda () (-let (((-topics . -other-parents) (okm-parents-by-topics (org-id-get))))
                             (setq topics (append topics -topics)
                                   other-parents (append other-parents -other-parents)))))
      (setq topics (-map #'org-roam-node-from-title-or-alias (--filter (not (string-empty-p it)) (-uniq topics)))
            other-parents (-map #'org-roam-node-from-title-or-alias (--filter (not (string-empty-p it)) (-uniq other-parents))))
      ;;(okm-print-parents topics other-parents))))
      (let ((all-topics (append topics other-parents)))
        (org-roam-ql-search (-uniq all-topics) 'org-ql "Query parents" `(member (org-id-get) (list ,@(-map #'org-roam-node-id all-topics)))
                          (--map (list :file-path it) (list "research topics.org" "People.org" "Projects.org")))))))


(defun okm-org-roam-query-topics ()
  "List all parent topics of results in buffer."
  (interactive)
  (when (derived-mode-p 'org-roam-mode)
    (let ((nodes (org-roam-ql--nodes-from-roam-buffer (current-buffer)))
          (original-format-function (symbol-function 'org-roam-ql-view--format-node))
          topics
          topic-nodes)
      (dolist (node nodes)
        (-let (((-topics . -other-parents) (okm-parents-by-topics (org-roam-node-id node))))
          (setq topics (append topics -topics -other-parents))))
      (setq topics (--map (cons (car it) (length (cdr it)))
                          (--group-by it
                                      (--filter (not (string-empty-p it))
                                                topics)))
            topic-nodes (-non-nil (--map (org-roam-node-from-title-or-alias (car it)) topics)))
      (cl-letf (((symbol-function 'org-roam-ql-view--format-node)
                 ;; Copied from `org-roam-ql-view--format-node'
                 (lambda (node)
                   (let* ((marker
                           (org-roam-ql--get-file-marker node))
                          (properties (list
                                       'org-marker marker
                                       'org-hd-marker marker))
                          (string (format "(%s) %s"
                                          (cdr (assoc (org-roam-node-title node)
                                                      topics))
                                          (org-roam-node-title node))))
                     (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
                     (--> string
                          (concat "  " it)
                          (org-add-props it properties
                            'org-agenda-type 'search
                            'todo-state (org-roam-node-todo node)))))))
        (org-roam-ql--agenda-buffer-for-nodes topic-nodes "Topics"
                                              (org-roam-ql--get-formatted-buffer-name
                                               (org-roam-ql--get-formatted-title
                                                (format "Topics - %s" (prin1-to-string org-roam-ql-buffer-title)) nil))
                                              `(backlinks-to ,org-roam-ql-buffer-query :type ,okm-parent-id-type-name)
                                              (--map
                                               (list :file-path it)
                                               (list "research topics.org" "People.org" "Projects.org")))))))


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
      ;; args not used
      (funcall oldfun)))

  (defun org-download--dir-1-get-relative-path (&rest args)
    "Get relative path to `org-download-image-dir'."
    (or (when org-download-image-dir
          (f-relative org-download-image-dir))
        "."))
  (advice-add 'org-download--dir-1 :override #'org-download--dir-1-get-relative-path)
  (advice-add 'org-download--dir-2 :around #'org-download--dir-2-use-id)

  (defun download-screenshot (file-name)
    "Capture screenshot and yank the resulting file name.
The screenshot tool is determined by `org-download-screenshot-method'."
    (interactive (list (file-truename (car (find-file-read-args "File to save in:" nil)))))
    (let* ((screenshot-dir (file-name-directory file-name)))
      (make-directory screenshot-dir t)
      (if (functionp org-download-screenshot-method)
          (funcall org-download-screenshot-method
                   file-name)
        (shell-command-to-string
         (format org-download-screenshot-method
                 file-name)))
      (when (file-exists-p file-name)
        (kill-new file-name)))))
 
  
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
  
(when (gethash 'use-pdf-tools configurations t)
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
          (save-buffer)
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

            (unless (org-entry-get pom "KEY_ORDER")
              (let ((idx -1)
                    bibtex-keys)
                (with-current-buffer (find-file-noselect (car bibtex-completion-bibliography))
                  (bibtex-map-entries
                   (lambda (key _ _)
                     (setq idx (1+ idx)))))
                (org-entry-put pom "KEY_ORDER" (format "%s" (1+ idx)))))

            (when (and full-path (file-exists-p full-path))
              (let ((text-file-name (expand-file-name (format "%s.txt" (file-name-base full-path)) (file-name-directory full-path))))
                (unless (and (file-exists-p text-file-name) (org-entry-get pom "PDF_TEXT_FILE"))
                  (condition-case nil
                      (progn
                        (with-temp-buffer
                          (insert (amsha/pdf-to-text full-path))
                          (write-file text-file-name))
                        (org-entry-put pom "PDF_TEXT_FILE" (amsha/rename-full-path text-file-name)))
                    (error (message "Error: failed to read pdf file: %s" full-path)
                           (setq tags (append tags '("PDF_ERROR")))))
                  (push 'txt-file changes))))
            (when changes
              (save-buffer))
            (setq tags
                  ;; If error, most likely node not created
		  (if (ignore-errors (remove okm-research-papers-id (okm-get-parents)))
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

(defvar okm-org-agenda-copy-research-papers-directory nil)

(defun org-agenda-okm-copy-research-papers ()
  "Copy research papers from org-ql buffer to a directory."
  (unless okm-org-agenda-copy-research-papers-directory
    (setq okm-org-agenda-copy-research-papers-directory
          (file-truename (expand-file-name (secure-hash 'md5 (format "%s" (current-time))) "~/Downloads"))
          org-agenda-bulk-action-post-execution-function
          (lambda ()
            (em (format "Copied files to %s" okm-org-agenda-copy-research-papers-directory))
            ;; ranger looks for a file, not a directory
            (ranger (expand-file-name "something.pdf" okm-org-agenda-copy-research-papers-directory))
            (setq okm-org-agenda-copy-research-papers-directory nil)))
    (condition-case nil
	(make-directory okm-org-agenda-copy-research-papers-directory)
      (file-already-exists
       (progn
	 (message "Deleting directory and creating anew: %s" out-dir) 
	 (delete-directory out-dir t)
	 (make-directory out-dir)))))
  (org-with-point-at (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error))
    (when-let ((file-path (org-entry-get (point) "INTERLEAVE_PDF")))
      (message "Copied %s" (file-name-nondirectory file-path))
      (copy-file file-path
        	 (expand-file-name (file-name-nondirectory file-path)
        			   okm-org-agenda-copy-research-papers-directory)))))


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

(defun org-agenda-okm-copy-query-notes-and-bib ()
  "To be used with the org-agenda-bulk-action."
  (unless org-agenda-copy-query-notes-and-bib-func
    (setq org-agenda-copy-query-notes-and-bib-func (copy-notes-and-bib-function))
    (setq org-agenda-bulk-action-post-execution-function (lambda ()
                                                           (setq org-agenda-copy-query-notes-and-bib-func nil)
                                                           (copy-notes-and-bib-function-switch-to-buffers))))
  (org-with-point-at (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error))
    (funcall org-agenda-copy-query-notes-and-bib-func)))

(defun okm-insert-reading-list-entries ()
  "To be used with reading lists."
  (let* ((nodes-of-interest (save-excursion
                              (goto-char 0)
                              (org-next-visible-heading 1)
                              (let ((beg (progn (org-roam-end-of-meta-data t)
                                                (point)))
                                    (end (progn (org-previous-visible-heading 1)
                                                (org-end-of-subtree)
                                                (point))))
                                (--filter (not (string-empty-p it))
                                          (--map (cadr it)
                                                 (s-match-strings-all "id:\\([a-z0-9-]*\\)"  (buffer-substring-no-properties beg end)))))))
         (nodes (-uniq (-flatten (--map (okm-get-children it) nodes-of-interest))))
         (nodes-in-file (-non-nil (org-map-entries (lambda () (org-entry-get (point) "NODE_ID")) "LEVEL=1")))
         (new-nodes (-difference nodes nodes-in-file)))
    ;; (list nodes nodes-in-file))
    (when new-nodes
      (goto-char (point-max))
      (save-excursion
        (dolist (node new-nodes)
          (goto-char (point-max))
          (org-insert-todo-heading t)
          (insert (org-roam-node-title (org-roam-node-from-id node)))
          (org-entry-put (point) "NODE_ID" node)
          (goto-char (point-max))
          (insert (format "#+transclude: [[id:%s]] :only-contents :exclude-elements \"drawer keyword\"\ncite:&%s\n"
                          node
                          (car (org-roam-node-refs (org-roam-node-from-id node))))))))))

(defun okm-roam-buffer-from-reading-list ()
  "Run a roam query on the readinglist genreated."
  (interactive)
  (org-roam-ql-search (list [(in id $v1)]
                            (apply #'vector
                                   (org-ql-select
                                     (buffer-name)
                                     (read (read-string "org-ql query: "))
                                     :action (lambda ()
                                               (org-entry-get (point) "NODE_ID")))))
                      'org-roam))

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
	   (search-point (word-search-forward arxiv-number nil t))
           (bibtex-completion-bibliography (list (buffer-file-name)))
           keys)
      (if search-point
	  (progn
	    (goto-char search-point)
	    (message "%s already exists in the database" arxiv-number))
	(goto-char (point-max))
	(when (not (looking-at "^")) (insert "\n\n"))
	(insert (arxiv-get-bibtex-entry-via-arxiv-api arxiv-number) "\n\n")
        (save-buffer)
	(org-ref-clean-bibtex-entry)
        (save-buffer)
	(setq keys (progn
		     (bibtex-beginning-of-entry)
		     (list (cdr (assoc "=key=" (bibtex-parse-entry))))))
	(goto-char (point-max))
	(when (not (looking-at "^")) (insert "\n"))
	(save-buffer)
	(save-excursion
	  (bibtex-completion-edit-notes keys)
          (save-buffer)
	  (goto-char (point-min))
	  (org-set-property "LINK" arxiv-link)
	  (research-papers-configure 'force-file))))))

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
  (--map (org-roam-node-id (org-roam-backlink-target-node it)) (okm-links-get (org-roam-node-from-id entry-id) t :unique t)))

(defun okm-get-children (&optional entry-id)
  "Get the child IDs for entry with id entry-id or in the current entry.
Parent-child relation is defined by the brain-parent links."
  (unless entry-id
    (setq entry-id (org-id-get-closest)))
  (cl-assert entry-id nil "entry-id cannot be nil/not under a valid entry.")
  (--map (org-roam-node-id (org-roam-backlink-source-node it)) (okm-links-get (org-roam-node-from-id entry-id) nil :unique t)))

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

(defun okm-remove-parent-topic (&optional entry-id)
  "Prompt and remove a parent for entry at point or entry with id ENTRY-ID"
  (interactive)
  (unless entry-id
    (setq entry-id (org-id-get-closest)))
  (cl-assert entry-id nil "entry-id cannot be nil/not under a valid entry.")
  (let* ((collection (--remove (string-empty-p (car it))
                               (--map (cons (org-roam-node-title it) (org-roam-node-id it))
                                      (-map #'org-roam-node-from-id (okm-get-parents entry-id)))))
         (remove-entry (cdr (assoc (completing-read "Parent to remove: " collection nil t)
                            collection))))
    (apply #'org-entry-put-multivalued-property (point) okm-parent-property-name
           (--remove (s-contains-p remove-entry it)
                     (org-entry-get-multivalued-property
                      (org-roam-node-point (org-roam-node-from-id entry-id)) okm-parent-property-name)))))

(defvar org-agenda-okm-add-parents--parents nil)

(defun org-agenda-okm-add-parents ()
  "To be used with the org-agenda-bulk-action."
  (unless org-agenda-okm-add-parents--parents
    (setq org-agenda-okm-add-parents--parents (-map #'org-roam-node-id (org-roam-node-read-multiple "Add parents: "))
          org-agenda-bulk-action-post-execution-function (lambda ()
                                                           (em "Cleared org-agenda-okm-add-parents--parents")
                                                           (setq org-agenda-okm-add-parents--parents nil))))
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
              (if entry
                  (if (f-equal-p (file-truename (org-roam-node-file entry)) research-topics-file)
                      (push (org-roam-node-title entry) topics)
                    (push (org-roam-node-title entry) other-parents))
                (push "<missing entry>" other-parents)))
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
    (delete-interleve-entry)))

(defun delete-interleve-entry ()
  "Delete the interleave entries"
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
    ;; NOTE: This is done to avoid the resulting tags being nil triggering an error
    (apply #'org-entry-put-multivalued-property
           (point) "RPC-TAGS"
           (delete "PDF_ERROR"
                   (delete "nosiblings"
                           (delete "ATTACH"
                                   (delete-dups (org-entry-get-multivalued-property (point) "RPC-TAGS"))))))))

(defun okm-delete-node-and-files ()
  "Deletes this note and the pdf/txt files associated with it."
  (interactive)
  (when (y-or-n-p "Sure you want to delete the entry and it's associated pdf file? ")
    (delete-interleve-entry)
    (save-excursion
      (delete-file (buffer-file-name))
      (kill-buffer))))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map "\C-c!" 'org-time-stamp-inactive)
	    (define-key org-mode-map "\C-coo" 'org-noter)
	    (define-key org-mode-map "\C-cop" 'okm-add-parent-topic)
	    (define-key org-mode-map "\C-coc" 'research-papers-configure)
            (define-key org-mode-map "\C-cos" 'okm-print-parents)
            (define-key org-mode-map "\C-cor" 'org-ref-citation-hydra/body)
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

(defun update-org-agenda-files ()
  "Updates the org-agenda-files"
  (interactive)
  (setq org-agenda-files (flatten-tree (list (--map (f-files it (lambda (f)
                                                                  (and (f-ext-p f "org")
                                                                       (with-temp-buffer
                                                                         (insert-file f)
                                                                         (org-mode)
                                                                         (if-let (kwds (org-collect-keywords '("filetags")))
                                                                             (not (member "agendauntrack" (split-string (cadar kwds) ":" 'omit-nulls)))
                                                                           t)))))
                                                    (f-glob "~/Documents/org/brain/*/project_boards"))
                                             (f-files "~/Documents/org/brain/roam-notes"
                                                      (lambda (f)
                                                        (and (f-ext-p f "org")
                                                             (with-temp-buffer
                                                               (insert-file f)
                                                               (org-mode)
                                                               (when-let (kwds (org-collect-keywords '("filetags")))
                                                                 (member "agendatrack" (split-string (cadar kwds) ":" 'omit-nulls)))))))))))


(defun okm-org-agenda-recompute ()
  "Recompute the agenda list through the org-roam db."
  (interactive)
  (setq org-agenda-files (flatten-tree (list (--map (f-files it (lambda (f)
                                                                  (and (f-ext-p f "org")
                                                                       (with-temp-buffer
                                                                         (insert-file f)
                                                                         (org-mode)
                                                                         (if-let (kwds (org-collect-keywords '("filetags")))
                                                                             (not (member "agendauntrack" (split-string (cadar kwds) ":" 'omit-nulls)))
                                                                           t)))))
                                                    (f-glob "~/Documents/org/brain/*/project_boards"))
                                             (f-files "~/Documents/org/brain/roam-notes"
                                                      (lambda (f)
                                                        (and (f-ext-p f "org")
                                                             (with-temp-buffer
                                                               (insert-file f)
                                                               (org-mode)
                                                               (when-let (kwds (org-collect-keywords '("filetags")))
                                                                 (member "agendatrack" (split-string (cadar kwds) ":" 'omit-nulls)))))))
                                             (f-glob "~/Documents/org/brain/personal/**/*.org")))))

(okm-org-agenda-recompute)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(update-org-agenda-files)
(setq org-log-done 'note
      org-log-into-drawer t
      org-deadline-warning-days 2
      org-startup-indented t
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
      org-tag-alist '(("TEMP_BIB"))
      org-export-with-broken-links t)

(provide 'orgZ)
;;; orgZ.el ends here
