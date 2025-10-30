;;; orgZ.el --- a simple package                     -*- lexical-binding: t; -*-

;;; Commentary:
;;my custom org setup
;;; Code:
(require 'org)
(require 'org-capture)
(require 'org-tempo)
(require 'org-ref-arxiv)
(require 'ox-extra)
;; (require 'org-capture-pop-frame)
;(ido-mode)

(defvar okm-base-directory (file-truename "~/Documents/org/brain") "org knowladge management base direcory.")
(defvar okm-research-papers-id "34854c23-cf0a-40ba-b0c6-c9e5b3bb3030" "The id of the research_papers file.") ;; research_papers id TODO: think of a better way to do this?
(defvar okm-parent-property-name "BRAIN_PARENTS" "Property name containing parent ids.")
(defvar okm-parent-id-type-name "brain-parent" "ID type name used to refer to parent.")

(require 'org-roam-extensions)

(org-link-set-parameters okm-parent-id-type-name
                         :follow 'org-roam-id-open
                         :help-echo (lambda (_win _obj pos) (format "Link: %s" (org-roam-node-title
                                                                                (org-roam-node-from-id
                                                                                 (substring
                                                                                  (cadr (get-text-property (point) 'htmlize-link))
                                                                                  (1+ (length okm-parent-id-type-name))))))))

(with-eval-after-load 'magit
  (magit-sync-repo "org" "~/Documents/org" git-message ("brain/research_papers"
                                                        "brain/roam-notes"
                                                        "brain/personal/work"
                                                        "brain/personal/notes"
                                                        "brain/work/figures"
                                                        "brain/work/notes"
                                                        "brain/work/project_boards")))

(ox-extras-activate '(ignore-headlines))

(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-startup-folded 'content
      org-id-track-globally t
      org-id-locations-file "~/.emacs.d/.org-id-locations"
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id

      org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p!/@)" "WAIT(w@/!)" "IDEA(i)" "|" "DONE(d!)" "CANCELED(c@)" "LATER(l@)")
			  (sequence "ROUNTINE(R)" "|" "ROUNTINE_COMPLETE(r@)" ))

      org-babel-load-languages (append org-babel-load-languages '((ruby . t)
				                                  (plantuml . t)
				                                  (emacs-lisp . t)
				                                  (python . t)
                                                                  (R . t)
				                                  (shell . t)))

      org-confirm-babel-evaluate nil;;'my-org-confirm-babel-evaluate)
      org-latex-image-default-width ""
      org-startup-with-inline-images t
      org-tag-persistent-alist '(("@work" . ?w) ("@home" . ?h) ("@mobile" . ?m))
      org-default-notes-file "~/Documents/org/notes.org"
      org-log-done 'note
      org-log-into-drawer t
      org-deadline-warning-days 2
      org-startup-indented t
      org-clock-idle-time 10
      org-return-follows-link t
      org-refile-use-outline-path "file"
      org-outline-path-complete-in-steps t
      org-completion-use-ido t
      org-attach-directory "~/Documents/org/documents/"
      org-clock-continuously t
      org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d"
      org-image-actual-width (list 650)
      org-tag-alist '(("TEMP_BIB"))
      org-export-with-broken-links t
      org-agenda-persistent-marks t
      org-agenda-files (when (f-exists-p "~/.emacs.d/org-agenda-org-roam-ql-cache")
                         (string-split
                          (with-temp-buffer
                            (insert-file "~/.emacs.d/org-agenda-org-roam-ql-cache")
                            (buffer-string))
                          "\n"))
      org-refile-targets '((org-agenda-files :maxlevel . 6))
                                        ;(org-c-refile-targets :maxlevel . 6)))
      org-export-allow-bind-keywords t
      org-latex-image-default-option "scale=0.6"
      org-capture-templates
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
	;; ("jg" "Journal entry general"
	;;  entry (file+olp+datetree "~/Documents/org/journal.org")
	;;  "* %?")
	("jw" "Journal entry work"
	 entry (file+olp+datetree "~/Documents/org/brain/work/project_boards/day-to-day.org")
	 "* TODO %?")
	("js" "Journal entry work-scrum"
	 entry (file+olp+datetree "~/Documents/org/brain/work/scrum.org")
	 "* Y:\n1. %?\n* T:\n1. "
	 :jump-to-captured t)
	;; ("jt" "Journal sub entry"
	;;  entry (file+olp+datetree "~/Documents/org/brain/work/notes.org")
	;;  "1. %?")
	("e" "Experiment setup information")
	("ej" "Add Journal entry")
        ;; ("ejt" "for task"
	;;  entry (file+olp+datetree "~/Documents/org/brain/work/notes.org")
	;;  "%(okm-board-task-location)")
        ;;       ("eje" "for experiment"
        ;;        entry (file+olp+datetree "~/Documents/org/brain/work/notes.org")
        ;;        "* [[file:experiments_log.org::#%^{EXP_ID}][%\\1]] %? :e%\\1:")
        ;;       ("el" "Add experiment"
        ;;        entry (file "~/Documents/org/brain/work/experiments_log.org")
        ;;        "\n\n* TODO <<%^{ID}>> %^{Experiment} [%] :@work:exp_%^{Project_id}:\n  :PROPERTIES:
        ;; :CUSTOM_ID:       %\\1
        ;; :PROJECT: [[file:projects.org::#%\\3][%\\3]]
        ;; :PROJECT_ID: %\\3
        ;; :SPRINT: %^{Sprint ID}
        ;; :END:\n- %^{Description}\n\n** Notes\n\n** TODO %?\n** TODO Conclusions"
        ;;        :jump-to-captured t)
        ;; 	("es" "Add sprint"
        ;; 	 entry (file+function "~/Documents/org/brain/work/projects.org" org-ask-title-location)
        ;; 	 "** TODO Sprint %(okm--org-templates-get-sprint-id): %^{TITLE}
        ;;    :PROPERTIES:
        ;;    :EXPORT_TOC: nil
        ;;    :EXPORT_TITLE: %\\1
        ;;    :EXPORT_OPTIONS: H:2
        ;;    :EXPORT_AUTHOR:
        ;;    :START_DATE: %u
        ;;    :END_DATE:
        ;;    :ID:       %(org-id-new)
        ;;    :END:
        ;; *** From previous:
        ;;     - %?
        ;; *** Sprint goal:
        ;; *** Related experiments:
        ;; *** Remarks:
        ;; " :jump-to-captured t)
        ;; 	("ep" "Add project"
        ;; 	 entry (file "~/Documents/org/brain/work/projects.org")
        ;; 	 "* TODO <<%(okm--org-templates-get-project-id)>> %^{TITLE}
        ;;   :PROPERTIES:
        ;;   :CUSTOM_ID: %(okm--org-templates-get-project-id)
        ;;   :ID:       %(org-id-new)
        ;;   :END:
        ;; ** Related repos:
        ;; ** %\\1 literature
        ;;    :PROPERTIES:
        ;;    :ID:       %(org-id-new)
        ;;    :END:
        ;; %?
        ;; "
        ;; 	 :jump-to-captured t)
        ("ep" "Add project board"
         plain (function okm-add-new-project-board)
         "%?"
         :after-finalize okm-add-new-project-finalize
	 :jump-to-captured t)
        ("et" "Add task"
	 entry (function okm-goto-task-board)
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

(repeatize 'org-babel-map)

;; use package defs *************************************************************************
(use-package org-roam-gocal
  :straight nil
  :config
  (setq org-roam-gocal-new-node-file (f-expand "google_calender_unlisted.org" okm-base-directory)))

(use-package org-protocol
  :ensure nil
  :straight nil
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("add doi link"
                 :protocol "add-doi-pdf"
                 :function add-doi-and-pdf
                 :kill-client t))

  (defvar previous-add-doi-and-pdf-data nil)

  (defun okm-retry-last-add-doi-and-pdf ()
    (interactive)
    (if previous-add-doi-and-pdf-data
        (add-doi-and-pdf previous-add-doi-and-pdf-data t)
      (user-error "No previous data")))

  (defun add-doi-and-pdf (data &optional no-save-data)
    "DATA exepcts to be an alist with keys :url and :filename."
    (message "Trying to add: %s" data)
    (unless no-save-data 
      (setq previous-add-doi-and-pdf-data data))
    (let* ((url (s-replace-regexp "\\(https:/\\)[^/]" "https://" (plist-get data :url) nil nil 1))
           (file-name-entry (plist-get data :filename))
           (file-name (when file-name-entry
                        (format "file:///%s" (expand-file-name (plist-get data :filename)))))
           (doi (plist-get data :doi))
           (bibtex-entry-format (remove 'required-fields bibtex-entry-format)))
      (save-match-data 
        (cond
         ((or doi
              (and (string-match "\\(10\\.[0-9]\\{4\\}\\(/\\|%2F\\)\\([a-z]\\|[0-9]\\|_\\|-\\|\\.\\)+\\)" url)
                   (setq doi (s-replace-regexp
                              "\\.$" ""
                              (s-replace-regexp
                               "\\.pdf$" ""
                               (s-replace "%2F" "/" (match-string 1 url)))))))
          (progn
            (save-excursion
              (with-current-buffer (find-file-noselect (car bibtex-completion-bibliography))
                (goto-char (point-max))
                (when (not (looking-at "^")) (insert "\n\n")))
              (doi-add-bibtex-entry doi (car bibtex-completion-bibliography))
              (doi-utils-open-bibtex doi)
              (org-ref-open-bibtex-notes)
              ;; make sure at the top most level
              (while (not (>= 1 (org-outline-level)))
                (org-up-element))
              ;; add link if not already set
              (-if-let* ((_ file-name)
                         (link (org-entry-get (point) "LINK"))
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

(use-package orglink
  :config
  (add-to-list 'orglink-activate-in-modes 'prog-mode)
  (global-orglink-mode))

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
                          :color "#777c80" :style nil)))))

  :config
  (defun amsha/custom-time-only-highlight (oldfn)
    (append
     (funcall oldfn)
     '(("\\[\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\(?::[0-9]\\{2\\}\\)? \\(?:AM\\|PM\\)\\)\\]" 0 'org-modern-time-inactive t))))

  (advice-add 'org-modern--make-font-lock-keywords :around #'amsha/custom-time-only-highlight))

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
  (setf (alist-get 'org-mode bibtex-completion-format-citation-functions) (lambda (keys) (s-join "," (--map (format "cite:%s" it) keys))))

  (defun amsha/bibtex-completion-apa-get-value (old-fun &rest args)
    "Override the downcase situation."
    ;; (if (and (stringp (car args)) (string-equal (car args) "title"))
    ;;     (if-let (value (bibtex-completion-get-value (car args) entry))
    ;;         (replace-regexp-in-string ; remove braces
    ;;          "[{}]"
    ;;          ""
    ;;          (replace-regexp-in-string ; remove macros
    ;;           "\\\\[[:alpha:]]+{"
    ;;           ""
    ;;           (replace-regexp-in-string ; upcase initial letter
    ;;            "^[[:alpha:]]"
    ;;            'upcase
    ;;            (replace-regexp-in-string ; preserve stuff in braces from being downcased
    ;;             "\\(^[^{]*{\\)\\|\\(}[^{]*{\\)\\|\\(}.*$\\)\\|\\(^[^{}]*$\\)"
    ;;             (lambda (x) (s-replace "\\" "\\\\" x))
    ;;             value))))
    ;;       (apply old-fun args))
    ;;   (apply old-fun args)))
    (let* ((field (car args))
           (entry (cadr args))
           (value (bibtex-completion-get-value field entry)))
      (cond
       ((and (stringp value) (string= "title" field))
        (replace-regexp-in-string ; remove braces
         "[{}]"
         ""
         (replace-regexp-in-string ; remove macros
          "\\\\[[:alpha:]]+{"
          ""
          (replace-regexp-in-string ; upcase initial letter
           "^[[:alpha:]]"
           'upcase
           (replace-regexp-in-string ; preserve stuff in braces from being downcased
            "\\(^[^{]*{\\)\\|\\(}[^{]*{\\)\\|\\(}.*$\\)\\|\\(^[^{}]*$\\)"
            (lambda (x) (s-replace "\\" "\\\\" x))
            value)))))
       ((and (stringp value) (string= "pages" field))
        (s-join "–" (s-split "[^0-9:]+" value t)))
       (t (em (apply old-fun args))))))

  (advice-add 'bibtex-completion-apa-get-value :around #'amsha/bibtex-completion-apa-get-value)

  ;; Handle the number of authors being changed to ... to 20
  (defun amsha/bibtex-completion-apa-format-authors (value &optional abbrev)
    "Format author list in VALUE in APA style.
When ABBREV is non-nil, format in abbreviated APA style instead."
    (cl-loop for a in (s-split " and " value t)
             if (s-index-of "{" a)
             collect
             (replace-regexp-in-string "[{}]" "" a)
             into authors
             else if (s-index-of "," a)
             collect
             (let ((p (s-split " *, *" a t)))
               (concat
                (car p) ", "
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (s-split " " (cadr p))))))
             into authors
             else
             collect
             (let ((p (s-split " " a t)))
               (concat
                (-last-item p) ", "
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (-butlast p)))))
             into authors
             finally return
             (let ((l (length authors)))
               (cond
                ((= l 1) (car authors))
                ((and abbrev (= l 2))
                 (concat (s-join " & " authors)))
                (abbrev
                 (format "%s et al." (car authors)))
                ;; This is originally 8 not made 21
                ((< l 20) (concat (s-join ", " (-butlast authors))
                                 ", & " (-last-item authors)))
                ;; This is originally 7 not made 20
                (t (concat (s-join ", " (-slice authors 0 20)) ", …"))))))

  (advice-add 'bibtex-completion-apa-format-authors :override #'amsha/bibtex-completion-apa-format-authors))

(use-package emacsql
  :straight (emacsql :includes (emacsql-sqlite)
                     :files (:defaults "*.el"))
  :ensure t)

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
         ("C-c ]" . org-ref-insert-link)
         :map bibtex-mode-map
         ("C-c o o" . org-ref-open-bibtex-notes))
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

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))
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

  (advice-add 'org-noter-pdf--get-selected-text :filter-return #'org-noter-pdf--get-selected-text-single-linified)

  (defun org-noter-append-title-at-point-to-highlight ()
    "Take the org heading title at point and append it to the annotation.

If an annotation is at the point, add content to it.
Else create a text annotations at point."
    (interactive)
    ;; Copied fron `org-noter-sync-current-note'
    (org-noter--with-selected-notes-window
     "No notes window exists"
     (if (string= (org-noter--get-or-read-document-property t)
                  (org-noter--session-property-text session))
         (let ((location (org-noter--parse-location-property (org-noter--get-containing-element)))
               (content-to-add (org-get-heading t t t t))
               (highlight-coords (when-let (highlight-data
                                             (org-entry-get (point) "HIGHLIGHT"))
                                   (car (pdf-highlight-coords
                                    (eval ;; FIXME: Should I be worried about saftey here?
                                     (car (read-from-string
                                           highlight-data))))))))
           (if location
               ;; Copied from `org-noter--doc-goto-location'
               (org-noter--with-valid-session
                (let ((window (org-noter--get-doc-window))
                      (mode (org-noter--session-doc-mode session)))
                  (with-selected-window window
                    (when (memq mode '(doc-view-mode pdf-view-mode))
                      (if (eq mode 'doc-view-mode)
                          (doc-view-goto-page (org-noter--get-location-page location))
                        (pdf-view-goto-page (org-noter--get-location-page location)))
                      ;; Parts copied from `org-noter-pdf--show-arrow' and related
                      (let* ((top (or (and highlight-coords
                                           (nth 1 highlight-coords))
                                      (org-noter--get-location-top location)))
                             (left (or (and highlight-coords
                                            (nth 0 highlight-coords))
                                       (org-noter--get-location-left location)))
                             (org-noter--arrow-location
                              (vector 0 0 top left))
                             (image-top  (if (floatp top)
                                             (round (* top  (cdr (pdf-view-image-size)))))) ; pixel location on page (magnification-dependent)
                             ;; Adding an offset of 2 to make the annotations is picked up correct
                             (image-left (+ 2
                                            (if (floatp left)
                                                (floor (* left (car (pdf-view-image-size)))))))
                             (a (ignore-errors (pdf-annot-at-position (cons image-left image-top))))
                             (current-contents (if a (pdf-annot-get a 'contents) "")))
                        ;; FIXME: is the delay needed here?
                        (org-noter-pdf--show-arrow)
                        (when (y-or-n-p (if a
                                            "Append title to annotation?"
                                          "Create annotation and append title to annotation"))
                          (unless a
                            (setq a (pdf-annot-add-text-annotation (cons (- image-left 2) image-top))))
                          (pdf-annot-put a
                              'contents
                            (concat
                             (unless (string-empty-p current-contents)
                               (format "%s\n\n" current-contents))
                             content-to-add)))))
                    (redisplay))))
             (user-error "No note selected")))
       (user-error "You are inside a different document"))))

  (define-key org-noter-notes-mode-map (kbd "C-M->") 'org-noter-append-title-at-point-to-highlight))

(use-package org-super-agenda
  :custom
  (org-super-agenda-groups '((:name "dates" :auto-ts t))))

(use-package org-ql
  :straight (org-ql :type git :host github :repo "alphapapa/org-ql")
  :bind (:map org-agenda-mode-map
              ("C-c o s" . org-ql-view-topics)
              ("C-c o o" . org-ql-view-noter)
              ("C-c o p" . org-ql-add-parents))
  :commands (org-ql-defpred okm-query-boards)
  :config
  ;; (setq org-super-agenda-header-map
  ;;       (let ((map (make-sparse-keymap)))
  ;;         (set-keymap-parent map org-agenda-mode-map)
  ;;         map))
  (setq org-super-agenda-header-map (make-sparse-keymap))

  (org-agenda-action org-ql-view-noter
    (org-noter))
  (org-agenda-action org-ql-view-topics
    (okm-print-parents))
  (org-agenda-action org-ql-add-parents
    (okm-add-parent-topic))

  (defun okm-query-boards ()
    "List the in progress items in the project boards directory.
Either show all or filter based on a sprint."
    (interactive)
    (let* ((files (f-glob "*/project_boards/*.org" okm-base-directory))
           (selection-list (append '(("ALL"))
                                   ;; (amsha/get-sprints '("INPROGRESS" "TODO"))
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

  (require 'org-ql-view)
  (add-to-list 'org-ql-views
               (cons "okm: day-to-day active tasks"
                     (list :buffers-files "~/Documents/org/brain/work/project_boards/day-to-day.org"
                           :query '(todo "TODO" "INPROGRESS")
                           :sort '(date priority)
                           :title "okm: day-to-day active tasks")))
  (add-to-list 'org-ql-views
               (cons "okm: project boards query"
                     #'okm-query-boards)))


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
                   (or (org-id-get-closest)
                       (progn
                         (goto-char 0)
                         (org-id-get)))))
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

;; Functions ********************************************************************************

;; from https://emacs.stackexchange.com/questions/44664/apply-ansi-color-escape-sequences-for-org-babel-results
(defun org-babel-ansi-color-result ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

;; from https://emacs.stackexchange.com/questions/42471/how-to-export-markdown-from-org-mode-with-syntax
(defun org-md-example-block-with-syntax (example-block _content info)
  "Transcode element EXAMPLE-BLOCK as ```lang ...'''."
  (format "```%s\n%s\n```"
          (org-element-property :language example-block)
          (org-remove-indentation
           (org-export-format-code-default example-block info))))

(defun amsha/reload-org-babel-langs ()
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

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

(defun okm-goto-task-board ()
  "Move the cursor to a location in a task board."
  (interactive)
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
  (let* ((ts (format-time-string "%I:%M %p" (current-time)))
         (targets
          (append
           '(("--None--"))
           (--map
            (let* ((title (s-replace-regexp " \\[[0-9]+/[0-9]+\\]" "" (org-roam-node-title it)))
                   (full-file-name (org-roam-node-file it))
                   (file-name (format "%s/%s" (f-base (f-parent (f-parent full-file-name))) (file-name-base full-file-name)))
                   (todo-state (or (org-roam-node-todo it) "")))
              (list (format "%-10s  %-30s %s"
                            (propertize todo-state 'face (org-get-todo-face todo-state))
                            (propertize file-name 'face 'marginalia-documentation)
                            title)
                    title
                    (org-roam-node-id it)))
            (org-roam-ql-nodes '([(and (= level 1) (like file $s1))] "%project_boards%")))))
         (target (progn
                   (assoc (completing-read "Select task: " targets nil t) targets))))
    (if (cdr target)
        (format "**** [%s] [[id:%s][%s]]  %%?"
                ts
                (nth 2 target)
                (nth 1 target))
      (format "**** [%s] %%?" ts))))

(defun okm-insert-timestamp ()
  (interactive)
  (insert "[" (format-time-string "%I:%M %p" (current-time)) "]"))
    
(defun okm-add-repository ()
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

(defun okm-add-new-project-board ()
  (let* ((title (read-string "title: "))
         (new-file (file-truename
                    (format
                     "~/Documents/org/brain/work/project_boards/%s.org"
                     (read-string "board name: " (s-replace " " "_" (downcase title)))))))
    (if (file-exists-p new-file)
        (user-error "File exists %s" (em new-file "boooooooooo"))
      (org-capture-put :new-buffer t)
      (find-file new-file)
      (setq org-capture-plist
            (plist-put org-capture-plist
                       :amsha-file-created
                       new-file))
      (goto-char (point-min))
      (org-id-get-create)
      (goto-char (point-max))
      (insert (format "\n#+title: %s\n
* Meta data
** Related repos:
** %s literature" title title))
      (org-id-get-create)
      (goto-char (point-max)))))

(defun okm-add-new-project-finalize ()
  (when org-note-abort
    (when-let* ((new-file (plist-get org-capture-plist :amsha-file-created))
                (_ (yes-or-no-p "Delete file for aborted capture?")))
      (when (find-buffer-visiting new-file)
        (kill-buffer (find-buffer-visiting new-file)))
      (delete-file new-file))))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let* ((n-inprogress (-sum
                        (org-map-entries
                         (lambda ()
                           (if (string= (org-get-todo-state) "INPROGRESS")
                               1
                             0))
                         (format "LEVEL=%s" (1+ (org-outline-level)))
                         'tree)))
         (target-state (cond
                        ((= n-not-done 0) "DONE")
                        ((or (> n-done 0) n-inprogress) "INPROGRESS")
                        (t "TODO"))))
    (unless (string= (org-get-todo-state) target-state)
      (org-todo target-state))))


(defun doi-add-bibtex-entry-with-note (doi)
  "."
  (interactive
   (list (read-string
          "DOI: "
          ;; now set initial input
          (doi-utils-maybe-doi-from-region-or-current-kill))))
  (doi-utils-add-bibtex-entry-from-doi doi)
  (find-file (car bibtex-completion-bibliography))
  (org-ref-open-bibtex-notes)
  (org-set-property "LINK" (completing-read "LINK: " nil nil nil (when (s-starts-with-p "file://" (car kill-ring))
                                                                   (car kill-ring))))
  (research-papers-configure t))



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
                              "--max-columns=10000"
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
  (let* ((results (--filter
                   (cdr it)
                   (-map (lambda (f)
                           (cons (f-base f)
                                 (okm--test-regexp-on-file f regexp)))
                         (f-glob "*.txt" bibtex-completion-library-path))))
         (nodes (--map (cons
                        (org-roam-node-id it)
                        (assoc (f-base (org-roam-node-file it)) results))
                       (bibtex-keys-to-nodes (map-keys results))))
         (temp-preview-function 
          (lambda (node _)
            (propertize (s-join "\n" (--map (format " - %s" it) (assoc (org-roam-node-id node) nodes))) 'face 'org-tag))))
  
    (org-roam-ql-search
     `(pdf-string ,(format "%s" regexp))
     (prin1-to-string regexp)
     "title"
     temp-preview-function)))

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

;; (defun amsha/org-brain-children-topics (entry)
;;   "list parents of all the children of an ENTRY."
;;   (interactive (list (org-roam-node-read-multiple)))
;;   (let (topics other-parents)
;;     (mapcar (lambda (child-entry)
;;               (-let (((-topics . -other-parents) (okm-parents-by-topics (org-roam-node-id (org-roam-backlink-target-node child-entry)))))
;;                 (setq topics (append topics -topics)
;;                       other-parents (append other-parents -other-parents))))
;;             (org-roam-backlinks-get entry))
;;     (setq topics (-uniq topics)
;;           other-parents (-uniq other-parents))
;;     (okm-print-parents topics other-parents)
;;     (cons topics other-parents)))

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
                                          (f-files bibtex-completion-notes-path
                                                   (lambda (f)
                                                     (equal (f-ext f) "org")))
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
            (unless (or (org-entry-get pom "OPENAI_FILE_ID") (string-empty-p (org-entry-get pom "OPENAI_FILE_ID")))
              (when-let ((text-file-name (org-entry-get pom "PDF_TEXT_FILE")))
                (org-entry-put pom "OPENAI_FILE_ID"
                               (plist-get
                                (amsha/add-file-to-openai text-file-name)
                                :id))))
            (unless (or (org-entry-get pom "ABSTRACT") (string-empty-p (org-entry-get pom "ABSTRACT"))
                        (org-entry-get pom "SUMMARY") (string-empty-p (org-entry-get pom "SUMMARY")))
              (when (org-entry-get pom "PDF_TEXT_FILE")
                (okm-gptel-get-paper-abstract-summary)))

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

(defun amsha/curl (method &rest rest)
  (shell-command-to-string
   (string-join
    (append
     (list
      "curl"
      "--disable" "--location" "--silent"
      (if (memq system-type '(windows-nt ms-dos))
          "" "--compressed")
      "-X" method "-y300" "-Y1"
      ;; "-D-"
      ;; "--trace-ascii"
      )
     rest)
    " ")))

(defun amsha/add-file-to-openai (file)
  ""
  (let* ((api-key (gethash 'openai-apk configurations))
         (org-data-store-id (gethash 'openai-org-vector-store configurations))
         (json-response
          (amsha/curl "POST"
           (format "-H \"Authorization: Bearer %s\"" api-key)
           "-F purpose=\"assistants\""
           (format "-F file=\"@%s\"" (file-truename file))
           "https://api.openai.com/v1/files"))
         (json-data
          (json-read-from-string json-response))
         (json-object-type 'plist)
         (json-null :null))
    (with-temp-buffer
      (insert
       (amsha/curl "POST"
        (format "-H \"Authorization: Bearer %s\"" api-key)
        "-H \"Content-Type: application/json\""
        "-H \"OpenAI-Beta: assistants=v2\""
        "-d" (format "%S" (json-encode (list :file_id (cdr (assoc-string 'id json-data)))))
        (format "https://api.openai.com/v1/vector_stores/%s/files" org-data-store-id)))
      (goto-char 0)
      (json-read))))

;; (with-eval-after-load 'gptel-openai
(defun amsha/doi-utils-get-pdf-url-uml (old-function &rest rest)
  "Making sure the urls that are being recived by org-ref is made to use uml links."
  (let ((url (apply old-function rest)))
    (when url
      (amsha/get-uml-link url))))

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
            (dired okm-org-agenda-copy-research-papers-directory)
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
    (if-let ((file-path (org-entry-get (point) "INTERLEAVE_PDF")))
        (progn
          (message "Copied %s" (file-name-nondirectory file-path))
          (copy-file file-path
        	     (expand-file-name (file-name-nondirectory file-path)
        			       okm-org-agenda-copy-research-papers-directory)))
      (message "FAILED to copy %s" file-path))))


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
    (setq parents (--map (org-roam-node-id it) (list (org-roam-node-read)))))
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
    (setq org-agenda-okm-add-parents--parents (list (org-roam-node-id (org-roam-node-read "Add parents: ")))
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

(defun org-archive--compute-location-to-dir (computed-location)
  "See ORG-ARCHIVE--COMPUTE-LOCATION, makes the locations move to dir named archive."
  (cons (let* ((f (car computed-location))
               (f (f-join (f-dirname f) "archive" (f-filename f))))
          (unless (f-exists-p f)
            (f-mkdir (f-dirname f)))
          f)
        (cdr computed-location)))

(advice-add 'org-archive--compute-location :filter-return #'org-archive--compute-location-to-dir)

(defun okm-get-pdf-txt (paper-id)
  "Given the paper ID get the full text extracted using pdf-tools."
  (with-temp-buffer
    (insert-file-contents (file-truename (expand-file-name (format "%s.txt" paper-id) bibtex-completion-library-path)))
    (buffer-string)))


;; meant to be used as tool-call-function
(defun okm-gptel-get-paper-abstract-summary (&optional tool-callback custom-id)
  "Given the custom-id, get the abstract and summary as a single string.
Meant to be used for LLMs.
The tool-callback should take one value, the result.
If CUSTOM-ID is not provided, assume the point it at the corresponding node."
  (unless (featurep 'gptel)
    (require 'gptel))
  (unless (featurep 'org-roam-ql)
    (require 'org-roam-ql))
  (let ((node (and custom-id (car (org-roam-ql-nodes `(properties "Custom_ID" ,custom-id)))))
        (custom-id (or custom-id (org-entry-get (point) "Custom_ID")))
        (buf (current-buffer)))
    (if-let* ((abstract (or
                         (and node (alist-get "ABSTRACT" (org-roam-node-properties node) nil nil #'string-equal))
                         (org-entry-get (point) "ABSTRACT")))
              (summary (or
                        (and node (alist-get "SUMMARY" (org-roam-node-properties node) nil nil #'string-equal))
                        (org-entry-get (point) "SUMMARY"))))
        (if tool-callback
            (funcall tool-callback (format "* abstract\n%s\n* summary\n%s" abstract summary))
          (format "* abstract\n%s\n* summary\n%s" abstract summary))
      (let ((gptel-context--alist nil)
            (gptel-tools nil)
            (gptel-openai-responses--tools nil))
        (condition-case err
            (gptel-request (format "%s

The above is the text extracted from a paper using pdf-tools.
Extract the abstract from this. Also provide a summary of the paper which can be used as input for other LLMs.
The summary of the paper should contain the motivation and the gap the paper is addressing, how they address this,
and the main results and disucssions points, all in one paragraph. This should mainly highlight details that
are not clearly available in the abstract which could be useulf to better understand the paper.
The format of the response should be as follows:
* abstract
<abstract text>
* summary
<summary text>"
                                   (okm-get-pdf-txt custom-id))
              :callback (lambda (response info)
                          (cond
                           ((null response)
                            (em "ERROR" info)
                            (if tool-callback
                              (funcall tool-callback (format "Error %s" info))))
                           ((stringp response)
                            (let* ((res (s-split "* summary" response))
                                   (abstract (s-trim (s-replace "* abstract" "" (car res))))
                                   (summary (s-trim (cadr res)))
                                   (process-data
                                    (lambda ()
                                      (goto-char (point-min))
                                      (org-entry-put (point) "ABSTRACT" abstract)
                                      (org-entry-put (point) "SUMMARY" summary)
                                      (save-buffer)
                                      (em "for" custom-id "added abstract and summary" abstract summary))))
                              (if node
                                  (org-roam-with-file (org-roam-node-file node) nil
                                    (funcall process-data))
                                (with-current-buffer buf
                                  (funcall process-data))))
                            (if tool-callback
                                (funcall tool-callback response))))))
          (t (if tool-callback
                 (funcall tool-callback (format "Error occurred %s" err))
               (user-error (format "Error occurred %s" err)))))))))

(defun okm-org-agenda-recompute-org-roam-ql ()
  "Same as `okm-org-agenda-recompute', but uses org-roam-ql"
  (interactive)
  (unless (featurep 'org-roam-ql)
    (require 'org-roam-ql))
  (with-temp-buffer
    (erase-buffer)
    (insert
     (string-join
      (setq org-agenda-files
            ;; (org-roam-ql-nodes-files '(or (and (file "project_boards") (not (tags "agendauntrack")))
            ;;                               (and (file "roam-notes") (tags "agendatrack"))
            ;;                               (file "google_calender_unlisted")))))
            (org-roam-ql-nodes-files
             (list [:select nodes:id :from nodes
                            :left-join files :on (= nodes:file files:file)
                            :left-join tags :on (= nodes:id tags:node-id)
                            :where (or
                                    (and (like files:file $s1) (not-like files:file $s5) (like tags:tag $s2))
                                    (and (like files:file $s1) (not-like files:file $s5) (or (is tags:tag nil) (not-like tags:tag $s3)))
                                    (like files:file $s4))]
                   "%project_boards%" "agendatrack" "agendauntrack" "%google_calender_unlisted%" "%archive%")))
      "\n"))
    (write-region (point-min) (point-max)
                  (file-truename (expand-file-name "~/.emacs.d/org-agenda-org-roam-ql-cache")))))

(defun okm-summarize-project-board ()
  "A stripped down version of the board usable with LLMs."
  (interactive)
  (okm-goto-task-board)
  (goto-char (point-min))
  (--> (--map (concat (if (eq (org-element-property :level it) 1)
                          "#" "##")
                      " "
                      (org-element-property :todo-keyword it)
                      " "
                      (car (org-element-property :title it)))
              (org-ql-select (current-buffer) '(level <= 2)))
       (with-current-buffer (get-buffer-create (format "* %s summary*" (buffer-name)))
         (erase-buffer)
         (insert (string-join it "\n"))
         (markdown-mode)
         (display-buffer (current-buffer)))))

(defun amsha/org-repalce-link-in-string (str)
  "Replace the link in the string."
  (s-replace-regexp
   "\\[id:\\([a-z]\\|[0-9]\\)\\{8\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{12\\}\\]"
   ""
   str))

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
     "Copy url" :column "Copy")
    ("A" (save-window-excursion
	   (let ((bibtex-completion-bibliography (org-ref-find-bibliography))
	         (entry (bibtex-completion-get-entry (org-ref-get-bibtex-key-under-cursor))))
             (kill-new (format "%s , %s" (bibtex-completion-apa-get-value "author-abbrev" entry) (bibtex-completion-get-value "year" entry)))))
     "Copy APA short" :column "Copy")))


(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  
(when (gethash 'use-pdf-tools configurations t)
  (define-key pdf-view-mode-map [C-M-down-mouse-1] 'pdf-crop-image)
  (define-key pdf-view-mode-map [C-M-S-down-mouse-1] 'pdf-crop-image-and-save))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(add-hook 'org-babel-after-execute-hook 'org-babel-ansi-color-result)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map "\C-c!" 'org-time-stamp-inactive)
	    (define-key org-mode-map "\C-coo" 'org-noter)
	    (define-key org-mode-map "\C-cop" 'okm-add-parent-topic)
	    (define-key org-mode-map "\C-coc" 'research-papers-configure)
            (define-key org-mode-map "\C-cos" 'okm-print-parents)
            (define-key org-mode-map "\C-cor" 'org-ref-citation-hydra/body)
            ;; (define-key org-mode-map "\C-coa" 'org-asana-hydra/body)
            (define-key org-mode-map (kbd "C-'") nil)
            (define-key org-mode-map "\C-c/" nil)
	    (flyspell-mode t)))
(add-hook 'org-mode-hook 'visual-line-mode)

(advice-add 'org-md-example-block :override #'org-md-example-block-with-syntax)
(advice-add #'doi-utils-get-pdf-url :around #'amsha/doi-utils-get-pdf-url-uml)

(with-eval-after-load 'org
  (amsha/reload-org-babel-langs))

(provide 'orgZ)
;;; orgZ.el ends here
