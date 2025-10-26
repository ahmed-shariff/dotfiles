
;; (use-package org-capture-pop-frame
;;   :straight (org-capture-pop-frame :type git :host github :repo "tumashu/org-capture-pop-frame"
;;                                    :fork (:host github :repo "ahmed-shariff/org-capture-pop-frame"))
;;   :config
;;   (setf (alist-get 'width ocpf-frame-parameters) 170)
;;   (setf (alist-get 'height ocpf-frame-parameters) 50))


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

;; (use-package consult-notes
;;   :straight (:type git :host github :repo "mclear-tools/consult-notes")
;;   :bind (("C-c n n" . consult-notes-search-in-all-notes)
;;          ("C-c n N" . consult-ripgrep-roam-notes)
;;          ("C-c n v" . consult-notes-visit-relation))
;;   :commands (consult-notes
;;              consult-notes-search-in-all-notes
;;              consult-notes-org-roam-find-node
;;              consult-ripgrep-roam-notes
;;              consult-notes-org-roam-find-node-relation)
;;   :config
;;   (setq consult-notes-sources `(("Org"  ?o  ,okm-base-directory)) ;; Set notes dir(s), see below
;;         consult-notes-org-roam-template org-roam-node-display-template ;; To make sure I can use my marginalia approach.
;;         consult-notes-org-roam-annotate-function nil)
;;   (consult-notes-org-roam-mode) ;; Set org-roam integration

;; (defun okm-query-papers-by-topics (&optional topic-ids)
;;   "Query papers based on topics."
;;   (interactive)
;;   (let* ((topics (if topic-ids
;;                      (-map #'org-roam-node-from-id topic-ids)
;;                    (org-roam-node-read-multiple "Query topics: ")))
;;          (topic-queries (--map `(child-of ,(org-roam-node-title it)) topics)))
;;     (org-roam-ql-search
;;      `(and (file "research_papers")
;;            ,(if (eq 1 (length topic-queries))
;;                (car topic-queries)
;;               `(,(pcase (completing-read "connector: " '(and or) nil t)
;;                    ("or" 'or)
;;                    ("and" 'and))
;;             ,@topic-queries)))
;;      (format "(%s)" (s-join ", " (-map #'org-roam-node-title topics))))))

;; ;; TODO: allow mulitiple combinations of brain-parent to be used (eg: (and (or ..) (or ..)))
;; (defun okm-query-papers-by-topic-with-ql ()
;;   "CONNECTOR."
;;   (interactive)
;;   (let* ((topics 
;;           (org-roam-node-read-multiple "Query topics: "))
;;          (connector (if (> (length topics) 1)
;;                         (pcase (completing-read "connector: " '(and or) nil t)
;;                           ("or" 'or)
;;                           ("and" 'and))
;;                       'and))
;;          (topic-ids (list (append `(okm-parent (quote ,connector))
;;                                   (mapcar
;;                                    (lambda (topic)
;;                                      `(quote ,(cons (org-roam-node-title topic) (org-roam-node-id topic))))
;;                                    topics))))
;;          (query (append '(and (level <= 1)) topic-ids))
;;          (after-change-major-mode-hook nil))
;;     (org-ql-search (f-glob "*.org" (f-join okm-base-directory "research_papers"))  query)))

;; (defun okm-query-papers-by-pdf-string (regexp)
;;   "query with org-ql REGEXP."
;;   (interactive "sRegexp: ")
;;   (let* ((query `(and (level <= 1) (pdf-regexp ,regexp))))
;;     (org-ql-search '("~/Documents/org/brain/research_papers/")  query)))

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

;; (defun okm-org-agenda-recompute ()
;;   "Recompute the agenda list."
;;   (interactive)
;;   (setq org-agenda-files (flatten-tree (list (--map (f-files it (lambda (f)
;;                                                                   (and (f-ext-p f "org")
;;                                                                        (with-temp-buffer
;;                                                                          (insert-file f)
;;                                                                          (org-mode)
;;                                                                          (if-let (kwds (org-collect-keywords '("filetags")))
;;                                                                              (not (member "agendauntrack" (split-string (cadar kwds) ":" 'omit-nulls)))
;;                                                                            t)))))
;;                                                     (f-glob "~/Documents/org/brain/*/project_boards"))
;;                                              (f-files "~/Documents/org/brain/roam-notes"
;;                                                       (lambda (f)
;;                                                         (and (f-ext-p f "org")
;;                                                              (with-temp-buffer
;;                                                                (insert-file f)
;;                                                                (org-mode)
;;                                                                (when-let (kwds (org-collect-keywords '("filetags")))
;;                                                                  (member "agendatrack" (split-string (cadar kwds) ":" 'omit-nulls)))))))
;;                                              (f-glob "~/Documents/org/brain/personal/**/*.org")
;;                                              '("~/Documents/org/brain/google_calender_unlisted.org")))))
