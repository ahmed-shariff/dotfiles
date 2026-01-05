;;; org-roam-extensions.el --- a simple package                     -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; On windows when the `cygwin1.dll mismatch issue` issue happens, This is solved by manually running the command seen in the *compilation* buffer
;; Would have to try that on the msys2 console
(straight-use-package 'org-roam)
(straight-use-package '(org-roam-ql :type git :host github :repo "ahmed-shariff/org-roam-ql"
                                    :files (:defaults (:exclude "org-roam-ql-ql.el"))))
(require 'org-roam)
(require 'org-roam-ql)

(use-package consult-org-roam
  :ensure t
  :demand 3
  :after org-roam
  :init
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-.")

  (defvar amsha/org-consult-notes-org-roam--nodes 
    `(:require-match t
      :category 'org-roam-node
      ;; :state ,(consult-org-roam--node-preview)
      :lookup (lambda (selected candidates input narrow) (alist-get selected candidates input nil #'equal))
      :action ,(lambda (cand)
                 (org-roam-node-open (get-text-property 0 'node cand))))
    "Setup for `org-roam' and `consult--multi'.")

  (defun consult-notes-visit-relation (node)
    "Navigate to related node of `node'."
    (interactive (list (or (when (eq major-mode 'org-mode) (org-roam-node-at-point)) (org-roam-node-read nil nil nil t "Select node: "))))
    (consult--multi (list
                     (plist-multi-put (copy-sequence amsha/org-consult-notes-org-roam--nodes)
                                      :name (propertize "Backlinks" 'face 'consult-help)
                                      :narrow ?b
                                      :items (lambda () (em (--map
                                                             (-->
                                                              (org-roam-backlink-source-node it)
                                                              (propertize
                                                               (org-roam-node-title it)
                                                               'node it))
                                                             (org-roam-backlinks-get node :unique t)))))
                     (plist-multi-put (copy-sequence amsha/org-consult-notes-org-roam--nodes)
                                      :name (propertize "Brain Children" 'face 'consult-help)
                                      :narrow ?c
                                      :items (lambda () (--map (propertize
                                                                (org-roam-node-title (org-roam-node-from-id it))
                                                                'node (org-roam-node-from-id it))
                                                               (okm-get-children (org-roam-node-id node)))))
                     (plist-multi-put (copy-sequence amsha/org-consult-notes-org-roam--nodes)
                                      :name (propertize "Forwardlink" 'face 'consult-help)
                                      :narrow ?f
                                      :items (lambda () (--map (propertize (car it) 'node (org-roam-node-from-id (cdr it)))
                                                               (org-roam-db-query
                                                                [:select [nodes:title nodes:id]
                                                                         :from links :inner :join nodes :on (= links:dest nodes:id)
                                                                         :where (in links:source $v1)]
                                                                (vector (org-roam-node-id node))))))
                     (plist-multi-put (copy-sequence amsha/org-consult-notes-org-roam--nodes)
                                      :name (propertize "Brain Parents" 'face 'consult-help)
                                      :narrow ?p
                                      :items (lambda () (--map (propertize
                                                                (org-roam-node-title (org-roam-node-from-id it))
                                                                'node (org-roam-node-from-id it))
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
      (consult-ripgrep)))

  (defun amsha/consult-org-roam--node-preview (filter-fn)
    "Create preview function for nodes.
FILTER-FN takes a node and return non-nil if it should be previewed."
    (let ((open (consult--temporary-files))
          (preview (consult--buffer-preview))
          (state  (window-state-get)))
      (lambda (action cand)
        (when (eq action 'exit)
          (progn
            ;; Restore saved window state
            ;; To move point to the original position
            (window-state-put state)
            (funcall open)))
        (if (and (org-roam-node-p cand) (funcall filter-fn cand))
            (funcall preview action
                     (and cand
                          (eq action 'preview)
                          (set-window-start
                           (selected-window)
                           (org-roam-node-point cand))
                          (funcall open (org-roam-node-file cand))))))))

  (defvar consult-org-roam-ql--history nil)

  ;; TODO: filter-fn and sort-fn does notthing now!
  (defun consult-org-roam-ql (&optional initial-input filter-fn sort-fn
                                        require-match prompt)
    "Consult with org-roam-ql for searching/narrowing."
    (interactive)
    (minibuffer-with-setup-hook
        (lambda ()
          ;; KLUDGE: No idea why this is here!
          (set-syntax-table emacs-lisp-mode-syntax-table)
          (add-hook 'completion-at-point-functions
                    #'org-roam-ql--completion-at-point nil t))
      (let* (split-pos mb-str is-highlight
             (consult-async-split-styles-alist
              `((org-roam-ql
                 ;; :initial ?\;  ;; this was interacting with the embark
                 :function
                 ;; Override how the empty string is handled!
                 ;; When empty async-str should return default candidates
                 ,(lambda (str style)
                    (pcase-let* ((res (consult--split-perl str style))
                                 (`(,async-str ,pos ,start-highlight . ,end-highlight) res)
                                 (force (or (get-text-property 0 'consult--force async-str)
                                            (and (not (null start-highlight)) (not (null end-highlight))))))
                      ;; This gets called at severaal places. We only want the data when it is
                      ;; called with the force value!
                      (setq is-highlight (or start-highlight end-highlight))
                      (when force
                        (setq split-pos pos
                              mb-str str))
                      (cond
                       ((and force (equal "" async-str))
                        (setf (car res) (propertize "-" 'consult--force end-highlight)))
                       ;; when no leading char split-perl can use, just fallback to normal matching
                       ((and (not (or is-highlight
                                      (string-empty-p async-str))))
                        (setf res (list (propertize (car res) 'consult--org-roam-ql-state 'no-narrow)
                                        ;; forcing the actual matching to work
                                        0 '(0 . 0) '(0 . 0)))))
                      res)))))
             (corfu-auto nil)
             (consult-async-input-debounce 1)
             ;; Default candidates
             (nodes (mapcar (lambda (node)
                              (cons (propertize (org-roam-node-title node) 'node node) node))
                            (org-roam-node-list)))
             ;; The sink is what holds the candidates and feed it back to all-completions
             (sink (consult--async-sink))
             (overriden-keymap (make-sparse-keymap))
             (delete-minibuffer-override
              (lambda ()
                (interactive)
                (cond
                 ((and mb-str split-pos)
                  (delete-minibuffer-contents)
                  (insert (substring mb-str 0 split-pos)))
                 ((not is-highlight)
                  (delete-minibuffer-contents))))))

        (define-key overriden-keymap "\M-d" delete-minibuffer-override)
        (define-key overriden-keymap "\M-D" #'delete-minibuffer-contents)
        (define-key overriden-keymap (kbd "C-,") (lambda ()
                                                   (interactive)
                                                   (when (minibufferp)
                                                     (embark-select)
                                                     (funcall delete-minibuffer-override))))
        (when (not (featurep 'org-roam-ql))
          (require 'org-roam-ql))
        (set-keymap-parent overriden-keymap org-roam-ql--read-query-map)
        ;; Feeding initial set of candidates to sink
        (funcall sink nodes)
        (-->
         (consult--async-pipeline
          (consult--async-split 'org-roam-ql)
          (consult--dynamic-collection
              (lambda (input)
                (if (or (length= input 0)
                        (equal input "-")
                        (eq (get-text-property 0 'consult--org-roam-ql-state input)
                            'no-narrow))
                    nodes
                  ;; TODO: can I update the state/indicator somehow?
                  (condition-case err
                    (mapcar
                     (lambda (node)
                       (cons (propertize (org-roam-node-title node) 'node node) node))
                     (org-roam-ql-nodes (read input)))
                    (user-error
                     (minibuffer-message (propertize (cadr err) 'face 'consult-async-failed))
                     nodes)))))
          (consult--async-indicator)
          (consult--async-refresh))
         (funcall it sink)
         (consult--read
          it
          :prompt (or prompt "Node: ")
          :initial (if initial-input initial-input ";")
          :keymap overriden-keymap
          :category 'org-roam-node
          :sort nil ;; TODO
          :require-match require-match
          :async-wrap nil
          :state (amsha/consult-org-roam--node-preview
                  (lambda (node)
                    (not (string-match "research_papers" (org-roam-node-file node)))))
          :history 'consult-org-roam-ql--history
          ;; Taken from consult-org-roam
          ;; Uses the DEFAULT argument of alist-get to return input in case the input is not found as key.
          :lookup (lambda (selected candidates input narrow) (alist-get selected candidates input nil #'equal)))
         (if (org-roam-node-p it)
             it
           (org-roam-node-create :title it))))))

  (advice-add #'consult-org-roam-node-read :override #'consult-org-roam-ql))

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

(use-package org-roam-ql-ql
  :straight (org-roam-ql-ql :type git :host github :repo "ahmed-shariff/org-roam-ql"
                            :files (:defaults (:exclude "org-roam-ql.el")))
  :after (org-roam org-ql org-roam-ql)
  :config
  (org-roam-ql-ql-init))

;; org-roam functions **************************************************************
;;;###autoload
(defun amsha/org-roam-db-sync ()
  (interactive)
  (org-roam-db-sync)
  (okm-org-agenda-recompute-org-roam-ql))

;;;###autoload
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

;;;###autoload
(defun okm-org-roam-quick-capture-topic (topic)
  "Quick add a topic to unclassified indices."
  (interactive (list (read-string "Topic: " (when (region-active-p) (buffer-substring (region-beginning) (region-end))))))
  (org-roam-with-file (file-truename (expand-file-name "unclassified_index.org" okm-base-directory)) t
    (goto-char (marker-position (org-roam-capture-find-or-create-olp (list topic))))
    (org-id-get-create)
    (org-roam-db-update-file)))

;;;###autoload
(defun amsha/backup-org-roam-db ()
  (interactive)
  (copy-file org-roam-db-location (format "%s-backup" org-roam-db-location) t))

;;;###autoload
(defun org-roam-buffer-for-node (node)
  "Display org-roam-buffer for NODE.
Like `org-roam-buffer-display-dedicated', but always
prompt when used interactively"
  (interactive (list (org-roam-node-read (when (derived-mode-p 'org-roam-mode)
                                           (when-let (_node (org-roam-node-at-point))
                                             (org-roam-node-title _node))))))
  (org-roam-buffer-display-dedicated node))

(defun org-roam-brain-children-section (node)
  "The brain children section for NODE.
Copied  from `org-roam-backlink-get'.
see also `org-roam-backlinks-section-with-ql-filter'.
"
  (let* ((backlinks (seq-sort #'org-roam-backlinks-sort (okm-links-get node)))
         (filter org-roam-ql-buffer-filter)
         (filter-node-ids (and filter
                               (condition-case _err
                                   (-map #'org-roam-node-id (org-roam-ql-nodes filter))
                                 (user-error
                                  (message "Cannot apply filter: %s" _err))))))
    (when backlinks
      (if filter-node-ids
          (progn
            (setq-local org-roam-ql--buffer-displayed-filter org-roam-ql-buffer-filter)
            (setq backlinks 
                  (--filter (member (org-roam-node-id (org-roam-backlink-source-node it)) filter-node-ids)
                            backlinks)))
        (setq filter nil))
      (magit-insert-section (org-roam-brain-children)
        (magit-insert-heading (if filter
                                  (concat "Brain Children (filter: "
                                          (-->
                                           (format "%s" filter)
                                           (substring it 0 (min 40 (length it))))
                                          "):")
                                "Brain Children:"))
        (dolist (backlink backlinks)
          (org-roam-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink)))
        (insert ?\n)))))

(defun org-roam-subtree-aware-preview-function (&optional node query)
  "Same as `org-roam-preview-default-function', but gets entire subtree in research_papers or notes."
  (if (--> (or node (org-roam-node-at-point))
           (org-roam-node-file it)
           (or (s-matches-p "brain/work/notes" it)
               (s-matches-p "brain/personal/notes" it)
               (s-matches-p "brain/roam-notes" it)
               (s-matches-p "brain/research_papers" it)
               (f-ancestor-of-p bibtex-completion-notes-path it)))
      (let* ((full-file (< (point) 5)) ;; means we are trying to display the whole file, I think!
             ;; Even when displaying full file, we skip the initial meta data
             (beg (progn
                    (unless (org-at-heading-p)
                        (org-previous-visible-heading 1))
                    (if (org-id-get)
                        (org-roam-end-of-meta-data t)
                      (org-beginning-of-line))
                    (point)))
             (end (if full-file
                      (point-max)
                    (progn (when (org-id-get)
                             (org-previous-visible-heading 1)
                             (org-beginning-of-line))
                           (org-end-of-subtree)
                           (point)))))
        (-reduce 
         (lambda (str el)
           ;; remove properties not interested. If prop drawer is empty at the end, remove drawer itself
           (s-replace-regexp (format "\n *:%s:.*$" el) "" str))
         ;; remove links
         (list (amsha/org-repalce-link-in-string (string-trim (buffer-substring-no-properties beg end)))
               "INTERLEAVE_PAGE_NOTE" "BRAIN_CHILDREN" "HIGHLIGHT" okm-parent-property-name "PROPERTIES:\n *:END")))
    (org-roam-preview-default-function)))

;; (org-roam-node-action org-roam-node-ref-hydra
;;   (org-ref-citation-hydra/body))

;; (defun org-roam-node-read-multiple (&optional prompt)
;;   "Like org-roam-node-read, but with mulitiple read excluding the template used by roam."
;;   (let ((nodes (mapcar (lambda (node)
;;                          (cons (org-roam-node-title node) node))
;;                        (org-roam-node-list))))
;;     (-non-nil
;;      (--map (org-roam-node-from-title-or-alias it)
;;             (completing-read-multiple
;;              (or prompt "Node(s):")
;;              (lambda (string pred action)
;;                (if (eq action 'metadata)
;;                    '(metadata
;;                      ;; (annotation-function . consult-notes-org-roam-annotate)
;;                      (category . org-roam-node))
;;                  (complete-with-action
;;                   action
;;                   nodes
;;                   string
;;                   pred))))))))

(defun org-roam-node-annotator (cand)
  "Annotate org-roam-nodes in completions"
  (when-let* ((node (or
                     (get-text-property 0 'node cand)
                     (condition-case err
                         (org-roam-node-from-title-or-alias (org-no-properties cand))
                       (error nil))))
              (file (concat (s-replace ".org" "" (f-relative (org-roam-node-file node) okm-base-directory))
                            (when (> (org-roam-node-level node) 1) (concat "::" (string-join (org-roam-node-olp node) " > "))))))
    (marginalia--fields
     (file :face 'shadow :truncate 1.0)
     ((marginalia--time (org-roam-node-file-mtime node)) :face 'org-cite))))

(org-roam-node-action org-roam-node-view-noter
  (org-noter))
(org-roam-node-action org-roam-node-view-topics
  (okm-print-parents))
(org-roam-node-action org-roam-node-add-parents
  (okm-add-parent-topic))

(defmacro amsha/org-roam-with-file (file keep-buf-p &rest body)
  "Like `org-roam-with-file', with kill-buffer hooks disabled.

`kill-buffer-query-functions' and `kill-buffer-hook' are set to nil."
  (declare (indent 2) (debug t))
  `(let* ((kill-buffer-query-functions nil)
          (kill-buffer-hook nil))
     (org-roam-with-file ,file ,keep-buf-p ,@body)))

;; org-roam-ql functions ***********************************************************

;; taken from `org-roam-ql--expand-link'
(cl-defun org-roam-ql--expand-recursive-link (source-or-query is-backlink type inlcude-refs)
  "Expansion function for recursive links.
Returns a list of nodes that have back links to the node that
SOURCE-OR-QUERY resolves to, as a list. If there are more than 1
resulting node from source-or-query, it will return an error.  TYPE is
the type of the link.  If is-backlink is nil, return forward
links, else return backlinks"
  (let* ((query-nodes
          ;; NOTE: -compare-fn is set in the `org-roam-ql--expand-query'
          (-uniq
           (org-roam-ql--nodes-cached source-or-query)))
         (len (length query-nodes))
         (_ (when (not (eq 1 len))  (user-error "%s results in %d nodes" source-or-query len)))
         (target-col (if is-backlink 'links:source 'links:dest))
         (test-col (if is-backlink 'links:dest 'links:source)))
    (->> (org-roam-db-query
          (apply
           #'vector
           `(
             :with
             :recursive
             [
              ;; Compute trees of headline nodes. org-roam does not store
              ;; these natively.
              (as (funcall headlines node_id relative_id)
                  [
                   :select [node:id relative:id]
                   :from (on (join (as [:select * :from nodes :where (in id $v1)] node)
                                   (as nodes relative))
                             (and (= node:file relative:file)
                                  (,(if is-backlink '> '<) node:level relative:level)
                                  (or (and (= ,(if is-backlink 'relative:level 'node:level) 1)
                                           (LIKE ,(if is-backlink 'node:olp 'relative:olp)
                                                 ;; TODO escape meta chars in title
                                                 (|| '"("
                                                     ,(if is-backlink 'relative:title 'node:title)
                                                     '"%")))
                                      (and (> ,(if is-backlink 'relative:level 'node:level) 1)
                                           (LIKE ,(if is-backlink 'node:olp 'relative:olp)
                                                 ;; TODO escape meta chars in olp/title
                                                 (|| '"("
                                                     (funcall TRIM relative:olp '"()")
                                                     '" "
                                                     ,(if is-backlink 'relative:title 'node:title)
                                                     '"%"))))))
                   ;; :group-by node:id
                   ;; :having (= relative:level (funcall MAX relative:level))])
                   ])
              (as (funcall links_tr id)
                  [
                   :values $v1
                   :union
                   :select ,target-col
                   :from [links links_tr]
                   :where
                   (and ,(if type `(= type $s3)
                           `(not (in type $v2)))
                        (= ,test-col links_tr:id))
                   ,@(when inlcude-refs
                       `(:union
                         :select ,target-col
                         :from [(on (join links refs)
                                    (and (= ,test-col refs:ref) ;,test-col
                                         (= links:type refs:type)))
                                links_tr]
                         :where (= refs:node_id links_tr:id)
                         :union
                         :select citations:node_id
                         :from [(on (join citations refs)
                                    (and (= citations:cite_key refs:ref)
                                         (= refs:type "cite")))
                                links_tr]
                         :where (= refs:node_id links_tr:id)))
                   :union
                   ;; :select headlines:node_id
                   :select headlines:relative_id
                   :from [headlines links_tr]
                   :where (= headlines:node_id links_tr:id)
                   ])]
             :select id
             :from links_tr
             :where (not (in id $v1))))
          (apply #'vector (-map #'org-roam-node-id query-nodes))
          ["http" "https"]
          type)
         (--map (org-roam-node-from-id (car it))))))

(cl-defun org-roam-ql-recursive-backlink-to (source-or-query &key (type "id") inlcude-refs)
  "Get recursive backlinks.
Returns a list of nodes that have back links to the node that
SOURCE-OR-QUERY resolves to, as a list. If there are more than 1
resulting node from source-or-query, it will return an error.  TYPE is
the type of the link."
  (org-roam-ql--expand-recursive-link source-or-query t type inlcude-refs))

(cl-defun org-roam-ql-recursive-backlink-from (source-or-query &key (type "id") inlcude-refs)
  "Get recursive forwardlinks.
Returns a list of nodes that have back links to the node that
SOURCE-OR-QUERY resolves to, as a list. If there are more than 1
resulting node from source-or-query, it will return an error.  TYPE is
the type of the link."
  (org-roam-ql--expand-recursive-link source-or-query nil type inlcude-refs))

(defun bibtex-keys-to-nodes (keys)
  "Return a list of org-roam-nodes for corresponding list of KEYS."
  (-map
   #'org-roam-node-from-id
   (-flatten (org-roam-db-query [:select id :from nodes :where (in file $v1)]
                                (apply #'vector (--map (f-expand (format "%s.org" it) bibtex-completion-notes-path)
                                                       keys))))))

(defun okm-get-all-roam-nodes-in-file (f)
  (org-roam-ql-nodes (list [:select [id] :from nodes :where (= file $s1)] f)))


(defun org-roam-ql-search-bib-files (regex)
  "And org roam ql expansion function to regex search bib files.

Returns the sql statement that can be used with `org-roam-ql-nodes'."
  (let* ((files (bibtex-completion-normalize-bibliography 'bibtex))
         (bibtex-completion-additional-search-fields '(booktitle journal))
         entries)
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char 0)
        (while (re-search-forward regex nil t)
          (save-excursion
            (save-match-data
              (bibtex-beginning-of-entry)
              (if-let* ((_ (parsebib--char '(?@)))
                        (type (parsebib--identifier))
                        (open (parsebib--char '(?\{ ?\( )))
                        (key (parsebib--key)))
                  (push key entries)
                (message "Error reading at point %s in " (point) file)))))))
    (bibtex-keys-to-nodes entries)))

;;;###autoload
(defun okm-org-roam-list-notes (entries)
  "Filter based on the list nodes in the notes files. Interactive only handles only one node.
If prefix arg used, search whole db."
  (interactive (list ;;(org-roam-node-read nil nil nil 'require-match "Filter on Nodes:")))
                (list (org-roam-node-read))))
  (let* ((entries (-uniq
                 (-flatten
                  (--map (let ((node (if (stringp it) (org-roam-node-from-title-or-alias it) it)))
                           (if (equal (org-roam-node-level node) 0)
                               (list node (okm-get-all-roam-nodes-in-file (org-roam-node-file node)))
                             node))
                         entries))))
         (names (s-join "," (--map (concat (org-roam-node-title it) "…") entries)))
         (id-query (apply #'vector (--map (org-roam-node-id it) entries))))
    (org-roam-ql--render-roam-buffer
     'backlinks
     entries
     '(or (file "brain/personal/notes")
          (file "brain/work/notes"))
     (format "(%s)" names)
     (format "*notes: %s*" names))))

(defvar okm-org-roam-preview-kills '())

;;;###autoload
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

;;;###autoload
(defun okm-org-roam-preview-kills-yank ()
  (interactive)
  (dolist (content okm-org-roam-preview-kills)
    (insert content "\n\n"))
  (setq okm-org-roam-preview-kills nil))

;;;###autoload
(defun okm-insert-paper-ids-for-query ()
  "Prompt for nodes and print the file names a org style list."
  (interactive)
  (insert "\n")
  (--map
   (when-let* ((f (org-roam-node-file it))
               (_ (string-match "research_papers" f)))
     (insert "- " (file-name-base f) "\n"))
   (org-roam-ql-nodes (org-roam-ql--read-query))))


;;;###autoload
(defun okm-goto-last-dailies (args)
  "Go the last dailies without going the capture process."
  (interactive "P")
  (when args
    (org-roam-db-sync))
  (org-roam-node-open (car (org-roam-ql-nodes
                            ;; Assuming I have a note in the last 100 days!
                            '(dailies-range "-100")
                            "title-reverse"))))
;; org-roam-ql expansions and stuff ************************************************
(org-roam-ql-defexpansion 'backlink-to-recursive
  "Recursive backlinks (heading, backlink & refs)"
  #'org-roam-ql-recursive-backlink-to)

(org-roam-ql-defexpansion 'backlink-from-recursive
  "Recursive backlinks (heading, backlink & refs)"
  #'org-roam-ql-recursive-backlink-from)

(org-roam-ql-defexpansion 'bib
  "Regex on bib files"
  #'org-roam-ql-search-bib-files)

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

(org-roam-ql-defexpansion 'dailies-range
  "Dailies in range"
  (lambda (&optional min max)
    (--> (--map (cons (time-convert (encode-time
                                     (org-parse-time-string
                                      (file-name-sans-extension
                                       (file-name-nondirectory it))))
                                    'integer)
                      it)
                (-flatten (--map (f-glob "*-*-*-*.org" it)
                                 (list (file-truename "~/Documents/org/brain/work/notes/")
                                       (file-truename "~/Documents/org/brain/personal/notes/")))))
         (if min (--filter (time-less-p (org-roam-ql--read-date-to-ts min) (car it)) it)
           it)
         (if max (--filter (time-less-p (car it) (org-roam-ql--read-date-to-ts max)) it)
           it)
         (-map #'cdr it)
         (--sort (string< it other) it)
         (-flatten (org-roam-db-query [:select id :from nodes :where (in file $v1) :and (= level 0)] (vconcat nil it)))
         (-map #'org-roam-node-from-id it))))

(org-roam-ql-defexpansion 'key-order-range
  "Papers in key-order range"
  (lambda (min max)
    (->>
     (org-roam-db-query [:select [id properties] :from nodes :where (like file $s1) :and (= level 1)] "%research_papers%")
     (--map (when-let ((key-order (alist-get "KEY_ORDER" (cadr it) nil nil #'string-equal))) 
              (cons (car it) (string-to-number key-order))))
     (--filter (and (cdr it) (> (cdr it) min) (< (cdr it) max)))
     (--map (org-roam-node-from-id (car it))))))

(org-roam-ql-defexpansion 'year-range
  "Papers in year range"
  (lambda (min max)
    (->>
     (org-roam-db-query [:select [id properties] :from nodes :where (like file $s1) :and (= level 1)] "%research_papers%")
     (--map (when-let ((key-order (alist-get "YEAR" (cadr it) nil nil #'string-equal))) 
              (cons (car it) (string-to-number key-order))))
     (--filter (and (cdr it) (> (cdr it) min) (< (cdr it) max)))
     (--map (org-roam-node-from-id (car it))))))

;; (org-roam-ql-defpred 'pdf-string
;;                      "Attached PDF has string"
;;                      (lambda (node)
;;                        (cdr (assoc "PDF_TEXT_FILE" (org-roam-node-properties node))))
;;                      #'okm--test-regexp-on-file)

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

(org-roam-ql-register-sort-fn "key-order" (lambda (el1 el2)
                                            (string< (cdr (assoc "KEY_ORDER" (org-roam-node-properties el1)))
                                                     (cdr (assoc "KEY_ORDER" (org-roam-node-properties el2))))))

(org-roam-ql-add-saved-query 'rp "Research papers" '(file "research_papers"))
(org-roam-ql-add-saved-query 'nrp "not Research papers" '([:select id :from nodes :where (not (like file $s1))] "%research_papers%"))
(org-roam-ql-add-saved-query 'pe "People" '(file "People.org"))
(org-roam-ql-add-saved-query 'rt "Research topics" '(file "brain/research topics.org"))
(org-roam-ql-add-saved-query 'pr "Projects" '(file "project_boards"))
(org-roam-ql-add-saved-query 'tg "Tags" '(and (file "brain/tags.org") (level= 1)))
(org-roam-ql-add-saved-query 'repo "Repositories" '(file "repositories.org"))
(org-roam-ql-add-saved-query 'tp "All topics"
  `([:select id :from nodes :where (and (> level 0)
                                        (or (like file $s1)
                                            (like file $s2)
                                            (like file $s3)
                                            (like file $s4)
                                            (like file $s5)
                                            (and (like file $s6) (like title $s7))))]
    "%index.org%" "%misc_topics.org%" "%People.org%" "%research topics.org%" "%tags.org%" "%project_boards/%.org%" "%literature%" "%unclassified_index.org%"))
(org-roam-ql-add-saved-query 'lvl0 "file nodes" '(level= 0))
(org-roam-ql-add-saved-query 'lvl1 "head nodes lvl1" '(level= 1))
(org-roam-ql-add-saved-query 'inp "inprogress" '(todo "INPROGRESS" t))
(org-roam-ql-add-saved-query 'todo "todo" '(todo "TODO" t))
(org-roam-ql-add-saved-query 'l10rp "last 10 papers" '(last-n-papers 10))
(org-roam-ql-add-saved-query 'l20rp "last 20 papers" '(last-n-papers 20))
(org-roam-ql-add-saved-query 'dailies-today "dailies today" '(dailies-range "-1d"))
(org-roam-ql-add-saved-query 'dailies-last-two-days "dailies last two days" '(dailies-range "-2d"))
(org-roam-ql-add-saved-query 'dailies-last-week "dailies last week" '(dailies-range "-8d"))
(org-roam-ql-add-saved-query 'dailies-yesterday "dailies yesterday" '(dailies-range "-2d" "-1d"))

(with-eval-after-load 'org-ql
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
      (org-roam-db-query [:select * :from links :where (in dest $v1) :and (= source $s2)] backlink-destinations id))))

;; setup ***************************************************************************
(bind-keys ("C-c n l" . org-roam-buffer-for-node)
           ("C-c n L" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ;;("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n t" . okm-org-roam-quick-capture-topic)
           ("C-c n y" . amsha/org-roam-db-sync)
           ;; Dailies
           ("C-c n j" . org-roam-dailies-capture-today)
           ;; org-roam-bibtex
           ("C-c n b" . orb-insert-link)
           ;; consult-org-roam
           ("C-c n e" . consult-org-roam-file-find)
           ("C-c n B" . consult-org-roam-backlinks-recursive)
           ("C-c n r" . consult-org-roam-search)
           ("C-c n v" . consult-notes-visit-relation)
           :map org-roam-mode-map
           ("j" . magit-section-forward)
           ("k" . magit-section-backward)
           ("v" . org-roam-ql-buffer-dispatch)
           :map org-roam-node-map
           ("C-c o s" . org-roam-node-view-topics)
           ("C-c o o" . org-roam-node-view-noter)
           ("C-c o p" . org-roam-node-add-parents)
           ;; ("C-c o r" . org-roam-node-ref-hydra)
           :map org-roam-preview-map
           ("C-c o s" . org-roam-node-view-topics)
           ("C-c o o" . org-roam-node-view-noter)
           ("C-c o p" . org-roam-node-add-parents)
           ("w" . okm-org-roam-ql-copy-preview)
           ;; ("C-c o r" . org-roam-node-ref-hydra)
           :map minibuffer-mode-map
           ("C-c n i" . org-roam-ql-insert-node-title)
         )

(add-hook 'kill-emacs-hook #'amsha/backup-org-roam-db)
(add-hook 'org-roam-post-node-insert-hook (lambda (_ _) (insert " ")))
(add-to-list 'org-agenda-custom-commands '("ca" "All upcoming agenda (roam)" org-roam-ql-agenda-block '(or (scheduled-is-after "+0")
                                                                                                           (deadline-is-after "+0"))))
(add-to-list 'marginalia-annotators
             '(org-roam-node org-roam-node-annotator marginalia-annotate-face builtin none))

(setq org-roam-v2-ack t
      org-roam-database-connector 'sqlite-builtin  ;;sqlite-builtin sqlite
      org-roam-directory (file-truename okm-base-directory)
      org-roam-file-extensions '("org" "org_archive")
      org-roam-dailies-directory "dailies"
      org-roam-dailies-capture-templates '(("d" "default" entry "%(okm-board-task-location)"
                                            :target (file+head
                                                     "~/Documents/org/brain/work/notes/%<%Y-%m-%d-%A>.org" "#+title: %<%Y-%m-%d-%A>\n\n")
                                            :jump-to-captured t)
                                           ("p" "personal" entry "%(okm-board-task-location)"
                                            :target (file+head
                                                     "~/Documents/org/brain/personal/notes/%<%Y-%m-%d-%A>.org" "#+title: %<%Y-%m-%d-%A>\n\n")
                                            :jump-to-captured t))
      org-roam-capture-templates '(("d" "default" entry "* ${title}%?
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
                                    :unnarrowed t))
      org-roam-node-display-template "${title}"
      org-roam-node-formatter (lambda (node) (s-replace-regexp " \\[[0-9]+/[0-9]+\\]" "" (org-roam-node-title node)))
      org-roam-preview-function #'org-roam-subtree-aware-preview-function

      org-roam-ql-default-org-roam-buffer-query (lambda () `(backlink-to (id ,(org-roam-node-id org-roam-buffer-current-node)) :type nil))
      org-roam-ql-preview-function #'org-roam-subtree-aware-preview-function
      org-roam-mode-sections (list #'org-roam-brain-children-section
                                   '(org-roam-backlinks-section-with-ql-filter :unique nil) ;; Setting to nil becuase when t it doesn't work too well with notes
                                   #'org-roam-reflinks-section-with-ql-filter)
      )

(org-roam-db-autosync-mode)

(provide 'org-roam-extensions)
;;; org-roam-extensions.el ends here
