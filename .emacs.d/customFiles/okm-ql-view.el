;; TODO: The input can be
;; - A valid query to pass to `org-roam-db-query'
;; - If just a :where syntax, append the [:select * :from nodes] to begining and run the query
;; - A list of nodes itself.
;; - A function that returns a list of nodes
(defun org-roam-view--get-nodes-from-query (source-or-query)
  "."
  (cond
   ((-all-p #'org-roam-node-p source-or-query) source-or-query)
   ((and (listp source-or-query) (vectorp (car source-or-query)))
    (let ((query (car source-or-query))
          (args (cdr source-or-query)))
      (--map (org-roam-node-from-id (car it))
       (apply #'org-roam-db-query
             (if (equalp :select (aref query 0))
                 query
               (vconcat [:select id :from nodes :where] query))
             args))))
   ((functionp source-or-query) (funcall source-or-query))))
    

(defun org-ql-roam-view (source-or-query title &optional super-groups)
  "Basically what `org-ql-search does', but for org-roam-nodes.
NODES is a list of org-roam-nodes. TITLE is a title to associate with the view.
See `org-roam-search' for details on SUPER-GROUPS."
  (let* ((nodes (org-roam-view--get-nodes-from-query source-or-query))
         (strings '())
         (title (format "org-roam - %s" title))
         (buffer (format "%s %s*" org-ql-view-buffer-name-prefix title))
         (header (org-ql-view--header-line-format
                  :title title))
         (org-ql-view-buffers-files (mapcar #'org-roam-node-file nodes))
         (org-ql-view-query '(property "ID"))
         (org-ql-view-sort nil)
         (org-ql-view-narrow nil)
         (org-ql-view-super-groups super-groups)
         (org-ql-view-title title))
    (dolist-with-progress-reporter (node nodes)
        (format "Processing %s nodes" (length nodes))
      (push (org-roam-ql-view--format-node node) strings))
    (when super-groups
      (let ((org-super-agenda-groups (cl-etypecase super-groups
                                       (symbol (symbol-value super-groups))
                                       (list super-groups))))
        (setf strings (org-super-agenda--group-items strings))))
    (org-ql-view--display :buffer buffer :header header
      :string (s-join "\n" strings))))


(cl-defun org-roam-ql-view--display (&key (buffer org-ql-view-buffer) header string)
  "Display STRING in `org-ql-view' BUFFER.

BUFFER may be a buffer, or a string naming a buffer, which is
reused if it already exists.  `org-ql-view-buffer' is used by
default.

HEADER is a string displayed in the buffer's header line.

The following special variables, if non-nil, are set
buffer-locally to preserve their value in the buffer for
subsequent refreshing of the buffer: `org-ql-view-buffers-files',
`org-ql-view-query', `org-ql-view-sort', `org-ql-view-narrow',
`org-ql-view-super-groups', `org-ql-title.'"
  (declare (indent defun))
  (let* (
         (vars (list 'org-ql-view-buffers-files 'org-ql-view-query
                     'org-ql-view-sort 'org-ql-view-narrow
                     'org-ql-view-super-groups 'org-ql-view-title))
         ;; Save the values of variables which are set buffer-locally in the
         ;; results buffer, which we want to override and set buffer-locally again.
         (vals (cl-loop for symbol in vars
                        collect (cons symbol (symbol-value symbol))))
         (buffer (if (bufferp buffer)
                     buffer
                   (with-current-buffer (get-buffer-create (or buffer "*org-roam-ql-buffer*"))
                     (unless (eq major-mode 'org-agenda-mode)
                       (org-agenda-mode)
                       (setf buffer-read-only t))
                     (current-buffer)))))
    (with-current-buffer buffer
      (setq-local bookmark-make-record-function #'org-ql-view-bookmark-make-record)
      (use-local-map org-ql-view-map)
      ;; Prepare buffer, saving data for refreshing.
      (cl-loop for symbol in vars
               do (progn
                    (kill-local-variable symbol)
                    (set (make-local-variable symbol) (alist-get symbol vals nil nil #'equal))))
      (setf header-line-format header)
      ;; Clear buffer, insert entries, etc.
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert string)
        (pop-to-buffer (current-buffer));;org-ql-view-display-buffer-action)
        (org-agenda-finalize)
        (goto-char (point-min))))))


(defun org-roam-ql-view--format-node (node)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return ELEMENT as a string with text-properties set by its property list.
Its property list should be the second item in the list, as
returned by `org-element-parse-buffer'.  If ELEMENT is nil,
return an empty string."
  (if (not node)
      ""
    (let* ((marker
            (org-roam-with-file (org-roam-node-file node) t
              (goto-char (org-roam-node-point node))
              (point-marker)))
           (properties (list
                        'org-marker marker
                        'org-hd-marker marker))
           ;; (properties '())
           (string (org-roam-node-title node))) ;;(org-roam-node--format-entry (org-roam-node--process-display-format org-roam-node-display-template) node)))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      ;; Add all the necessary properties and faces to the whole string
      (--> string
        ;; FIXME: Use proper prefix
        (concat "  " it)
        (org-add-props it properties
          'org-agenda-type 'search
          'todo-state (org-roam-node-todo node)
          'tags (org-roam-node-tags node)
          ;;'org-habit-p (org)
          )))))

(provide 'okm-ql-view)

;; for debugging
;; (let ((nodes (subseq (org-roam-node-list) 0 10)))
;;          (elp-reset-all)
;;          (--map (kill-buffer (find-file-noselect (org-roam-node-file it))) nodes)
;;          (--map (org-roam-with-file (org-roam-node-file it) t
;;                   (goto-char (org-roam-node-point it))
;;                   (point-marker))
;;                 nodes)
;;          (elp-results))
