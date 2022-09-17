;; TODO: The input can be
;; - A valid query to pass to `org-roam-db-query'
;; - If just a :where syntax, append the [:select * :from nodes] to begining and run the query
;; - A list of nodes itself.
;; - A function that returns a list of nodes
(defun org-roam-ql-view--get-nodes-from-query (source-or-query)
  "Convert SOURCE-OR-QUERY to org-roam-nodes.
SOURCE-OR-QUERY can be one of the following:
- A list of params that can be passed to `org-roam-db-query'. Expected
  to have the form (QUERY ARG1 ARG2 ARG3...). `org-roam-db-query' will
  called with the list or parameters as:
  (org-roam-db-query QUERY ARG1 ARG2 ARG3...). The first element in each
  row in the result from the query is expected to have the ID of a
  corresponding node, which will be conerted to a org-roam-node. QUERY
  can be a complete query. If the query is going to be of the form
  [:select [id] :from nodes :where (= todo \"TODO\")], you can omit the
  part till after :where. i.e., pass only [(= todo \"TODO\")] and the
  rest will get appended in the front.
- A list of org-roam-nodes
- A function that returns a list of org-roam-nodes"
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
    

(defun org-roam-ql-view (source-or-query title &optional super-groups)
  "Basically what `org-ql-search does', but for org-roam-nodes.
See `org-roam-ql-view--get-nodes-from-querySOURCE-OR-QUERY' for what
SOURCE-OR-QUERY can be. TITLE is a title to associate with the view.
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


;; ;; modified version of org-ql-view--display
;; ;; note needed.
;; (cl-defun org-roam-ql-view--display (&key (buffer org-ql-view-buffer) header string)
;;   "Display STRING in `org-ql-view' BUFFER.

;; BUFFER may be a buffer, or a string naming a buffer, which is
;; reused if it already exists.  `org-ql-view-buffer' is used by
;; default.

;; HEADER is a string displayed in the buffer's header line.

;; The following special variables, if non-nil, are set
;; buffer-locally to preserve their value in the buffer for
;; subsequent refreshing of the buffer: `org-ql-view-buffers-files',
;; `org-ql-view-query', `org-ql-view-sort', `org-ql-view-narrow',
;; `org-ql-view-super-groups', `org-ql-title.'"
;;   (declare (indent defun))
;;   (let* ((vars (list 'org-ql-view-buffers-files 'org-ql-view-query
;;                      'org-ql-view-sort 'org-ql-view-narrow
;;                      'org-ql-view-super-groups 'org-ql-view-title))
;;          ;; Save the values of variables which are set buffer-locally in the
;;          ;; results buffer, which we want to override and set buffer-locally again.
;;          (vals (cl-loop for symbol in vars
;;                         collect (cons symbol (symbol-value symbol))))
;;          (buffer (if (bufferp buffer)
;;                      buffer
;;                    (with-current-buffer (get-buffer-create (or buffer "*org-roam-ql-buffer*"))
;;                      (unless (eq major-mode 'org-agenda-mode)
;;                        (org-agenda-mode)
;;                        (setf buffer-read-only t))
;;                      (current-buffer)))))
;;     (with-current-buffer buffer
;;       (setq-local bookmark-make-record-function #'org-ql-view-bookmark-make-record)
;;       (use-local-map org-ql-view-map)
;;       ;; Prepare buffer, saving data for refreshing.
;;       (cl-loop for symbol in vars
;;                do (progn
;;                     (kill-local-variable symbol)
;;                     (set (make-local-variable symbol) (alist-get symbol vals nil nil #'equal))))
;;       (setf header-line-format header)
;;       ;; Clear buffer, insert entries, etc.
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert string)
;;         (pop-to-buffer (current-buffer));;org-ql-view-display-buffer-action)
;;         (org-agenda-finalize)
;;         (goto-char (point-min))))))

(defmacro with-plain-file (file keep-buf-p &rest body)
  "Same as `org-roam-with-file', but doesn't start `org-roam'."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (auto-mode-alist nil)
          (find-file-hook nil)
          (buf (or
                (and (not ,file)
                     (current-buffer)) ;If FILE is nil, use current buffer
                (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                (progn
                  (setq new-buf t)
                  (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
         (when (find-buffer-visiting ,file)
           (kill-buffer (find-buffer-visiting ,file))))
     res))

;; modified org-ql-view--format-element to work with org-roam nodes
(defun org-roam-ql-view--format-node (node)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return NODE as a string with text-properties set by its property list.
If NODE is nil, return an empty string."
  (if (not node)
      ""
    (let* ((marker
            (org-roam-with-file (org-roam-node-file node) t
            ;; (with-current-buffer (find-file-noselect (org-roam-node-file node))
            ;; (with-plain-file (org-roam-node-file node) t
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

(defun org-roam-ql--get-file-marker (node)
  ;; (org-roam-with-file (org-roam-node-file node) t
  ;; (with-current-buffer (find-file-noselect (org-roam-node-file node))
  (with-plain-file (org-roam-node-file node) t
    (goto-char (org-roam-node-point node))
    (point-marker)))

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

;; (straight-use-package 'org)
;; (straight-use-package 'org-roam)

;; (defun run-elp (func sources)
;;   "Instrument org and FUNC and iterate on SOURCES with FUNC.
;; FUNC is a sumbol representing a function that takes one parameter.
;; SOURCES is a list of element that will be processed by FUNC"
;;   (elp-instrument-package "org")
;;   (elp-instrument-function func)
;;   (elp-reset-all)
;;   (mapcar func sources)
;;   (elp-results))

;; (defmacro with-plain-file (file keep-buf-p &rest body)
;;   "Same as `org-roam-with-file', but doesn't start `org-roam'."
;;   (declare (indent 2) (debug t))
;;   `(let* (new-buf
;;           (auto-mode-alist nil)
;;           (find-file-hook nil)
;;           (buf (or
;;                 (and (not ,file)
;;                      (current-buffer)) ;If FILE is nil, use current buffer
;;                 (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
;;                 (progn
;;                   (setq new-buf t)
;;                   (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
;;           res)
;;      (with-current-buffer buf
;;        (setq res (progn ,@body))
;;        (unless (and new-buf (not ,keep-buf-p))
;;          (save-buffer)))
;;      (if (and new-buf (not ,keep-buf-p))
;;          (when (find-buffer-visiting ,file)
;;            (kill-buffer (find-buffer-visiting ,file))))
;;      res))

;; (defun test-org-load-files (func &optional restart)
;;   (let ((test-dir "~/temp/org-mode-test/")
;;         files)
;;     (message "Tests running")
;;     (when (and (file-exists-p test-dir) restart)
;;       (dolist (f (directory-files (file-truename test-dir))) (unless (member f '("." "..")) (delete-file f)))
;;       (delete-directory (file-truename test-dir) t))

;;     (if (or restart (not (file-exists-p test-dir)))
;;         (progn
;;           (make-directory (file-truename test-dir))
;;           ;; generating a bunch of file for testing
;;           (dolist (num (number-sequence 1 25 1))
;;             (let ((auto-mode-alist nil)
;;                   (find-file-hook nil)
;;                   (id (org-id-new))
;;                   (f (file-truename (format "~/temp/org-roam-test/test_%s.org" num))))
;;               (push f files)
;;               (with-current-buffer (find-file-noselect f)
;;                 (erase-buffer)
;;                 (insert (format "* This is the heading in file number %s
;;   :PROPERTIES:
;;   :ID:       %s
;;   :TEST_PROP_1: %s
;;   :TEST_PROP_2: id:%s
;;   :END:" num id num id))
;;                 (save-buffer)
;;                 (kill-buffer (find-buffer-visiting f))))))
;;       (progn
;;         (mapcar (lambda (f) (let ((f (find-buffer-visiting f)))
;;                               (em f)
;;                               (when f
;;                                 (kill-buffer f))))
;;                 (setq files (f-glob "*.org" test-dir)))))

;;     (run-elp func files)
;;     (with-current-buffer "*ELP Profiling Results*"
;;       (write-file (format "~/elp_results_%s" func (format-time-string "%Y-%m-%dT%H-%M-%S%-z"))))))

;; (defun --test-org-roam-with-file (f)
;;   (org-roam-with-file f t
;;     (goto-char 3)
;;     (point-marker)))

;; (defun --test-with-current-buffer (f)
;;   (with-current-buffer (find-file-noselect f)
;;     (goto-char 3)
;;     (point-marker)))

;; (defun --test-with-plain-file (f)
;;   (with-plain-file f t
;;     (goto-char 3)
;;     (point-marker)))

;; (setq org-roam-directory (file-truename "~/temp/org-mode-test/"))
;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;; (org-roam-db-autosync-mode)

;; (with-eval-after-load 'org-roam
;;   ;; running twice to so that the first time around module loading won't effect times
;;   (dolist (func '(--test-org-roam-with-file
;;                   --test-with-current-buffer
;;                   --test-with-plain-file))
;;     (test-org-load-files func t))

;;   (dolist (func '(--test-org-roam-with-file
;;                   --test-with-current-buffer
;;                   --test-with-plain-file))
;;     (test-org-load-files func t)))

;; ;; (let ((nodes (--map (org-roam-node-from-id it) (s-split " " "e42d7cc2-5585-4bea-adfa-e9f918fc329b 18bdf45a-de72-40f3-952f-7b4f510c932d 02425145-cb5b-4c9f-b07d-b6f50a87d27d fe1726ca-cd60-4c78-b08a-06d6077534b5 91fe465d-ec95-4bc7-989f-c3e3c239455d 9422cbaa-d5b3-4972-84a3-17dca1cd0590 c47a94e1-746f-4822-91d7-c04bff6dcda3 4942c3f0-663e-4471-8eb7-3bbde01085d4 43518aa3-eee2-44fd-9896-80006ec90912 e7cda305-c046-4960-8654-3459958c9af9 92f21d48-4e7c-4af3-93d6-4897bc8aff5a 34543bc3-ea30-4d50-9ead-323264222f13 f1f4874d-8d6e-41e8-83dc-88d2921c9a6b 7bef4e30-d446-459f-ac8a-c9742a0273f9 f98853d3-8bc6-4fc3-81c3-112491f379e5 de946bf5-6cee-48aa-8e02-90a1b3cd15b6 5866db42-3865-451b-8dd4-07e7f82b2440 79935522-6450-413e-86dc-bbe8303df31c d7543d1e-c3fe-4fdd-9833-840706770737 9214e238-ef31-428f-b656-641308cebaca 15171045-0ea4-4502-9b32-fac8dbf5c9af e94b762a-33ef-4d28-9296-f1681db44e7b 1bfc79ba-7c33-474b-b506-0719f6ee2cff b9a19da6-c155-44f2-8dbd-cca22282e375 41dc836f-59cc-4771-a6fa-9904247d0b0f e5181a37-1442-45bd-b72f-dd86be2aa5d1 e122235d-e1ac-4397-ab8f-9848c476dcbc e9176998-8c39-417c-82b2-a1331aa77eb6 7842d6c0-3e21-47df-ab03-83c45ff797f8 c0953cb5-04cd-4b7a-9210-f9329c5d6411 da6941ef-2f83-4b89-b33c-e3d0085d77b0 c6c6d814-3954-4b38-9c98-23a93f0b840f 3d40d071-e060-4dba-8a7e-aa14e1ab559c 222aea17-ceb9-4dae-819a-3f2c59b1bbe8 5455d8bf-bf11-4053-8ba7-373e500ad98d cbf2ac3c-cd04-4b3f-8e4a-98db21605629 9f42414e-9e84-4abc-8bcb-c1b231387aad 5e1e149a-3b40-4aea-ae28-1426529aac27 d39f0a87-c6b2-4680-9025-645d5f0c05bf 8e2d8536-bf88-4591-8902-4b6c426f9ae0 1b1f47cd-6fc1-4284-a27b-722bd9d09dd7 d36c4ed4-fdbb-48f2-9beb-e185374bc381 65b325e7-ddc8-4c54-aeb5-1b1bb12b1783 cd9e1767-649a-4443-bb25-bd2cdbffdb4f 67c8a6ba-4c9b-40d4-9bec-baba8b851e63 55694ac3-8e1b-4744-8daa-323cf60f4184 6af67dd2-27de-4065-ad6f-00d0258c99d9 03e9b65d-311f-4826-b85a-e15f0ae4210e fe9012b8-81f3-4637-bff1-54a233f745f2 6d80574c-8719-4a95-a639-5a264620b3ea 0a9a28aa-861d-4dfd-b324-b94a172041b6 8eb12e55-2d49-4056-82eb-b305117fd57b cbaa80f8-a59a-40ce-95b9-6877e3d3ee9d d0141ca9-0552-4476-bc99-eb3d465754ae 5d365dee-fede-42b9-8894-c12baf907df3"))))
;; ;;          (--map (when-let (buf (find-buffer-visiting (org-roam-node-file it))) (kill-buffer buf)) nodes)
;; ;;          (run-elp 'org-roam-ql-view--format-node nodes))
