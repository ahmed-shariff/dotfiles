(require 'org-roam)

(defvar org-roam-gocal-path "~/Projects/gocal/")
(defvar org-roam-gocal-out-buffer "*gocal-out*")
(defvar org-roam-gocal-new-node-file (f-expand "google_calender_unlisted.org" okm-base-directory))
(defvar org-roam-gocal-ignore-summaries-regexp (rx (seq string-start "Jumma" string-end)))

(defun org-roam-gocal--clear-out-buffer ()
  (-when-let (b (get-buffer org-roam-gocal-out-buffer))
    (with-current-buffer b
      (erase-buffer))))

(defun org-roam-gocal--get ()
  (org-roam-gocal--clear-out-buffer)
  (let ((default-directory org-roam-gocal-path))
    (if (= 0 (call-process "go" nil org-roam-gocal-out-buffer nil "run" "main.go" "get"))
        (--map (json-parse-string it)
               (--filter (not (string-empty-p it))
                         (s-split "\n" (with-current-buffer org-roam-gocal-out-buffer (buffer-string)))))
      (switch-to-buffer org-roam-gocal-out-buffer)
      (error "failed"))))

(defun org-roam-gocal--put (entry-ht)
  (org-roam-gocal--clear-out-buffer)
  (let ((default-directory org-roam-gocal-path))
    (if (= 0 (call-process "go" nil org-roam-gocal-out-buffer nil "run" "main.go" "put" (json-encode entry-ht)))
        (json-parse-string
         (s-trim (with-current-buffer org-roam-gocal-out-buffer (buffer-string))))
      (switch-to-buffer org-roam-gocal-out-buffer)
      (error (with-current-buffer org-roam-gocal-out-buffer (buffer-string))))))

(defun org-roam-gocal--ht->plist (entry-ht)
  (ht->plist entry-ht))

;;;###autoload
(defun org-roam-gocal-pull ()
  (interactive)
  (org-roam-db-sync)
  (->> (org-roam-gocal--get)
       (-map #'org-roam-gocal--ht->plist)
       (--filter 
        (not (s-match org-roam-gocal-ignore-summaries-regexp (org-roam-gocal-get-summary it))))
       (-map #'org-roam-gocal--update-entry))
  (org-roam-db-sync))

;;;###autoload
(defun org-roam-gocal-push ()
  (interactive)
  (org-roam-db-sync)
  (let ((existing-resutls
         (->> (org-roam-gocal--get)
              (-map #'org-roam-gocal--ht->plist)
              (--map (cons (org-roam-gocal-get-id it) it)))))
    (->> (org-roam-ql-nodes '(or (and (deadline > "+0") (deadline < "+45d"))
                                 (and (scheduled > "+0") (scheduled < "+45d"))))
         (--filter (let ((entry (cdr (assoc-string
                                      (cdr (assoc-string "GOCAL_ID" (org-roam-node-properties it)))
                                      existing-resutls
                                      #'string-equal))))
                     (or (not entry)
                         (not (equal (org-roam-node-title it) (org-roam-gocal-get-summary entry)))
                         (not (equal (parse-time-string (or (org-roam-node-scheduled it) (org-roam-node-deadline it)))
                                     (parse-time-string (org-roam-gocal-get-startdatetime entry)))))))
         (-map #'org-roam-gocal--push-node)))
  (org-roam-db-sync))

(defun org-roam-gocal-get-summary (entry-plist)
  (plist-get entry-plist "Summary" #'equal))

(defun org-roam-gocal-get-id (entry-plist)
  (plist-get entry-plist "Id" #'equal))

(defun org-roam-gocal-get-startdatetime (entry-plist)
  (plist-get entry-plist "StartDateTime" #'equal))

(defun org-roam-gocal--update-entry (entry-plist)
  "Either create a new entry in `org-roam-gocal-new-node-file', or update an existing one."
  (let* ((node (car (org-roam-ql-nodes `(properties "GOCAL_ID" ,(org-roam-gocal-get-id entry-plist))))))
    (with-current-buffer (find-file-noselect (if node (org-roam-node-file node) org-roam-gocal-new-node-file))
      (org-with-wide-buffer
       (goto-char (if node (org-roam-node-point node) (point-max)))
       (unless node
         (insert "* ---" ) ;;  gets replaced later
         (org-id-get-create)
         (org-entry-put (point) "GOCAL_ID" (org-roam-gocal-get-id entry-plist)))
       (org-back-to-heading)
       (search-forward-regexp "* " nil t)
       (when (search-forward-regexp org-todo-regexp (pos-eol) t)
         (insert " "))
       (delete-region (point) (pos-eol))
       (insert (org-roam-gocal-get-summary entry-plist))
       (cond
        ((org-roam-node-scheduled node)
         (org-schedule nil (plist-get entry-plist "StartDateTime" #'equal)))
        ((org-roam-node-deadline node)
         (org-deadline nil (plist-get entry-plist "StartDateTime" #'equal)))
        (t
         (org-schedule nil (plist-get entry-plist "StartDateTime" #'equal)))))
      (save-buffer))))

(defun org-roam-gocal--push-node (node)
  ""
  (unless (assoc-string "GOCAL_ID" (org-roam-node-properties node))
    (with-current-buffer (find-file-noselect (org-roam-node-file node))
      (org-with-wide-buffer
       (goto-char (org-roam-node-point node))
       (org-entry-put (point) "GOCAL_ID" (s-replace "-" "" (org-roam-node-id node)))
       (save-buffer)
       (org-roam-db-update-file (buffer-file-name))
       (setq node (org-roam-node-at-point)))))
  (org-roam-gocal--put (ht<-plist (list "EndDateTime" (or (org-roam-node-scheduled node) (org-roam-node-deadline node))
                                        "StartDateTime" (or (org-roam-node-scheduled node) (org-roam-node-deadline node))
                                        "Summary" (org-roam-node-title node)
                                        "Id" (cdr (assoc-string "GOCAL_ID" (org-roam-node-properties node)))))))

(provide 'org-roam-gocal)
