
(require 'org-roam)

(defvar org-roam-gocal-path "~/Projects/gocal/")
(defvar org-roam-gocal-out-buffer "*gocal-out*")

(defun org-roam-gocal--clear-out-buffer ()
  (-when-let (b (get-buffer org-roam-gocal-out-buffer))
    (with-current-buffer b
      (erase-buffer))))

(defun org-roam-gocal-get ()
  (org-roam-gocal--clear-out-buffer)
  (let ((default-directory org-roam-gocal-path))
    (if (= 0 (call-process "go" nil org-roam-gocal-out-buffer nil "run" "main.go" "get"))
        (--map (json-parse-string it)
               (--filter (not (string-empty-p it))
                         (s-split "\n" (with-current-buffer org-roam-gocal-out-buffer (buffer-string)))))
      (error "failed"))))


(defun org-roam-gocal-put (entry-ht)
  (org-roam-gocal--clear-out-buffer)
  (let ((default-directory org-roam-gocal-path))
    (if (= 0 (call-process "go" nil org-roam-gocal-out-buffer nil "run" "main.go" "put" (em (json-encode entry-ht))))
        (json-parse-string
         (s-trim (with-current-buffer org-roam-gocal-out-buffer (buffer-string))))
      (error (with-current-buffer org-roam-gocal-out-buffer (buffer-string))))))

(provide 'org-roam-gocal)
