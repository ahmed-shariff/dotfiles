
;; Below is a compact Emacs Lisp implementation that parses a buffer
;; containing the profiling text you showed and generates two Markdown
;; tables (Time / Milliseconds and GC). It creates a new buffer
;; "*perf-md/" with the markdown tables.

;; Usage:
;; - Open the buffer with your raw profiling text.
;; - M-x unity-perf-report-to-markdown
;; - A new buffer /perf-md/ will be created with the markdown output.

(defun unity-perf--short-name (name)
  "Shorten NAME by taking text after the last '.' or '_' if any."
  (let* ((no-gc (if (string-suffix-p ".GC()" name) (substring name 0 (- (length name) 5)) name))
         (last-dot (or (and (string-match "\\." no-gc) (let ((pos (- (length no-gc) (length (replace-regexp-in-string ".*\\." "" no-gc))))) pos)) nil))
         (dot-pos (let ((i (cl-position ?\. no-gc :from-end t)))
                    (if i i -1)))
         (us-pos (let ((i (cl-position ?_ no-gc :from-end t)))
                   (if i i -1)))
         (idx (max dot-pos us-pos)))
    (if (and idx (>= idx 0))
        (substring no-gc (1+ idx))
      no-gc)))

(defun unity-perf--read-section (start)
  "Given point at the header line, parse one section and return plist:
:name, :kind ('time or 'gc), :metrics (alist label->value). Moves point to line after section."
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        name kind simple)
    (cond
     ((string-suffix-p " in Milliseconds" line)
      (setq kind 'time)
      (setq name (string-trim (substring line 0 (- (length line) (length " in Milliseconds"))))))
     ((string-suffix-p " in Undefineds" line)
      (setq kind 'gc)
      (setq name (string-trim (substring line 0 (- (length line) (length " in Undefineds"))))))
     (t
      (error "Unknown section header: %s" line)))
    ;; remove optional ".GC()"
    (when (string-suffix-p ".GC()" name)
      (setq name (substring name 0 (- (length name) 5))))
    (setq simple (unity-perf--short-name name))
    ;; advance to next lines and collect metrics
    (forward-line 1)
    (let ((metrics '())
          (wanted '("Min" "Median" "Max" "Avg" "StdDev" "SampleCount" "Sum")))
      (while (and (not (eobp))
                  (not (string-match-p " in Milliseconds\\| in Undefineds" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
        (let* ((ln (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
               (colon-pos (string-match ":" ln)))
          (when (and colon-pos
                     (not (string-empty-p ln)))
            (let* ((label (string-trim (substring ln 0 colon-pos)))
                   (label-naked (replace-regexp-in-string ":" "" label)))
              (when (member label-naked wanted)
                ;; extract first number (integer or float), and optional trailing "ms"
                (if (string-match "\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\(?:[ \t]*ms\\)?" ln)
                    (let ((val (match-string 1 ln))
                          (has-ms (string-match "ms" ln)))
                      (push (cons label-naked
                                  (if (eq kind 'time)
                                      (concat val " ms")
                                    val))
                            metrics))
                  ;; if no number, ignore
                  )))))
        (forward-line 1))
      (list :name name :simple simple :kind kind :metrics (nreverse metrics)))))

(defun unity-perf--collect-all ()
  "Parse current buffer and return plist with :order (list of simples), :time (hash), :gc (hash).
Each hash maps simple-name -> assoc list of metric->value."
  (goto-char (point-min))
  (let ((order '())
        (time (make-hash-table :test 'equal))
        (gc (make-hash-table :test 'equal)))
    (while (not (eobp))
      (let ((ln (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
        (if (or (string-suffix-p " in Milliseconds" ln)
                (string-suffix-p " in Undefineds" ln))
            (let* ((sec (unity-perf--read-section (point)))
                   (simple (plist-get sec :simple))
                   (kind (plist-get sec :kind))
                   (metrics (plist-get sec :metrics)))
              (unless (member simple order) (push simple order))
              (if (eq kind 'time)
                  (puthash simple metrics time)
                (puthash simple metrics gc)))
          (forward-line 1))))
    (list :order (nreverse order) :time time :gc gc)))

(defun unity-perf--row-string (label cells)
  "Return a pipe-separated row for LABEL and list of cell strings CELLS."
  (concat "| " (format "%-12s" (concat label ":")) " | "
          (mapconcat (lambda (c) (format "%-12s" c)) cells " | ")
          " |"))

;;;###autoload
(defun unity-perf-report-to-markdown ()
  "Parse current buffer (profiling text) and insert markdown tables in a new buffer *perf-md*."
  (interactive)
  (let* ((data (unity-perf--collect-all))
         (order (plist-get data :order))
         (time (plist-get data :time))
         (gc (plist-get data :gc))
         (metrics-order '("Min" "Median" "Max" "Avg" "StdDev" "SampleCount" "Sum")))
    (with-current-buffer (get-buffer-create "*perf-md*")
      (erase-buffer)
      ;; Time table
      (insert "Time:\n")
      ;; header
      (insert "|              | ")
      (insert (mapconcat (lambda (n) (format "%s" n)) order " | "))
      (insert " |\n")
      ;; separator
      (insert "|--------------|" )
      (dolist (_ order) (insert "-------------|"))
      (insert "\n")
      ;; rows
      (dolist (m metrics-order)
        (let ((cells (mapcar (lambda (n)
                               (let ((alist (gethash n time)))
                                 (or (cdr (assoc m alist)) "")))
                             order)))
          (insert (unity-perf--row-string m cells) "\n")))
      (insert "\n\n")
      ;; GC table
      (insert "GC:\n")
      (insert "|              | ")
      (insert (mapconcat (lambda (n) (format "%s" n)) order " | "))
      (insert " |\n")
      (insert "|--------------|")
      (dolist (_ order) (insert "-------------|"))
      (insert "\n")
      (dolist (m metrics-order)
        (let ((cells (mapcar (lambda (n)
                               (let ((alist (gethash n gc)))
                                 (or (cdr (assoc m alist)) "")))
                             order)))
          (insert (unity-perf--row-string m cells) "\n")))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; Provide the command name
(provide 'perf-md-export)
