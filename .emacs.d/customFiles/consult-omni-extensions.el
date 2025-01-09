;;; ---     -*- lexical-binding: t; -*-

(require 'consult-omni)
  ;;; Load Sources Core code
(require 'consult-omni-sources)

  ;;; Load Embark Actions
(require 'consult-omni-embark)

  ;;; Custom sources-------------------------
(cl-defun consult-omni--openalex-autosuggest-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from openalex Autosuggest."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count
                (--> (plist-get opts :count)
                     (or (and it (integerp (read it)) (string-to-number it))
                         consult-omni-default-count)))
               (page
                (--> (plist-get opts :page)
                     (or (and (integerp it) it)
                         (and it (string-to-number (format "%s" it)))
                         1)))
               (params `(("q" . ,query))))
    (consult-omni--fetch-url "https://api.openalex.org/autocomplete" consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (response-table)
                               (when-let* ((results (gethash "results" response-table))
                                           (annotated-results
                                            (mapcar (lambda (item)
                                                      (let ((display-name (gethash "display_name" item)))
                                                        (propertize
                                                         (concat (propertize (gethash "publication_year" item) 'face 'consult-omni-keyword-face)
                                                                 (propertize display-name 'face 'consult-omni-default-face))
                                                         :source "Openalex AutoSuggest"
                                                         :title display-name
                                                         :url (gethash "id" item))))
                                                    results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

(consult-omni-define-source "OpenAlex AutoSuggest"
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--openalex-autosuggest-fetch-results
                            :on-preview #'ignore
                            :on-return #'identity
                            :on-callback #'string-trim
                            :on-new #'identity
                            :search-hist 'consult-omni--search-history
                            :select-hist t
                            :group #'consult-omni--group-function
                            :enabled (lambda () t)
                            :sort t
                            :interactive consult-omni-intereactive-commands-type)

(defvar consult-omni--openalex-searchable-types '("works" "authors" "sources" "institutions" "topics" "publishers" "funders"))

(cl-defun consult-omni--openalex-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results got INPUT from openalex API.

See URL `https://docs.openalex.org'"
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input))
               (opts (car-safe opts))
               (count
                (--> (plist-get opts :count)
                     (or (and it (integerp (read it)) (string-to-number it))
                         consult-omni-default-count)))
               (page
                (--> (plist-get opts :page)
                     (or (and (integerp it) it)
                         (and it (string-to-number (format "%s" it)))
                         1)))
               (type
                (if-let* ((maybe-type (--> (plist-get opts :type)
                                           (or (and (not (null it)) it)
                                               "works")))
                          (_ (member maybe-type consult-omni--openalex-searchable-types)))
                    maybe-type
                  (completing-read
                   (format "Unknown type (got %s). Select valid type:" maybe-type)
                   consult-omni--openalex-searchable-types)))
               (api-url (format "https://api.openalex.org/%s" type))
               (params `(("search" . ,query)
                         ("page" . ,page)
                         ("per-page" . ,count))))
    (consult-omni--fetch-url api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (response-table)
                               (when-let* ((results (gethash "results" response-table))
                                           (annotated-results
                                            (mapcar (lambda (item)
                                                      (let ((display-name (gethash "display_name" item)))
                                                        (propertize (propertize display-name 'face 'consult-omni-default-face)
                                                                    :source "Openalex AutoSuggest"
                                                                    :title display-name
                                                                    :url (gethash "id" item))))
                                                    results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

(consult-omni-define-source "OpenAlex"
                            :narrow-char ?o
                            :type 'dynamic
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--openalex-fetch-results
                            :on-new (apply-partially #'consult-omni-external-search-with-engine "DuckDuckGo")
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type)

(defun consult-omni--org-roam-ql-results (query callback)
  "function that returns the results of a query for consult-omni."
  (unless (featurep 'org-roam-ql) (require 'org-roam-ql))
  (let ((results (--map
                  (let ((title (org-roam-node-title it)))
                    (propertize (propertize title 'face 'consult-omni-default-face)
                                :source "org-roam-ql"
                                :title title
                                :node it))
                  (org-roam-ql-nodes query))))
    (when callback (funcall callback results))
    results))

(defun consult-omni--org-roam-node-goto (cand)
  "Go to node of CAND."
  (org-roam-node-open (get-text-property 0 :node cand)))

(cl-defun consult-omni--org-roam-get-nodes (input &rest args &key callback &allow-other-keys)
  "Collects all nodes based on input title"
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input)))
    (consult-omni--org-roam-ql-results `(title ,query) callback)))

(consult-omni-define-source "org-roam nodes"
                            :narrow-char ?o
                            :type 'dynamic
                            :require-match nil
                            :state #'consult-org-roam--node-preview
                            :face 'consult-omni-engine-title-face
                            :category 'org-roam-node
                            :request #'consult-omni--org-roam-get-nodes
                            :on-new #'org-roam-capture
                            :on-callback #'consult-omni--org-roam-node-goto
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type)

(cl-defun consult-omni--org-roam-get-research-papers (input &rest args &key callback &allow-other-keys)
  "Collects research paper nodes based on input title"
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input)))
    (consult-omni--org-roam-ql-results `(and "rp" (title ,query)) callback)))

(consult-omni-define-source "org-roam research papers"
                            :narrow-char ?o
                            :type 'dynamic
                            :require-match nil
                            :state #'consult-org-roam--node-preview
                            :face 'consult-omni-engine-title-face
                            :category 'org-roam-node
                            :request #'consult-omni--org-roam-get-research-papers 
                            :on-new #'org-roam-capture
                            :on-callback #'consult-omni--org-roam-node-goto
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type)
  ;;;----------------------------------------

;; (-non-nil (-flatten (--map (ignore-errors (mapcar (lambda (el) (expand-file-name el it)) (directory-files it nil ".exe"))) (s-split ";" (getenv "path")))))
;; (f-glob "**/**.lnk" (file-truename "~/AppData/Roaming/Microsoft/Windows/Start Menu/Programs"))
;; (f-glob "**.lnk" (file-truename "~/AppData/Roaming/Microsoft/Windows/Start Menu/Programs"))
;; (f-directories (file-truename "~/AppData/Roaming/Microsoft/Windows/Start Menu/Programs") nil t)
(when (eq system-type 'windows-nt)
  (unless (featurep 'f) (require 'f))
  (setq consult-omni-apps-paths (-non-nil
                                 (append (list (file-truename "~/AppData/Roaming/Microsoft/Windows/Start Menu/Programs"))
                                         (f-directories (file-truename "~/AppData/Roaming/Microsoft/Windows/Start Menu/Programs") nil t)
                                         (list (file-truename "C:/ProgramData/Microsoft/Windows/Start Menu/Programs"))
                                         (f-directories (file-truename "C:/ProgramData/Microsoft/Windows/Start Menu/Programs") nil t)
                                         (-map #'file-truename (s-split ";" (getenv "path")))))
        consult-omni-apps-regexp-pattern "\\(.*\\.exe$\\|.*\\.lnk$\\)"
        consult-omni-apps-open-command-args "powershell Start-Process"
        consult-omni-apps-default-launch-function (lambda (app &optional file)
                                                    ;; (let ((consult-omni-apps-open-command-args (format "powershell $env:path='%s';Start-Process" (string-replace "/" "\\" (string-join consult-omni-apps-paths ";")))))
                                                    ;; (em consult-omni-apps-open-command-args)
                                                    ;; (let ((shell-file-name "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
                                                    ;; (consult-omni-apps-open-command-args "Start-Process"));;(format "$env:path='%s';Start-Process" (string-replace "/" "\\" (string-join consult-omni-apps-paths ";")))))
                                                    (consult-omni--apps-launch-app (format "'%s'" app) file))))

  ;;; Optionally add more interactive commands

;; consult-omni-web
(setq consult-omni-web-sources (list "gptel"
                                     ;; "OpenAlex AutoSuggest"
                                     ;; "Brave"
                                     ;; "elfeed"
                                     ;; "mu4e"
                                     ;; "Wikipedia"
                                     ;; "DuckDuckGo API"
                                     ;; "GitHub"
                                     "Invidious"
                                     ))

;;;###autoload
(defun consult-omni-web (&optional initial prompt sources no-callback &rest args)
  "Interactive web search”

This is similar to `consult-omni-multi', but runs the search o n
web sources defined in `consult-omni-web-sources'.
See `consult-omni-multi' for more details.
"
  (interactive "P")
  (let ((prompt (or prompt (concat "[" (propertize "consult-omni-web" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
        (sources (or sources consult-omni-web-sources)))
    (consult-omni-multi-static initial prompt sources no-callback args)))

;; consult-omni-local
(defvar consult-omni-local-sources (list "ripgrep"
                                         ;; "mdfind"
                                         ;; "Notes Search"
                                         "org-roam nodes"
                                         "Apps"
                                         "Org Agenda"))

;;;###autoload
(defun consult-omni-local (&optional initial prompt sources no-callback &rest args)
  "Interactive local search”

This is similar to `consult-omni-multi', but runs the search on
local sources defined in `consult-omni-local-sources'.
See `consult-omni-multi' for more details.
"
  (interactive "P")
  (let ((prompt (or prompt (concat "[" (propertize "consult-omni-local" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
        (sources (or sources consult-omni-local-sources)))
    (consult-omni-multi initial prompt sources no-callback args)))

;; consult-omni-scholar
(setq consult-omni-scholar-sources (list
                                    ;; "PubMed"
                                    ;; "Scopus"
                                    ;; "Notes Search"
                                    "org-roam research papers"
                                    "OpenAlex"))

;;;###autoload
(defun consult-omni-scholar (&optional initial prompt sources no-callback &rest args)
  "Interactive “multi-source acadmic literature” search

This is similar to `consult-omni-multi', but runs the search on
academic literature sources defined in `consult-omni-scholar-sources'.
See `consult-omni-multi' for more details.
"
  (interactive "P")
  (let ((prompt (or prompt (concat "[" (propertize "consult-omni-multi" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
        (sources (or sources consult-omni-scholar-sources)))
    (consult-omni-multi initial prompt sources no-callback args)))

;; AutoSuggest at point
;;;###autoload
(defun consult-omni-autosuggest-at-point ()
  (interactive)
  (let ((input (or (thing-at-point 'url) (thing-at-point 'filename) (thing-at-point 'symbol) (thing-at-point 'sexp) (thing-at-point 'word))))
    (when (and (minibuffer-window-active-p (selected-window))
               (equal (substring input 0 1) (consult--async-split-initial nil)))
      (setq input (substring input 1)))
    (consult-omni-brave-autosuggest input)))

(provide 'consult-omni-extensions)
