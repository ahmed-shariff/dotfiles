;;; llm                     -*- lexical-binding: t; -*-

;;; Commentary:
;;my custom org setup
;;; Code:

(require 'gptel)
(require 'gptel-integrations)
(require 'gptel-org)

;;;; Other packages ************************************************************************
(use-package gptel-openai-assistant
  :after gptel
  :straight (gptel-openai-assistant :type git :host github :repo "ahmed-shariff/gptel-openai-assistant")
  :config
  (setf gptel-openai-assistant-assistant-id (gethash 'openai-assistant-id configurations))

  (defun amsha/gptel--replace-file-id-with-cite (start end)
    "Updating annotations strings."
    (save-excursion
      (goto-char start)
      (let ((nodes (--map
                    (cons
                     (alist-get "OPENAI_FILE_ID" (org-roam-node-properties it) nil nil #'equal)
                     (cons (alist-get "CUSTOM_ID" (org-roam-node-properties it) nil nil #'equal)
                           (org-roam-node-id it)))
                    (org-roam-ql-nodes `(properties "OPENAI_FILE_ID" ".+")))))
        (condition-case err
            (while (re-search-forward "\\[file_citation:\\([a-zA-Z0-9\\-]*\\)\\]" end)
              (when-let* ((elt (alist-get (match-string 1) nodes nil nil #'equal))
                          (newtext (format " [cite:%s]" (car elt))))
                (setq end (+ end (- (length newtext) (length (match-string 0)))))
                (replace-match newtext)))
          (search-failed nil)))))
  ;; (add-to-list 'gptel-post-response-functions #'gptel-openai-assistant-replace-annotations-with-filename)
  (setf (alist-get "openai-assistant" gptel--known-backends
             nil nil #'equal)
        (gptel-make-openai-assistant "openai-assistant" :key (gptel--get-api-key)))

  (add-to-list 'gptel-post-response-functions #'amsha/gptel--replace-file-id-with-cite)

  )

(use-package mcp
  :straight (mcp :type git :host github :repo "lizqwerscott/mcp.el"
                 :files (:defaults "mcp-hub")))

(use-package mcp-hub
  :after mcp
  :ensure nil
  :straight nil
  :config
  (setq mcp-hub-servers
        '(("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))
          ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "~/temp"))))))

(use-package gptel-mcp
  :ensure t
  :straight (gptel-mcp :type git :host github :repo "lizqwerscott/gptel-mcp.el")
  ;; :bind (:map gptel-mode-map
  ;;             ("C-c m" . gptel-mcp-dispatch)))
  )

;;;; Tool use ******************************************************************************
(gptel-make-tool
 :function (lambda (url)
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min)) (forward-paragraph)
               (let ((dom (libxml-parse-html-region (point) (point-max))))
                 (run-at-time 0 nil #'kill-buffer (current-buffer))
                 (with-temp-buffer
                   (shr-insert-document dom)
                   (buffer-substring-no-properties (point-min) (point-max))))))
 :name "read_url"
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
               :type "string"
               :description "The URL to read"))
 :category "web")

(gptel-make-tool
 :function (lambda (buffer text)
             (with-current-buffer (get-buffer-create buffer)
               (save-excursion
                 (goto-char (point-max))
                 (insert text)))
             (format "Appended text to buffer %s" buffer))
 :name "append_to_buffer"
 :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to append text to.")
             '(:name "text"
               :type "string"
               :description "The text to append to the buffer."))
 :category "emacs")

;; Message buffer logging tool
(gptel-make-tool
 :function (lambda (text)
             (message "%s" text)
             (format "Message sent: %s" text))
 :name "echo_message"
 :description "Send a message to the *Messages* buffer"
 :args (list '(:name "text"
               :type "string"
               :description "The text to send to the messages buffer"))
 :category "emacs")

;; buffer retrieval tool
(gptel-make-tool
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Error: buffer %s is not live." buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :name "read_buffer"
 :description "Return the contents of an Emacs buffer"
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer whose contents are to be retrieved"))
 :category "emacs")


(gptel-make-tool
 :function (lambda (directory)
	     (mapconcat #'identity
                        (directory-files directory)
                        "\n"))
 :name "list_directory"
 :description "List the contents of a given directory"
 :args (list '(:name "directory"
	       :type "string"
	       :description "The path to the directory to list"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (parent name)
             (condition-case nil
                 (progn
                   (make-directory (expand-file-name name parent) t)
                   (format "Directory %s created/verified in %s" name parent))
               (error (format "Error creating directory %s in %s" name parent))))
 :name "make_directory"
 :description "Create a new directory with the given name in the specified parent directory"
 :args (list '(:name "parent"
	       :type "string"
	       :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name"
	       :type "string"
	       :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (path filename content)
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :name "create_file"
 :description "Create a new file with the specified content"
 :args (list '(:name "path"
	       :type "string"
	       :description "The directory where to create the file")
             '(:name "filename"
	       :type "string"
	       :description "The name of the file to create")
             '(:name "content"
	       :type "string"
	       :description "The content to write to the file"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (filepath)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name filepath))
	       (buffer-string)))
 :name "read_file"
 :description "Read and display the contents of a file"
 :args (list '(:name "filepath"
	       :type "string"
	       :description "Path to the file to read.  Supports relative paths and ~."))
 :category "filesystem")

(gptel-make-tool
 :function #'okm-get-pdf-txt
 :name "paper_full_text"
 :description "Get the full text extracted using pdf-tools for a given paper"
 :args (list '(:name "paper_id"
               :type "string"
               :description "The id of the paper. Would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
 :category "okm")

(gptel-make-tool
 :function #'okm-gptel-get-paper-abstract-summary
 :name "paper_abstract_and_summary"
 :description "Get the abstract and summary of the paper. The abstarct is extracted from the paper. The summary is an LLM summary of the paper."
 :args (list '(:name "paper_id"
               :type "string"
               :description "The id of the paper. Would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
 :category "okm"
 :async t)

;;;; Presets *******************************************************************************

(gptel-make-preset 'default
  :description "Preset with defaults"
  :backend "ChatGPT"
  :model 'o4-mini
  :system 'default
  :tools nil
  :stream t
  :temperature 1.0
  :use-context 'system
  :include-reasoning t)

(gptel-make-preset 'openai-assistant
  :description "Search using openai assistant"
  :backend "openai-assistant"
  :model 'o4-mini)

(gptel-make-preset 'search-mini
  :description "Search using gpt-4o-mini-search-preview"
  :backend "ChatGPT"
  :model 'gpt-4o-mini-search-preview
  :temperature nil)

(gptel-make-preset 'search
  :description "Search using gpt-4o-search-preview"
  :backend "ChatGPT"
  :model 'gpt-4o-search-preview
  :temperature nil)

(gptel-make-preset 'g41
  :description "Search using gpt-4.1"
  :model 'gpt-4.1)

(gptel-make-preset 'g41m
  :description "Search using gpt-4.1-mini"
  :model 'gpt-4.1-mini)

(gptel-make-preset 'readurl
  :description "Tool: read url"
  :tools '("read_url"))

(gptel-make-preset 'notools
  :description "no tools"
  :tools nil)
;;;; misc-functions ************************************************************************
(defun gptel-extensions--modeline ()
  "Add modelines showing current model."
  (propertize (format "🧠 %s-%s "
                      (gptel-backend-name gptel-backend)
                      (gptel--model-name gptel-model))
              'face '(:foreground "gray80" :background "#000033")))

(defun amsha/gptel-get-bib-entry-from-citation (citation)
  "Function to take in a citation and turn that into a bib entry."
  (interactive "sCitation: ")
  (let ((buf
         (get-buffer-create (format "*gptel-cite-%s*" (gensym))))
        author-check title-check)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (markdown-mode)
      (gptel-mode)
      (insert citation "\n\n")
      (gptel-request (format "Extract the bib entry from the following citation:\n%s" citation)
        :callback (lambda (response info)
                    (goto-char (point-max))
                    (insert response)
                    (goto-char (point-min))
                    (search-forward-regexp "author *= *{\\(.*\\)}" nil t)
                    (setq author-check (--all-p (string-match it citation) (s-split " and " (match-string 1))))
                    (search-forward-regexp "title *= *{\\(.*\\)}" nil t)
                    (setq title-check (not (null (string-match (match-string 1) citation))))
                    (goto-char (point-max))
                    (insert (format "\n\n---------------\nauthor-check: %s\ntitle-check: %s" author-check title-check)))))))

(defvar amsha/explain-grammarly--explanations-cache "~/.emacs.d/.cache/amsha-explain-grammarly--explanations")

;;;###autoload
(defun amsha/explain-grammarly (sentence explanation)
  "Function to take in grammarly error and provide explanation."
  (interactive (list (read-from-minibuffer "Sentence: ")
                     (completing-read "Explanation: "
                                      (when (f-exists-p amsha/explain-grammarly--explanations-cache)
                                        (with-temp-buffer
                                          (insert-file-contents amsha/explain-grammarly--explanations-cache)
                                          (when (length> (buffer-string) 0)
                                            (read (current-buffer))))))))
  (let ((buf
         (get-buffer-create (format "*gptel-grammarly-%s*" (gensym)))))

    (unless (f-exists-p amsha/explain-grammarly--explanations-cache)
      (f-touch amsha/explain-grammarly--explanations-cache))
    (with-temp-file amsha/explain-grammarly--explanations-cache
      (let ((current-content
             (when (f-exists-p amsha/explain-grammarly--explanations-cache)
               (with-temp-buffer
                 (insert-file-contents amsha/explain-grammarly--explanations-cache)
                 (when (length> (buffer-string) 0)
                   (read (current-buffer))))))
            (print-level nil)
            (print-length nil)
            (pp-default-function 'pp-28)
            (fill-column 999))
        (pp (append current-content
                    (unless (--any-p (string= explanation it) current-content)
                      (list explanation)))
            (current-buffer))))

    (switch-to-buffer buf)
    (with-current-buffer buf
      (markdown-mode)
      (gptel-mode)
      (insert (format "For the following sentence \"%s\" grammerly says \"%s\". How should I fix that?"
                      sentence explanation))
      (goto-char (point-max))
      (gptel-send))))

(defvar amsha/gptel-highlight-preset-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o q p") #'amsha/gptel-insert-preset)
    map))

(defun amsha/gptel-insert-preset ()
  "Insert a gptel preset anywhere."
  (interactive)
  (let ((completion-extra-properties `(:annotation-function ,(lambda (comp)
                                                               (concat ": "
                                                                       (map-nested-elt
                                                                        gptel--known-presets
                                                                        `(,(intern-soft comp)
                                                                          :description)))))))
    (insert "@" (string-join (seq-uniq (completing-read-multiple "Preset: " gptel--known-presets nil t)
                                       #'string-equal)
                             " @"))))

(define-minor-mode amsha/gptel-highlight-preset-mode
  "Highlight presets in any buffer"
  :group 'amsha/gptel
  :keymap amsha/gptel-highlight-preset-mode-map
  (let ((keywords '(("\\(@\\([^[:blank:]\n]+\\)\\)"
                           1 (when-let* ((comps (all-completions (match-string 2)
                                                                 gptel--known-presets))
                                         ((member (match-string 2) comps)))
                               '(:box -1 :inherit secondary-selection))
                           prepend))))
    (cond
     (amsha/gptel-highlight-preset-mode
      (font-lock-add-keywords nil keywords t))
     (t
      (font-lock-remove-keywords nil keywords)))))

;; (define-global-minor-mode amsha/global-gptel-highlight-preset-mode
;;   amsha/gptel-highlight-preset-mode
;;   (lambda ()
;;     (amsha/gptel-highlight-preset-mode 1)))

;; (amsha/global-gptel-highlight-preset-mode)

(add-hook 'prog-mode-hook #'amsha/gptel-highlight-preset-mode)
(add-hook 'text-mode-hook #'amsha/gptel-highlight-preset-mode)

(defun amsha/gptel--transform-apply-preset (_fsm)
  "Apply a gptel preset to the buffer depending on the prompt.

If the user prompt ends with @foo, the preset foo is applied."
  (while-let (((looking-back "@\\([^[:blank:]]+\\)\\s-*[[:blank:]\n]*\\s-*"))
              (name (match-string 1))
              (preset (or (gptel-get-preset (intern-soft name))
                          (gptel-get-preset name))))
    (delete-region (match-beginning 0) (match-end 0))
    (gptel--apply-preset (cons name preset)
                         (lambda (sym val)
                           (set (make-local-variable sym) val)))
    (message "Sending request with preset %s applied!"
             (propertize name 'face 'mode-line-emphasis))))

(advice-add 'gptel--transform-apply-preset :after #'amsha/gptel--transform-apply-preset)

;;;; More setup ****************************************************************************
(defvar amsha/gptel--openrouter
  (gptel-make-openai "OpenRouter"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (gethash 'openrouter-apk configurations)                   ;can be a function that returns the key
    :models '(deepseek/deepseek-r1-distill-llama-70b:free)))

(let ((mode-line-string '(:eval (gptel-extensions--modeline))))
  (unless (memql mode-line-string global-mode-string)
    (add-to-list 'global-mode-string mode-line-string)))

(provide 'gptel-extensions)
;;; orgZ.el ends here
