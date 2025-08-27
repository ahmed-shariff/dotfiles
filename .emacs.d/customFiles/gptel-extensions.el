;;; llm                     -*- lexical-binding: t; -*-

;;; Commentary:
;; gptel and associated setup
;;; Code:

(straight-use-package 'gptel)
(require 'gptel)
(require 'gptel-integrations)
(require 'gptel-org)
(require 'gptel-transient)
(require 'gptel-context)
(require 'gptel-curl)
(require 'f)

;;;; packages ******************************************************************************
(use-package elysium
  :bind (("C-c o q c" . elysium-query))
  :after gptel
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))

(use-package magit-gptcommit
  :straight (:type git :host github :repo "douo/magit-gptcommit" :branch "gptel"
                   :fork (:host github :repo "ahmed-shariff/magit-gptcommit" :branch "add-custom-backend"))
  :demand t
  :after (magit)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-backend 'gptel)
  (magit-gptcommit-prompt "You are an expert programmer writing a Git commit message.
You have carefully reviewed every file diff included in this commit.

First, write a high-level one-line summary of the commit.
- Keep it to a single line, no more than 50 characters
- Use the imperative tense (e.g., 'Add logging' not 'Added logging')
- Ensure the message reflects a clear and cohesive change
- Do not end the summary with a period
- Do not use backticks (`) anywhere in the response

Finally, provide a detail of the changes being made.
The detailed summary should contain, at a high-level, what changes were made, why they were made.

Here's an example diff:
```
modified   gptel-openai.el
@@ -289,8 +289,8 @@ Mutate state INFO with response metadata.
            :stream ,(or gptel-stream :json-false)))
-        (reasoning-model-p ; TODO: Embed this capability in the model's properties
-         (memq gptel-model '(o1 o1-preview o1-mini o3-mini o3 o4-mini))))
+        (reasoning-model-p
+         (gptel--model-capable-p 'reasoning)))
     (when (and gptel-temperature (not reasoning-model-p))
       (plist-put prompts-plist :temperature gptel-temperature))
```

The one-line summary of the above diff would look like:
\"gptel--request-data get reasoning capability from prop\"

Here's an example of the detailed summary that may follow the above one-line summary:
\"The models with reasoning capabilities were hard coded in openai's
`gptel--request-data`. This commit replaces it with
`gptel--model-capable-p` to get the reasoning capabilitie from the
model properties.\"

If there are parts in the detailed summary that I need to provide more details for, include them as follows
\"[TODO: provide reason for X]\", where X is the part that needs more clarification.

The number of charachters in a single line should never exceed 80 charachters.

THE FILE DIFFS:
```
%s
```
Now, write the commit message using this format:
[summary]

[detailed summary]]")
  ;; (magit-gptcommit-gptel-backend amsha/gptel--openrouter)
  ;; (magit-gptcommit-gptel-model 'deepseek/deepseek-r1-distill-llama-70b:free)
  :config

  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup)
  )

(use-package gptel-quick
  :straight (:type git :host github :repo "karthink/gptel-quick")
  :after gptel
  :bind
  (("C-c o q q" . gptel-quick)
   ("C-c o q c" . gptel-quick-check))
  :config
  (setq gptel-quick-backend gptel--openai
        gptel-quick-model 'gpt-4o)
  (defun gptel-quick-check ()
    "Check the region or thing at point with an LLM.

QUERY-TEXT is the text being explained.  COUNT is the approximate
word count of the response."
    (interactive)
    (let ((gptel-quick-system-message
           (lambda (count)
             (format "Is the sentence correct. Explain %d words or fewer." count))))
      (call-interactively #'gptel-quick))))

(use-package evedel
  :after gptel
  :config
  (customize-set-variable 'evedel-empty-tag-query-matches-all nil)
  (defvar evedel-keymap nil)
  (define-prefix-command 'evedel-keymap)
  (define-key evedel-keymap (kbd "r") #'evedel-create-reference)
  (define-key evedel-keymap (kbd "d") #'evedel-create-directive)
  (define-key evedel-keymap (kbd "s") #'evedel-save-instructions)
  (define-key evedel-keymap (kbd "l") #'evedel-load-instructions)
  (define-key evedel-keymap (kbd "p") #'evedel-process-directives)
  (define-key evedel-keymap (kbd "m") #'evedel-modify-directive)
  (define-key evedel-keymap (kbd "C") #'evedel-modify-reference-commentary)
  (define-key evedel-keymap (kbd "x") #'evedel-delete-instructions)
  (define-key evedel-keymap (kbd "c") #'evedel-convert-instructions)
  (define-key evedel-keymap (kbd "j") #'evedel-next-instruction)
  (define-key evedel-keymap (kbd "k") #'evedel-previous-instruction)
  (define-key evedel-keymap (kbd "J") #'evedel-cycle-instructions-at-point)
  (define-key evedel-keymap (kbd "t") #'evedel-add-tags)
  (define-key evedel-keymap (kbd "T") #'evedel-remove-tags)
  (define-key evedel-keymap (kbd "D") #'evedel-modify-directive-tag-query)
  (define-key evedel-keymap (kbd "P") #'evedel-preview-directive-prompt)
  (define-key evedel-keymap (kbd "/") #'evedel-directive-undo)
  (define-key evedel-keymap (kbd "?") #'(lambda ()
                                                (interactive)
                                                (evedel-directive-undo t)))
  (repeatize 'evedel-keymap)
  (define-key global-map (kbd "C-c o e") evedel-keymap))

(use-package gptel-openai-assistant
  :after gptel
  :straight (gptel-openai-assistant :type git :host github :repo "ahmed-shariff/gptel-openai-assistant")
  :config
  (setf gptel-openai-assistant-assistant-id (gethash 'openai-assistant-id configurations))

  (setf (alist-get "openai-assistant" gptel--known-backends
                   nil nil #'string-equal)
        (gptel-make-openai-assistant "openai-assistant" :key (gptel--get-api-key)))
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

(use-package macher
  :straight (macher :type git :host github :repo "kmontag/macher")

  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'org)

  :config
  ;; Adjust buffer positioning to taste.
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . bottom)))
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher-patch:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . right)))
  (macher-install)
  )


;;;; openai reponse ************************************************************************
(cl-defstruct (gptel-openai-responses
               (:constructor gptel-openai--make-responses)
               (:copier nil)
               (:include gptel-openai)))

(cl-defun gptel-make-openai-responses
    (name &key curl-args models stream key request-params
          (header
           (lambda () (when-let* ((key (gptel--get-api-key)))
                   `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.openai.com")
          (protocol "https")
          (endpoint "/v1/responses"))
  "Create a openai responses backend."
  (declare (indent 1))
  (let ((backend (gptel-openai--make-responses
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
                  backend))))

(defun gptel-openai-response--process-output (output-item info)
  ;; both streams output_item.done.item and the output-item in response are the same.
  (pcase (plist-get output-item :type)
    ("function_call"
     (gptel--inject-prompt        ; First add the tool call to the prompts list
      (plist-get info :backend)
      (plist-get info :data)
      (copy-sequence output-item)) ;; copying to avoid following changes effect output-item
     (ignore-errors (plist-put output-item :args
                               (gptel--json-read-string
                                (plist-get output-item :arguments))))
     (plist-put output-item :arguments nil)
     (plist-put info :tool-use
                (append
                 (plist-get info :tool-use)
                 (list output-item))))
    ("reasoning"
     (gptel--inject-prompt        ; responses expects the reasoning blocks
      (plist-get info :backend) (plist-get info :data) output-item)
     (plist-put info :reasoning
                (append
                 (plist-get info :reasoning)
                 (list (map-nested-elt output-item '(:summary :text))))))
    ("file_search_call"
     (plist-put info :file-search-call-queries (plist-get output-item :queries))
     (plist-put info :file-search-call-results (plist-get output-item :results)))
    (_ ;; TODO handle others
     )))

(defun gptel-openai-response--process-annotation (annotation info)
  "Returns string that can be added to content or nil."
  (pcase (plist-get annotation :type)
    ("file_citation"
     (format "[file_citation:%s]" (plist-get annotation :file_id)))
    (_ ;; TODO handle otheres
     )))

(defcustom gptel-openai-response-inlcude-file-search-results t
  "Include file-search-results?"
  :type 'boolean)

(cl-defmethod gptel--request-data ((backend gptel-openai-responses) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let* ((prompts (cl-call-next-method))
         (p prompts))
    (when gptel-openai-response-inlcude-file-search-results
      (plist-put prompts :include (vconcat (plist-get prompts :include)
                                          '("file_search_call.results"))))
    ;; Adding built-in tools
    (when gptel-openai-responses--tools
      (plist-put prompts :tools (vconcat (plist-get prompts :tools)
                                         (mapcar (lambda (built-in-tool)
                                                   (if (functionp built-in-tool)
                                                       (funcall built-in-tool)
                                                     built-in-tool))
                                                 gptel-openai-responses--tools))))
    (while p
      (when (eq (car p) :messages)
        (setcar p :input))
      (setq p (cddr p)))
    prompts))

(cl-defmethod gptel--inject-prompt
    ((backend gptel-openai-responses) data new-prompt &optional _position)
  "JSON encode PROMPTS for sending to ChatGPT."
  (when (keywordp (car-safe new-prompt)) ;Is new-prompt one or many?
    (setq new-prompt (list new-prompt)))
  (let ((prompts (plist-get data :input)))
    (plist-put data :input (vconcat prompts new-prompt))))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai-responses) info)
  "Parse an OpenAI API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let* ((content-strs))
    (condition-case err
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (let ((json-response (save-excursion
                                   (gptel--json-read))))
              (pcase (plist-get json-response :type)
                ;; ("response.completed"
                ;;  ;; Once stream end processing
                ;;  )
                ("response.output_text.delta"
                 (push (plist-get json-response :delta) content-strs))
                ("response.output_text.annotation.added"
                 (let ((annotation (plist-get json-response :annotation)))
                   (push (gptel-openai-response--process-annotation annotation info) content-strs)))
                ("response.output_item.done"
                 (let ((output-item (plist-get json-response :item)))
                   (gptel-openai-response--process-output output-item info)))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-openai-responses) response info)
  "Parse an OpenAI (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata."
  (plist-put info :stop-reason
             (list (plist-get response :status)
                              (plist-get response :incomplete_details)))
  (plist-put info :output-tokens
             (map-nested-elt response '(:usage :total_tokens)))

  (cl-loop for output-item across (plist-get response :output)
           if (equal (plist-get output-item :type) "message")
             collect
             (string-join
              (list (map-nested-elt output-item '(:content 0 :text))
                    (string-join
                     (mapcar (lambda (annotation)
                               (gptel-openai-response--process-annotation annotation info))
                             (map-nested-elt output-item '(:content 0 :annotations)))
                     "\n"))
              "\n")
             into return-val
           else
             do (gptel-openai-response--process-output output-item info)
           finally return (funcall #'string-join return-val)))

(cl-defmethod gptel--parse-tool-results ((_backend gptel-openai-responses) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  (mapcar
   (lambda (tool-call)
     (list
      :type "function_call_output"
      :call_id (plist-get tool-call :call_id)
      :output (plist-get tool-call :result)))
   tool-use))

(setq gptel-openai-response-backend (gptel-make-openai-responses "ChatGPT-response" :key gptel-api-key :models gptel--openai-models :stream t))

(defun test-response ()
  (let ((gptel-backend gptel-openai-response-backend)
        (gptel-model 'gpt-4.1-mini)
        (gptel-tools (list (cdar (alist-get "boo" gptel--known-tools nil nil #'equal)))))
    (gptel-request "What is the temperature in kelowna" :callback (lambda (r i) (print r i)))))

;;;; Supporting built-in tools for responses ***********************************************
(defclass amsha/add-to-list-switch (transient-variable)
  ((target-value :initarg :target-value)
   (target-list  :initarg :target-list)
   (format       :initarg :format      :initform " %k %d")
   ))

(cl-defmethod transient-infix-read ((obj amsha/add-to-list-switch))
  ;;Do nothing
  )

(cl-defmethod transient-infix-set ((obj amsha/add-to-list-switch) _)
  (if (member (oref obj target-value) (symbol-value (oref obj target-list)))
      (set (oref obj target-list)
           (delete (oref obj target-value) (symbol-value (oref obj target-list))))
    (set (oref obj target-list)
         (append (symbol-value (oref obj target-list))
                 (list (oref obj target-value)))))
  (transient-setup))

(cl-defmethod transient-format-description ((obj amsha/add-to-list-switch))
  (propertize (transient--get-description obj) 'face
              (if (member (oref obj target-value) (symbol-value (oref obj target-list)))
                  'transient-value
                'transient-inactive-value)))

(defvar gptel-openai-responses--known-tools '(("web_search_preview" .
                                                 (lambda ()
                                                   (list :type "web_search_preview" :search-context-size "low")))))

(defvar gptel-openai-responses--tools nil)

(transient-define-prefix gptel-openai-response-built-in-tools ()
  [["Built in tools"
    ("wl" "web search (low context)" ""
     :class amsha/add-to-list-switch
     :target-value (:type "web_search_preview")
     :target-list gptel-openai-responses--tools)
    ;; ("wl" "web search (low context)" ""
    ;;  :class amsha/add-to-list-switch
    ;;  :target-value (:type "web_search_preview" :search-context-size "low")
    ;;  :target-list gptel-openai-responses--tools)
    ;; ("wm" "web search (medium context)" ""
    ;;  :class amsha/add-to-list-switch
    ;;  :target-value (:type "web_search_preview" :search-context-size "medium")
    ;;  :target-list gptel-openai-responses--tools)
    ;; ("wh" "web search (high context)" ""
    ;;  :class amsha/add-to-list-switch
    ;;  :target-value (:type "web_search_preview" :search-context-size "high")
    ;;  :target-list gptel-openai-responses--tools)
    ("fo" "File search (org)" ""
     :class amsha/add-to-list-switch
     :target-value (lambda ()
                     `(:type "file_search"
                             :vector_store_ids ,(vconcat (list (gethash 'openai-org-vector-store configurations)))))
     :target-list gptel-openai-responses--tools)
    ""
    ("DEL" "Remove all" (lambda ()
                          (interactive)
                          (setq gptel-openai-responses--tools nil)
                          (transient-setup))
     :transient t
     :if (lambda () gptel-openai-responses--tools))
    ("RET" "Done" transient-quit-one)
    ]])
   ;; ["Actions"
   ;;  ("RET" "Show X list" (lambda ()
   ;;                         (interactive)
   ;;                         (message "my-x-list: %S" my-x-list)))]])

(transient-append-suffix 'gptel-menu '(0 -1)
  [:if (lambda () (gptel-openai-responses-p gptel-backend))
   ""
   (:info
    (lambda ()
      (concat
       "Built-in tools"
       (and gptel-openai-responses--tools
            (concat " (" (propertize (format "%d"
                                             (length gptel-openai-responses--tools))
                                     'face 'warning)
                    ")"))))
    :format "%d" :face transient-heading)
   (gptel-openai-response-built-in-tools :key "T" :description "Select")])

;; (cl-defmethod gptel--parse-tools :around (backend tools)
;;   "Remove tools not of type `gptel-tool'"
;;   (cl-call-next-method backend (seq-filter (lambda (tool) (gptel-tool-p tool)) tools)))

;; (cl-defmethod gptel--parse-tools :around ((backend gptel-openai-responses) tools)
;;   (vconcat (cl-call-next-method backend tools)
;;            (mapcar (lambda (built-in-tool)
;;                      (if (functionp built-in-tool)
;;                          (funcall built-in-tool)
;;                        built-in-tool))
;;                    gptel-openai-responses--tools)))

(defun amsha/gptel-oai-response-insert-file-search-query-and-results ()
  (interactive)
  (if gptel--fsm-last
      (let ((queries (plist-get (gptel-fsm-info gptel--fsm-last) :file-search-call-queries))
            (results (plist-get (gptel-fsm-info gptel--fsm-last) :file-search-call-results)))
        (insert "\n * Queries:\n- " (string-join queries "\n- "))
        (insert "\n * Results:\n- "
                (string-join (--map (format "id: ~%s~  name: ~%s~  score: ~%s~\n  #+BEGIN_QUOTE\n%s\n  #+END_QUOTE"
                                            (plist-get it :file_id)
                                            (plist-get it :filename)
                                            (plist-get it :score)
                                            (plist-get it :text))
                                    results)
                             "\n\n- ")))
    (user-error "No last fsm.")))

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
  :context--alist nil
  :include-reasoning t
  :prompt-transform-functions `(gptel--transform-apply-preset gptel--transform-add-context))

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

(gptel-make-preset 'nocontext
  :description "no context"
  :context-alist nil)

(gptel-make-preset 'cite-add-abstract-summary
  :description "Add abstract and summary for `cite:`"
  :prompt-transform-functions
  (lambda ()
    (add-before-special-or-append gptel-prompt-transform-functions
                                  #'amsha/okm-gptel-transform-replace-cite-with-abstract-and-summary
                                  #'gptel--transform-add-context)))

(gptel-make-preset 'cite-add-pdf-txt
  :description "Add pdf txt for `cite:`"
  :prompt-transform-functions
  (lambda ()
    (add-before-special-or-append gptel-prompt-transform-functions
                                  #'amsha/okm-gptel-transform-add-pdf-txt
                                  #'gptel--transform-add-context)))

;;;; misc-functions ************************************************************************
(defun add-before-special-or-append (lst elem special)
  "If last element of LST is SPECIAL, add ELEM before it.
Otherwise, add ELEM as the last element."
  (let* ((len (length lst)))
    (cond
     ((and (> len 0) (equal (nth (1- len) lst) special))
      (append (butlast lst) (list elem) (last lst)))
     (t
      (append lst (list elem))))))

(defun gptel-extensions--modeline ()
  "Add modelines showing current model."
  (concat
   (propertize (format "🧠 %s-%s "
                       (gptel-backend-name gptel-backend)
                       (gptel--model-name gptel-model))
               'face '(:foreground "gray80" :background "#000033"))
   (if gptel-context--alist
       (propertize (format "(%s) "
                           (length gptel-context--alist))
                   'face '(:foreground "red3" :background "#000033"))
     "")))

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

(defvar gptel-persistent-context-dir "~/.emacs.d/.cache/gptel-temp-context")

(make-directory gptel-persistent-context-dir t)
(dolist (context-file (directory-files gptel-persistent-context-dir
                                       nil
                                       directory-files-no-dot-files-regexp))
  (gptel-make-preset (intern (format "%s-c" (f-base context-file)))
    :description (format "Use persistent context %s" (f-base context-file))
    :context--alist `((,context-file))))

(defun amsha/gptel--get-persistent-context ()
  (completing-read "Context name: "
                   (-map #'f-base
                         (directory-files gptel-persistent-context-dir
                                          nil
                                          directory-files-no-dot-files-regexp))))

(defun amsha/gptel--get-context-file (context-name)
  (file-truename
   (expand-file-name context-name
                     gptel-persistent-context-dir)))

(defun amsha/gptel-add-region-to-persistent-context (context-name)
  "Adding the region to the persistent context CONTEXT-NAME."
  (interactive (list (amsha/gptel--get-persistent-context)))
  (if (not (use-region-p))
      (user-error "Nor regions highligted")
    (let ((content (buffer-substring (region-beginning) (region-end)))
          (context-file (amsha/gptel--get-context-file context-name)))
      (make-directory gptel-persistent-context-dir t)
      (with-temp-file context-file
        (when (file-exists-p context-file)
          (insert-file-contents context-file))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n---------\n"))
        (insert content))
      (gptel-make-preset (intern (format "%s-c" context-name))
        :description (format "Use persistent context %s" context-name)
        :context--alist `((,context-file))))))

(defun amsha/gptel-clear-persistent-context (context-name)
  "Clear persistent context CONTEXT-NAME."
  (interactive (list (amsha/gptel--get-persistent-context)))
  (let ((context-file (amsha/gptel--get-context-file context-name)))
    (when (y-or-n-p (format "Clear the context %s" context-name))
      (with-temp-file context-file))))

(defun amsha/gptel-visit-persistent-context (context-name)
  "Visit persistent context CONTEXT-NAME."
  (interactive (list (amsha/gptel--get-persistent-context)))
  (let ((context-file (amsha/gptel--get-context-file context-name)))
    (find-file context-file)))

(defun amsha/gptel-delete-persistent-context (context-name)
  "Delete context CONTEXT-NAME."
  (interactive (list (amsha/gptel--get-persistent-context)))
  (let ((context-file (amsha/gptel--get-context-file context-name)))
    (when (y-or-n-p (format "Delete the context file for context %s" context-name))
      (delete-file context-file)
      (setf (alist-get (intern context-name) gptel--known-presets nil 'remove) nil))))

(defun amsha/gptel-add-persistent-context-to-context (context-name)
  "Add persistent-context to context."
  (interactive (list (amsha/gptel--get-persistent-context)))
  (let ((context-file (amsha/gptel--get-context-file context-name)))
    (gptel-context-add-file context-file)))

(transient-append-suffix 'gptel-menu '(0 -1)
  [""
   "Persistent context"
   ("pa" "Append region" amsha/gptel-add-region-to-persistent-context :transient t)
   ("pc" "Clear" amsha/gptel-clear-persistent-context :transient t)
   ("pv" "Visit" amsha/gptel-visit-persistent-context :transient nil)
   ("pd" "Delete" amsha/gptel-delete-persistent-context :transient t)
   ("pA" "Add to context" amsha/gptel-add-persistent-context-to-context :transient t)])

;;; * okm replace cite with context
(defun amsha/okm-gptel-transform-replace-cite-with-abstract-and-summary ()
  "Add respective abstract and summary of  cite:... references."
  (let ((reset 0))
   (-->
    (buffer-substring-no-properties (point-min) (point-max))
    (s-match-strings-all "cite:\\([a-z0-9-_]*\\)" it)
    (--map (cadr it) it)
    (--filter (not (string-empty-p it)) it)
    (-uniq it)
    (--map
     (let ((abs-summary
            (okm-gptel-get-paper-abstract-summary
             (lambda (val)
               (when (> reset 0)
                 (org-roam-db-sync))
               val)
             it)))
       (if (gptel-fsm-p abs-summary)
           (cl-incf reset)
         (with-current-buffer (get-buffer-create (format "*abstract-summary-%s*" it))
           (erase-buffer)
           (insert
            "* cite key:" it "\n\n"
            abs-summary)
           (message "Adding %s to context" it)
           (gptel-context-add))))
     it))
   (when (> reset 0)
     (error "Need to wait for abstracts and summaries"))))

;;; * okm add pdf to context
(defun amsha/okm-gptel-transform-add-pdf-txt ()
  "Add the pdf txt files of cite keys."
  (let ((reset 0))
   (-->
    (buffer-substring-no-properties (point-min) (point-max))
    (s-match-strings-all "cite:\\([a-z0-9-_]*\\)" it)
    (--map (cadr it) it)
    (--filter (not (string-empty-p it)) it)
    (-uniq it)
    (--map
     (gptel-context-add-file
      (file-truename (expand-file-name (format "%s.txt" it) bibtex-completion-library-path)))
     it))
   (when (> reset 0)
     (error "Need to wait for abstracts and summaries"))))

;;; * handline headings in org response
(defun amsha/gptel--replace-* (beg end)
  "Updating annotations strings."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-heading-regexp end t)
        (forward-line 0)
        (delete-char (1+ (length (match-string 1))))
        (insert-and-inherit "*")
        (end-of-line)
        (skip-chars-backward " \t\r")
        (insert-and-inherit "*")))))

(add-to-list 'gptel-post-response-functions #'amsha/gptel--replace-*)

;;; * Function to restore gptel states
(defun amsha/gptel-set-region-as-response ()
  "Set the selected region as a response (add text prop)."
  (interactive)
  (put-text-property
   (region-beginning) (region-end)
   'gptel 'response))

(defun amsha/gptel-set-region-as-user ()
  "Set the selected region as a user text (add text prop)."
  (interactive)
  (put-text-property
   (region-beginning) (region-end)
   'gptel nil))

(defun amsha/gptel--replace-file-id-with-cite (start end)
  "Updating annotations strings."
  (save-excursion
    (goto-char start)
    (let ((nodes (--map
                  (cons
                   (alist-get "OPENAI_FILE_ID" (org-roam-node-properties it) nil nil #'string-equal)
                   (cons (alist-get "CUSTOM_ID" (org-roam-node-properties it) nil nil #'string-equal)
                         (org-roam-node-id it)))
                  (org-roam-ql-nodes `(properties "OPENAI_FILE_ID" ".+")))))
      (condition-case err
          (while (re-search-forward "\\[file_citation:\\([a-zA-Z0-9\\-]*\\)\\]" end)
            (when-let* ((elt (alist-get (match-string 1) nodes nil nil #'string-equal))
                        (newtext (format " [cite:%s]" (car elt))))
              (setq end (+ end (- (length newtext) (length (match-string 0)))))
              (replace-match newtext)))
        (search-failed nil)))))
;; (add-to-list 'gptel-post-response-functions #'gptel-openai-assistant-replace-annotations-with-filename)
(add-to-list 'gptel-post-response-functions #'amsha/gptel--replace-file-id-with-cite)


;;;; setup *********************************************************************************
(bind-keys :package gptel
           ("C-c o q m" . gptel-menu)
           ("C-c o q b" . gptel)
           ("C-c o q Q" . gptel-send)
           :map gptel-mode-map
           ("C-c DEL" . amsha/erase-buffer-with-confirmation))

(setf gptel-use-curl t
      gptel-backend gptel-openai-response-backend
      gptel-model 'gpt-5-mini
      gptel-default-mode #'org-mode

      gptel-org-branching-context t
      gptel-expert-commands t
      (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n"
      (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

;; (put 'o3-mini :request-params '(:reasoning_effort "high" :stream :json-false))

(add-to-list 'yank-excluded-properties 'gptel)

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

(condition-case err
    (setf (gptel-backend-models gptel--openai) (append (gptel-backend-models gptel--openai)
                                                   (--map (prog1 it (put it :capabilities '(reasoning)))
                                                          '(gpt-4o-search-preview gpt-4o-mini-search-preview))))
  (error (message "ERROR %s" err)))

(provide 'gptel-extensions)
;;; gptel-extensions.el ends here
