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
(require 'gptel-request)
(require 'gptel-rewrite)
(require 'f)

;;; Some top level defaults*****************************************************************
(defvar amsha/gptel-default-prompt-transform-functions gptel-prompt-transform-functions)

;;; packages ******************************************************************************
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

If there are any potentials bug or issues in the changes, include them as follows:
\"FIX:\n X\", where X is the short explanation of the bugs/issues.

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

;; (use-package gptel-openai-assistant
;;   :after gptel
;;   :straight (gptel-openai-assistant :type git :host github :repo "ahmed-shariff/gptel-openai-assistant")
;;   :config
;;   (setf gptel-openai-assistant-assistant-id (gethash 'openai-assistant-id configurations))

;;   (setf (alist-get "openai-assistant" gptel--known-backends
;;                    nil nil #'string-equal)
;;         (gptel-make-openai-assistant "openai-assistant" :key (gptel--get-api-key)))
;;   )

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

(use-package gptel-agent
  :straight (:host github :repo "karthink/gptel-agent"
                   :files (:defaults "agents")) ;use :ensure for Elpaca
  :config
  (add-to-list 'gptel-agent-dirs "~/.emacs.d/customFiles/gptel-paper-agent/")
  (gptel-agent-update)         ;Read files from agents directories

  (when-let* ((paper-agent-plist (assoc-default "paper-agent" gptel-agent--agents nil nil)))
    (apply #'gptel-make-preset 'paper-agent paper-agent-plist))

  (defun amsha/gptel-paper-agent ()
    "Paper agent."
    (interactive)
    (gptel-agent okm-base-directory 'paper-agent)))

;;; openai reponse ************************************************************************
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
    (when-let (tool-defs (plist-get prompts :tools))
      (plist-put prompts :tools
                 (cl-loop for tool-def across tool-defs
                          if (string-equal (plist-get tool-def :type) "function")
                          collect (append '(:type "function") (plist-get tool-def :function))
                          into out
                          else collect tool-def into out
                          finally return (apply #'vector out))))
    ;; TODO: Handle multipart
    (while p
      (when (eq (car p) :messages)
        (setcar p :input)
        (setcar (cdr p)
                (apply #'vector
                       (mapcar (lambda (input-item)
                                 (pcase input-item
                                   ((let (and tool-calls (guard tool-calls)) (plist-get input-item :tool_calls))
                                    (cl-assert (length= tool-calls 1) t "Expected only one value in tool_calls")
                                    (let ((tool-call (aref tool-calls 0)))
                                      `(:type "function_call"
                                              :call_id ,(plist-get tool-call :id)
                                              ,@(plist-get tool-call :function))))
                                  (_ input-item)
                                 ))
                               (cadr p)))))
      (when (memq (car p) '(:max_completion_tokens :max_tokens))
        (setcar p :max_output_tokens))
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
      :call_id (plist-get tool-call :id)
      :output (plist-get tool-call :result)))
   tool-use))

(setq gptel-openai-response-backend (gptel-make-openai-responses "ChatGPT-response" :key gptel-api-key :models gptel--openai-models :stream t))

(defun test-response ()
  (let ((gptel-backend gptel-openai-response-backend)
        (gptel-model 'gpt-5-nano)
        (gptel-tools (list (gptel-make-tool
                            :function (lambda (location unit)
                                        (format "Temp in %s is 25 %s" location unit))
                            :name "get_weather"
                            :description "Get the current weather in a given location"
                            :args (list '(:name "location"
                                                :type string
                                                :description "The city and state, e.g. San Francisco, CA")
                                        '(:name "unit"
                                                :type string
                                                :enum ["celsius" "farenheit"]
                                                :description
                                                "The unit of temperature, either 'celsius' or 'fahrenheit'"
                                                :optional t))))))
    (gptel-request "What is the temperature in kelowna" :callback (lambda (r i) (message "%S\n----\n%S" r i)))))

;;; Supporting built-in tools for responses ***********************************************
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

(defun amsha/gptel-oai-response-insert-file-search-query-and-results (&optional info)
  (interactive)
  (if-let (info (or info
                    (and gptel--fsm-last
                         (gptel-fsm-info gptel--fsm-last))))
      (when-let ((queries (plist-get info :file-search-call-queries))
                 (results (plist-get info :file-search-call-results)))
        (insert "\n * Queries:\n- " (string-join queries "\n- "))
        (insert "\n * Results:\n- "
                (string-join (--map (format "id: ~%s~  name: ~%s~  score: ~%s~\n  #+BEGIN_QUOTE\n%s\n  #+END_QUOTE"
                                            (plist-get it :file_id)
                                            (plist-get it :filename)
                                            (plist-get it :score)
                                            (plist-get it :text))
                                    results)
                             "\n\n- ")))
    (user-error "No last fsm and no info provided.")))

;;; Tool use ******************************************************************************
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

;;;; * paper agent tools
(gptel-make-tool
 :function #'okm-get-pdf-txt
 :name "okm_paper_full_text"
 :description "Get the full text extracted using pdf-tools for a given paper"
 :args (list '(:name "paper_id"
               :type "string"
               :description "The id of the paper. Would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
 :confirm t
 :category "okm")

;; (gptel-make-tool
;;  :function #'okm-gptel-get-paper-abstract-summary
;;  :name "okm_paper_abstract_and_summary"
;;  :description "Get the title, authors, year, abstract and summary of the paper. The abstarct is extracted from the paper. The summary is an LLM summary of the paper."
;;  :args (list '(:name "paper_id"
;;                :type "string"
;;                :description "The id of the paper. Would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
;;  :category "okm"
;;  :async t)

(gptel-make-tool
 :name "okm_get_papers_abstract_summary"
 :function #'okm-gptel-get-papers-abstract-summary
 :description "Given an array of paper IDs, fetch each paper's abstract and a generated summary. Returns a single string containing the concatenated results for all requested papers. For each paper id, the title, authors, year, abstract and summary of the paper are included. The abstarct is extracted from the paper. The summary is an LLM summary of the paper."
 :args (list '(:name "paper_ids"
               :type array
               :items (:type string)
               :description "Array of paper IDs (strings). Each ID would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
 :async t
 :category "okm")

(gptel-make-tool
 :name "okm_oai_query_file_search"
 :function #'okm-oai-query-file-search
 :description "Search the OpenAI vector store for documents matching QUERY, then request a model summary of the retrieved documents. The model summary is returned as a single string: a bullet-list formatted summary with inline citations of the form cite:<paper_id>. On success the tool invokes the provided callback with that summary and also appends a 'Papers queried' list. On failure the callback receives an error message string."
 :args (list '(:name "query"
               :type string
               :description "Search query to run against the OAI vector store"))
 :async t
 :category "okm"
 :include t)

(gptel-make-tool
 :name "okm_pdf_question"
 :function #'okm-pdf-question
 :description "Ask questions about a paper identified by its paper_id. QUESTIONS is a single string, can contain multiple questions. The return value is a string with the answer to the question."
 :args (list '(:name "paper_id"
               :type string
               :description "The id of the paper. Would be something like 'faleel21_hpui' or 'joe12_what_is_inter'.")
             '(:name "questions"
               :type string
               :description "The question(s) to ask about the paper."))
 :async t
 :category "okm"
 :include t)

;; (gptel-make-tool
;;  :name "okm_paper_get_notes"
;;  :function #'okm-paper-get-notes
;;  :description "Return the notes for the paper identified by paper_id. It also include the title, authors, and year of the paper. If the paper with paper_id is not found, will return an error message."
;;  :args (list '(:name "paper_id"
;;                :type string
;;                :description "The id of the paper. Would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
;;  :async t
;;  :category "okm")

(gptel-make-tool
 :name "okm_papers_get_notes"
 :function #'okm-papers-get-notes
 :description "Given an array of paper IDs, return the notes for each paper as a single string. The result is a human-readable concatenation of each paper's notes. For each paper the title, authors, and year of the paper are included."
 :args (list '(:name "paper_ids"
               :type array
               :items (:type string)
               :description "Array of paper IDs (strings). Each ID would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
 :category "okm")

(gptel-make-tool
 :name "okm_get_keywords"
 :function #'okm-gptel-get-keywords
 :description "Return the list of keywords in the database. Takes no arguments and returns a list of keyword strings."
 :args nil
 :category "okm")

(gptel-make-tool
 :name "okm_get_paper_ids_for_keyword"
 :function #'okm-gptel-paper-ids-for-keyword
 :description "Return the list of paper IDs associated with a given keyword."
 :args (list '(:name "keyword"
               :type string
               :description "The keyword to search for"))
 :category "okm"
 :include t)

;; (gptel-make-tool
;;  :name "okm_get_paper_details"
;;  :function #'okm-gptel-get-paper-details
;;  :description "Given a paper_id, return details of the paper formatted for LLM use. Returns a string containing title, authors and year."
;;  :args (list '(:name "custom_id"
;;                :type string
;;                :description "The id of the paper. Would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
;;  :category "okm"
;;  :include t)

(gptel-make-tool
 :name "okm_get_papers_details"
 :function #'okm-gptel-get-papers-details
 :description "Given an array of paper IDs, return bibliographic details (title, year, authors) for each paper as a single formatted string suitable for LLM consumption."
 :args (list '(:name "custom_ids"
               :type array
               :items (:type string)
               :description "Array of paper custom IDs (strings). Each ID would be something like 'faleel21_hpui' or 'joe12_what_is_inter'."))
 :category "okm")

(gptel-make-tool
 :name "okm_write_session_note"
 :function #'okm-gptel-write-session-note
 :description "Write CONTENT to RELATIVE-PATH for the current agent session. This tool will append content to file if it already exists."
 :args (list
        '(:name "relative_path"
          :type string
          :description "Path relative to the session directory, e.g. \"notes/todo.org\"")
        '(:name "content"
          :type string
          :description "Text content to write into the file"))
 :category "okm")

;;;; * skills agent tools
(gptel-make-tool
 :name "skill_progressive_disclosure"
 :function #'gptel-agent-execute-skill
 :description "
Progressive disclosure tool for skills - a unified handler to inspect and interact with installed skills. Supports actions:
- \"summary\": return the skill's frontmatter description,
- \"details\": return the SKILL.md body,
- \"load_resource\": return the contents of a resource file (path relative to the skill dir),
- \"run_script\": execute a script in the skill dir with optional args.

Always first use \"summary\" to understand a skill. Load a skill only
when necessary using \"details\". Use \"load_resources\" and
\"run_script\" for a given skill based on the details only when
necessary."
 :args (list
        '(:name "skill" :type string :description "Skill name")
        '(:name "action" :type string :description "Action"
                :enum ["summary" "details" "load_resource" "run_script"])
        '(:name "path" :type string :optional t :description "Resource or script path relative to skill dir")
        '(:name "args" :type array :optional t :description "Array of string args for scripts"))
 :async t
 :category "skills")

;;; Presets *******************************************************************************

(gptel-make-preset 'default
  :description "Preset with defaults"
  ;; :backend '(:eval amsha/gptel-default-backend)
  :backend gptel-openai-response-backend
  :model 'gpt-5-mini
  :system 'default
  :tools nil
  :openai-responses--tools nil
  :stream t
  :temperature 1.0
  :use-context 'system
  :context nil
  :include-reasoning t
  :prompt-transform-functions '(:eval amsha/gptel-default-prompt-transform-functions))

;; (gptel-make-preset 'openai-assistant
;;   :description "Search using openai assistant"
;;   :backend "openai-assistant"
;;   :model 'o4-mini)

;; (gptel-make-preset 'search-mini
;;   :description "Search using gpt-4o-mini-search-preview"
;;   :backend "ChatGPT"
;;   :model 'gpt-4o-mini-search-preview
;;   :temperature nil)

;; (gptel-make-preset 'search
;;   :description "Search using gpt-4o-search-preview"
;;   :backend "ChatGPT"
;;   :model 'gpt-4o-search-preview
;;   :temperature nil)

;; (gptel-make-preset 'g41
;;   :description "Search using gpt-4.1"
;;   :model 'gpt-4.1)

;; (gptel-make-preset 'g41m
;;   :description "Search using gpt-4.1-mini"
;;   :model 'gpt-4.1-mini)

(gptel-make-preset 'readurl
  :description "Tool: read url"
  :tools '("read_url"))

(gptel-make-preset 'notools
  :description "no tools"
  :tools nil)

(gptel-make-preset 'nocontext
  :description "no context"
  :context nil)

(gptel-make-preset 'filectx
  :description "Current file as context"
  :pre
  (lambda ()
    (gptel-context-add)))

(gptel-make-preset 'cite-add-abstract-summary
  :description "Add abstract and summary for `cite:`"
  :prompt-transform-functions
  '(:eval
    (add-before-special-or-append gptel-prompt-transform-functions
                                  #'amsha/okm-gptel-transform-replace-cite-with-abstract-and-summary
                                  #'gptel--transform-add-context)))

(gptel-make-preset 'cite-add-pdf-txt
  :description "Add pdf txt for `cite:`"
  :prompt-transform-functions
  '(:eval
    (add-before-special-or-append gptel-prompt-transform-functions
                                  #'amsha/okm-gptel-transform-add-pdf-txt
                                  #'gptel--transform-add-context)))

;;; misc-functions ************************************************************************
(defun add-before-special-or-append (lst elem special)
  "If last element of LST is SPECIAL, add ELEM before it.
Otherwise, add ELEM as the last element."
  (let* ((len (length lst)))
    (cond
     ((and (> len 0) (equal (nth (1- len) lst) special))
      (append (butlast lst) (list elem) (last lst)))
     (t
      (append lst (list elem))))))

;;;###autoload
(defun amsha/gptel-yank (arg)
  "Yank without excluding 'gptel prop."
  (interactive "p")
  (let ((yank-excluded-properties (remove 'gptel yank-excluded-properties)))
    (yank arg)))

(defvar amsha/w32-active-notification nil)

(defun amsha/notify-tool-calls (tool-calls info &optional _)
  "On windows, send out notification of tool call confirmation requests."
  (when (eq system-type 'windows-nt)
    (when amsha/w32-active-notification
      (w32-notification-close amsha/w32-active-notification))
    (setq amsha/w32-active-notification
          (w32-notification-notify :title "gptel tool confirmation"
                                   :body (em (format "Awaiting %s tool confirmation in %s"
                                                     (length tool-calls) (plist-get info :buffer)))))))

;;; mode line *****************************************************************************
;; from karthink https://github.com/karthink/gptel/issues/858
(defvar gptel--mode-line-status " ")
(defvar gptel--mode-line-format "")

(defun gptel--mode-line-status (msg &optional face)
  (setq gptel--mode-line-status
        (if face (propertize msg 'face face) msg)))

;; Update according to current state
(defun gptel--mode-line-status-update (fsm)
  (pcase (gptel-fsm-state fsm)
    ('WAIT (gptel--mode-line-status "↑" 'warning))
    ('TYPE (gptel--mode-line-status "↓" 'success))
    ('TOOL (gptel--mode-line-status "🔨" 'warning))
    ('ERRS (gptel--mode-line-status "❌" 'error))
    (_     (gptel--mode-line-status " ")))
  (gptel--update-mode-line))

;; Add to `gptel-send' FSM handlers
(dolist (state '(WAIT TYPE TOOL ERRS DONE))
  (cl-pushnew 'gptel--mode-line-status-update
              (alist-get state gptel-send--handlers)))

;; Optional: Also add to `gptel-request' FSM handlers
;; (i.e. for all gptel commands, not just `gptel-send')
(dolist (state '(WAIT TYPE TOOL ERRS DONE))
  (cl-pushnew 'gptel--mode-line-status-update
              (alist-get state gptel-request--handlers)))

;; Add to `gptel--rewrite-handlers'
(dolist (state '(WAIT TYPE TOOL ERRS DONE))
  (cl-pushnew 'gptel--mode-line-status-update
              (alist-get state gptel--rewrite-handlers)))

(defun gptel--update-mode-line (&rest _)
  "Add modelines showing current model, status and context."
  (setq gptel--mode-line-format
        (concat (propertize gptel--mode-line-status
                            'face `(:inherit ,(or (get-text-property 0 'face gptel--mode-line-status)
                                                  'default)
                                             :background "#000033"))
                (propertize (format "🧠 %s-%s "
                                    (gptel-backend-name gptel-backend)
                                    (gptel--model-name gptel-model))
                            'face '(:foreground "gray80" :background "#000033"))
                (if (or gptel-context gptel-tools gptel-openai-responses--tools)
                    (let ((context-string
                           (when gptel-context
                             (format "C:%s" (length gptel-context))))
                          (tool-string
                           (when (or gptel-tools gptel-openai-responses--tools)
                             (format "T:%s"
                                     (+ (if gptel-tools
                                            (length gptel-tools)
                                          0)
                                        (if gptel-openai-responses--tools
                                            (length gptel-openai-responses--tools)
                                          0))))))
                      (propertize (format "(%s) "
                                          (cond
                                           ((and context-string tool-string)
                                            (concat context-string "/" tool-string))
                                           ((or context-string tool-string)
                                            (or context-string tool-string))
                                           (t "huh")))
                                'face '(:foreground "red3" :background "#000033")))
                  ""))))

(run-with-idle-timer 5 t #'gptel--update-mode-line)

(advice-add 'gptel--infix-provider :after #'gptel--update-mode-line)
(advice-add 'gptel-context--at-point :after #'gptel--update-mode-line)
(advice-add 'gptel-add :after #'gptel--update-mode-line)
(advice-add 'gptel-context-add-current-kill :after #'gptel--update-mode-line)
(advice-add 'gptel-abort :after #'gptel--update-mode-line)

(add-hook 'transient-exit-hook #'gptel--update-mode-line)

(cl-pushnew '((:eval gptel--mode-line-format)) global-mode-string)

;;; okm tools for tool use ****************************************************************
(defun amsha/gptel-get-bib-entry-from-citation (citation)
  "Function to take in a citation and turn that into a bib entry."
  (interactive "sCitation: ")
  (let ((buf
         (get-buffer-create (format "*gptel-cite-%s*" (gensym))))
        (citation (replace-regexp-in-string "\n" " " (replace-regexp-in-string "- " "" citation)))
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

(defun okm-oai-query-file-search (callback query)
  "Given a query, get resutls from oai vector store and summary from oai."
  (let* ((gptel-backend gptel-openai-response-backend)
         (gptel-model 'gpt-5-nano)
         (gptel-tools nil)
         (gptel-openai-responses--tools `((:type "file_search"
                                                :vector_store_ids ,(vconcat (list (gethash 'openai-org-vector-store configurations))))))
         (gptel-context nil)
         ;; (fsm (copy-tree gptel-request--handlers))
         ;; (fsm-done (lambda (fsm)
         ;;             (pcase (plist-get (gptel-fsm-info fsm) :context)
         ;;               ('file-search )
         ;;              ))))
         )
    (gptel-request query
      :callback
      (lambda (response info)
        ;; (plist-put info :post (cons (lambda (info)
        ;;                               (em "killing the buffer" buffer)
        ;;                               (kill-buffer buffer))
        ;;                             (plist-get info :post)))
        (if (stringp response)
            (let ((gptel-backend gptel-openai-response-backend)
                  (gptel-model 'gpt-5-nano)
                  (gptel-tools nil)
                  (gptel-openai-responses--tools nil)
                  (gptel-context nil)
                  cite-list)
              (gptel-request (with-temp-buffer
                               (insert "Summarize the following. The summary should be in the form of a bullet "
                                       "list with appropriate citations in the form cite:<paper_id>:\n\n"
                                       response "\n")
                               (amsha/gptel-oai-response-insert-file-search-query-and-results info)
                               (amsha/gptel--replace-file-id-with-cite (point-min) (point-max))
                               (em "Recieved oai-file-search result")
                               (let ((ret-string (buffer-string)))
                                 (setf cite-list (-uniq
                                                  (append
                                                   (-flatten (s-match-strings-all "cite:[a-z0-9_\\-&]*" ret-string))
                                                   (when-let (results (plist-get info :file-search-call-results))
                                                         (--map (concat "cite:" (string-replace ".txt" "" (plist-get it :filename)))
                                                                results)))))
                                 ret-string))
                :context cite-list
                :callback
                (lambda (response info)
                  (funcall callback
                           (if (stringp response)
                               (concat response
                                       "\n\nPapers queried:\n- "
                                       (string-join (plist-get info :context) "\n- "))
                             (format "gptel-request<2> failed with message: %s"
                                     (plist-get info :status)))))))
          (funcall callback (format "gptel-request<1> failed with message: %s"
                   (plist-get info :status))))))))

(defun okm-pdf-question (callback paper_id questions)
  "Returns the answer to the question from paper represented by paper_id."
  (em "asking questions from " paper_id)
  (let ((node (car (org-roam-ql-nodes `(properties "Custom_ID" ,paper_id))))
        (gptel-backend gptel-openai-response-backend)
        (gptel-model 'gpt-5-nano)
        (gptel-tools nil)
        (gptel-context nil))
    (if node
        (gptel-request (with-temp-buffer
                         (concat "The text of a paper, extracted from a pdf file, is as follows:\n\n"
                                 (okm-get-pdf-txt paper_id)
                                 "\n-------------------------\n The text from the pdf ends here.\n\n"
                                 "Based on this paper, answer the following. Be concise!:\n"
                                 questions))
          :context (cons questions (org-roam-node-id node))
          :callback
          (lambda (response info)
            (funcall callback
                     (if (stringp response)
                           (pcase-let ((`(,questions . ,node-id) (plist-get info :context)))
                             (save-excursion
                               (org-roam-with-file (org-roam-node-file (org-roam-node-from-id node-id)) nil
                                 (org-with-wide-buffer
                                  (goto-char (point-min))
                                  (org-next-visible-heading 1)
                                  (insert "\n\n** ")
                                  (org-timestamp-inactive '(16))
                                  (insert " [[id:54066713-c3ba-464b-870f-6aa0be6604d7][gpt query]] "
                                          (string-replace "\n" " " questions)
                                          "\n" response "\n"))))
                             response)
                       (format "gptel-request failed with message: %s"
                               (plist-get info :status))))))
      (funcall callback (format "paper_id %s is not found." paper_id)))))

(defun okm-gptel-get-papers-abstract-summary (tool-callback paper-ids)
  (let* ((count 0)
         (results-buffer (generate-new-buffer (generate-new-buffer-name "abstract-summary")))
         (callback (lambda (result)
                     (with-current-buffer results-buffer
                       (insert "----------------------\n"
                               result "\n")
                       (cl-incf count)
                       (when (eq count (length paper-ids))
                         (funcall tool-callback (buffer-string)))))))
    (mapcar (lambda (paper-id)
              (okm-gptel-get-paper-abstract-summary callback paper-id))
            paper-ids)))

(defun okm-paper-get-notes (paper_id)
  "Returns the notes from paper represented by paper_id."
  (let ((node (car (org-roam-ql-nodes `(properties "Custom_ID" ,paper_id)))))
    (if node
        (save-excursion
          (org-roam-with-file (org-roam-node-file node) nil
              (org-with-wide-buffer
               (goto-char (point-min))
               (concat
                (okm-gptel-get-paper-details paper_id) "\n"
                (org-roam-subtree-aware-preview-function node)))))
      (format "paper_id %s is not found." paper_id))))

(defun okm-papers-get-notes (paper-ids)
  "Returns the notes from paper represented by paper-ids."
  (with-temp-buffer
    (mapcar (lambda (paper-id)
              (insert paper-id "\n================\n"
                      (okm-paper-get-notes paper-id) "\n\n"))
            paper-ids)
    (buffer-string)))

(defun okm-gptel-get-keywords ()
  "Return the list of keywords in the db."
  (-map #'org-roam-node-title (org-roam-ql-nodes '(and rt lvl1))))

(defun okm-gptel-paper-ids-for-keyword (keyword)
  "Return the list of paper-ids for a given keyword."
  (--map (alist-get "CUSTOM_ID" (org-roam-node-properties it) nil nil #'string-equal)
         (org-roam-ql-nodes `(and (backlink-to (and (title ,keyword t)
                                                    (file "research topics"))
                                               :type nil)
                                  (file "research_papers")))))

(defun okm-gptel-get-paper-details (paper-id)
  "Given the paper-id, get the details of a paper.
Meant to be used for LLMs.
The tool-callback should take one value, the result. "
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
 	 (bib-entry (bibtex-completion-get-entry paper-id)))
         ;; (kill-new (format "%s , %s" (bibtex-completion-apa-get-value "author-abbrev" entry) (bibtex-completion-get-value "year" entry)))))
    (if bib-entry
        (format "* title: %s\n* year: %s\n* authors: %s"
            (bibtex-completion-get-value "title" bib-entry)
            (bibtex-completion-get-value "author" bib-entry)
            (bibtex-completion-get-value "year" bib-entry))
      (format "Details for id %s not found" paper-id))))

(defun okm-gptel-get-papers-details (paper-ids)
  "Given a list of paper-ids, return the paper details as a single string."
  (with-temp-buffer
    (mapcar (lambda (paper-id)
              (insert paper-id "\n================\n"
                      (okm-gptel-get-paper-details paper-id) "\n\n"))
            paper-ids)
    (buffer-string)))

(defvar-local gptel-agent-session-id nil)

(defun okm-gptel-write-session-note (relative-path content)
  "Write CONTENT in RELATIVE-PATH relative to the current session director.
session directory is '~/.emacs.d/.cache/gptel-agent/<session-id>/'.
Session-id is taken from the buffer local variable
`gptel-agent-session-id'. If `gptel-get-paper-details' is not defined,
then it will be set here."
  (unless gptel-agent-session-id
    (setq-local gptel-agent-session-id (concat (format-time-string "%Y-%m-%d_%H%M_" (current-time))
                                               (org-trim (shell-command-to-string org-id-uuid-program)))))
  ;; (with-current-buffer (get-buffer-create (expand-file-name relative-path gptel-agent-session-id))
    ;; (insert "\n" content)))
  (let* ((directory (expand-file-name gptel-agent-session-id "~/.emacs.d/.cache/gptel-agent"))
         (file-path (expand-file-name relative-path directory)))
    (save-excursion
      (unless (f-exists-p directory)
        (make-directory directory t))
      (unless (f-exists-p file-path)
        (unless (f-exists-p (f-parent file-path))
          (make-directory (f-parent file-path) t))
        (with-temp-buffer
          (write-region nil nil file-path)))
      (with-temp-buffer
        (goto-char (point-min))
        (insert "\n" content)
        (write-region nil nil file-path t)
        file-path))))

(defun okm-open-session-notes ()
  (interactive)
  (if gptel-agent-session-id
      (dired (expand-file-name gptel-agent-session-id "~/.emacs.d/.cache/gptel-agent"))
    (error "No gptel-agent-session-id")))

;;; preset highlight **********************************************************************
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

;;; agent-skiklls *************************************************************************
(defvar gptel-agent-skills-dirs (list (expand-file-name "~/.emacs.d/.cache/gptel-skills/"))
  "Directories holding local skills (each skill is a subdirectory).")

(defvar gptel-agent-skills--known-skills nil
  "Known skills - cache.")

(defun gptel-agent-skills-update ()
  "Update the known skills list from `gptel-agent-skills-dirs'."
  (mapcar (lambda (dir)
            (dolist (skill-file (f-glob "**/SKILL.md" dir))
              (pcase-let ((`(,name . ,skill-plist)
                           (gptel-agent-read-file skill-file)))
                (setf (alist-get name gptel-agent-skills--known-skills nil nil #'string-equal)
                      (cons (f-parent skill-file) skill-plist)))))
          gptel-agent-skills-dirs)
  gptel-agent-skills--known-skills)

(defun gptel-agent-write-skills-summary-to-hidden-buffer ()
  "Write a compact skills summary (name + description) into a hidden buffer.
  Add that buffer to `gptel-context' so gptel will include it as context.
  Return the buffer object."
  (let ((buf (get-buffer-create "*gptel-skills-summary*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# Skills summary\n\n")
        ;; gptel-agent-skills--known-skills is expected to be populated already
        (when (and (boundp 'gptel-agent-skills--known-skills)
                   gptel-agent-skills--known-skills)
          (dolist (entry gptel-agent-skills--known-skills)
            (let* ((name (car entry))
                   ;; the skill metadata/plist is often stored in the tail;
                   ;; gptel-agent-skills-update stores the plist in the cdr/last element
                   (meta (if (consp (cdr entry))
                             (cdr entry)
                           (cdr (last entry))))
                   (desc (and (plistp meta) (plist-get meta :description))))
              (insert (format "## %s\n\n%s\n\n" name (or desc "(no description)"))))))
        (setq buffer-read-only t))
      (gptel-context-add))
    buf))

(defun gptel-agent-skill--tool-run-script-async (callback skill script args)
  "Run SCRIPT (relative to skill dir) with Poetry asynchronously.
  CALLBACK is called with a JSON string representing the result when process exits."
  (let* ((dir (car (alist-get skill gptel-agent-skills--known-skills nil nil #'string-equal)))
         (script-rel (or script ""))
         (script-full (and dir (expand-file-name script-rel dir))))
    (cond
     ((not dir)
      (funcall callback (json-serialize `(:error "skill-not-found" :skill ,skill))))
     ((not (file-exists-p script-full))
      (funcall callback (json-serialize `(:error "script-not-found" :script ,script-full))))
     (t
      (let* ((out-buf (get-buffer-create (format "*gptel-skill-run-%s-%s*" skill script-rel)))
             (poetry-cmd "poetry")
             (proc-args (append (list "run" "-q" "python" script-full) (or args '())))
             (default-directory dir)
             (proc
              (apply #'start-process
                     ;; name
                     (format "gptel-skill-run-%s" (file-name-nondirectory script-rel))
                     out-buf
                     poetry-cmd
                     proc-args)))

        ;; Ensure buffer holds output and is not displayed
        (set-process-query-on-exit-flag proc nil)
        (set-process-buffer proc out-buf)
        (set-process-filter
         proc
         (lambda (_proc string)
           ;; append is automatic to buffer, but ensure we have it
           (with-current-buffer out-buf
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert string)))))
        (set-process-sentinel
         proc
         (lambda (p event)
           (when (memq (process-status p) '(exit signal))
             (let* ((exit-code (if (fboundp 'process-exit-status)
                                   (process-exit-status p)
                                 ;; parse event text as fallback "exited abnormally with code X"
                                 (if (string-match "code \\([0-9]+\\)" event)
                                     (string-to-number (match-string 1 event))
                                   (if (string-match "finished" event) 0 1))))
                    (output (with-current-buffer out-buf (buffer-string)))
                    (result (json-serialize
                             `(:skill ,skill
                                      :script ,script-rel
                                      :exit-code ,exit-code
                                      :output ,output))))
               ;; call the callback with JSON string result
               (funcall callback result)
               ;; cleanup buffer
               (when (buffer-live-p out-buf) (kill-buffer out-buf)))))))))))

(defun gptel-agent-execute-skill (callback skill action &optional path args)
  "Main tool handler.
CALLBACK is used for async responses. ACTION is one of \"summary\"/\"details\"/\"load_resource\"/\"run_script\".
PATH is a unified path (resource or script)."
    (pcase-let ((`(,skill-dir . ,skill-plist) (alist-get skill gptel-agent-skills--known-skills nil nil #'string-equal)))
      (if (not skill-dir)
          (funcall (or callback (lambda (x) x))
                   (json-serialize `(:error "skill-not-found" :skill ,skill)))
        (pcase action
          ("summary"
           (plist-get skill-plist :description))
          ("details"
           (funcall callback (json-serialize
                                `(:skill
                                  ,skill
                                  :body
                                  ,(plist-get
                                    (cdr (gptel-agent-read-file
                                          (expand-file-name "SKILL.md" skill-dir)
                                          ;; forcing the full content
                                          '(("--noop---"))))
                                    :system)))))
          ("load_resource"
           (if (not path)
               (funcall (or callback (lambda (x) x)) (json-serialize `(:error "no-path-provided" :action ,action)))
             (let ((full (expand-file-name path skill-dir)))
               (if (not (file-readable-p full))
                   (funcall callback (json-serialize `(:error "resource-not-found" :path ,full)))
                 (funcall callback
                          (json-serialize `(:skill ,skill
                                                   :resource ,path
                                                   :content ,(with-temp-buffer
                                                               (insert-file-contents full)
                                                               (buffer-string)))))))))
          ("run_script"
           (if (not path)
               (funcall (or callback (lambda (x) x)) (json-serialize `(:error "no-path-provided" :action ,action)))
             ;; run async; reuse your async runner (ensure it accepts CALLBACK, skill, script, args)
             (gptel-agent-skill--tool-run-script-async (or callback (lambda (x) x)) skill path args)))
          (_
           (funcall (or callback (lambda (x) x)) (json-serialize `(:error "unknown-action" :action ,action))))))))

;;; persistent context ********************************************************************
(defvar gptel-persistent-context-dir "~/.emacs.d/.cache/gptel-temp-context")

(make-directory gptel-persistent-context-dir t)
(dolist (context-file (directory-files gptel-persistent-context-dir
                                       nil
                                       directory-files-no-dot-files-regexp))
  (gptel-make-preset (intern (format "%s-c" (f-base context-file)))
    :description (format "Use persistent context %s" (f-base context-file))
    :context `((,context-file))))

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
        :context `((,context-file))))))

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

;;;; * Save preset with only context

(defun amsha/gptel--save-context-as-preset (name &optional description)
  "Save a preset with only the context.

Same as `gptel--save-preset', only save context."
  (interactive
   (list (intern (completing-read "Save gptel settings to (existing or new) preset: "
                                  gptel--known-presets))
         (read-string "Description (optional): ")))
  (let ((preset-code
         `(gptel-make-preset ',name
           :description ,(when (and description
                                (not (string-blank-p description)))
                           description)
           :context ',gptel-context)))
    (kill-new (pp-to-string preset-code))
    (eval preset-code)
    (message "Preset %s saved. (Lisp expression for preset saved to kill-ring)"
             (propertize (symbol-name name) 'face 'highlight))))

(defun amsha/gptel-context-remove-all-except-overlays (&optional verbose)
  "Remove all gptel context.

If VERBOSE is non-nil, ask for confirmation and message
afterwards."
  (interactive (list t))
  (if (null gptel-context)
      (when verbose (message "No gptel context sources to remove."))
    (when (or (not verbose) (y-or-n-p "Remove all context except overlays? "))
      (cl-loop
       for context in gptel-context
       for (source . ovs) = (ensure-list context)
       unless (cl-every #'overlayp ovs) do ; unless buffers and buffer regions
       (gptel-context-remove source) ;files or other types
       finally do (setq gptel-context nil)))
    (when verbose (message "Removed all gptel context sources except overlays."))))

(defun amsha/remove-context-at-point ()
  (interactive)
  (gptel-context-remove))

(transient-suffix-put 'gptel-menu '(0 1 6) :key "-D")
(transient-insert-suffix 'gptel-menu '(0 1 6)
  '("-d" "Remove non overlays" amsha/gptel-context-remove-all-except-overlays
    :if (lambda () gptel-context)
    :transient t))

(transient-insert-suffix 'gptel-menu '(0 1 7)
  '("/r" "Remove context at point" amsha/remove-context-at-point
    :if (lambda () (or (region-active-p) (gptel-context--at-point)))
    :transient t))

(transient-append-suffix 'gptel--preset '(0 0 -1)
  '("/C" "Save current context as new preset" amsha/gptel--save-context-as-preset :if (lambda () gptel-context)))

;;; transform functions *******************************************************************

;;;; * okm replace cite with context
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

;;;; * okm add pdf to context
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

;;; post response cleanups ****************************************************************
;;;; * handline headings in org response
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

;;;; * Function to restore gptel states
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
  "Updating annotations strings, preserving text properties of replaced text."
  (save-excursion
    (goto-char start)
    (let ((nodes (--map
                  (cons
                   (alist-get "OPENAI_FILE_ID" (org-roam-node-properties it) nil nil #'string-equal)
                   (cons (alist-get "CUSTOM_ID" (org-roam-node-properties it) nil nil #'string-equal)
                         (org-roam-node-id it)))
                  (org-roam-ql-nodes `(properties "OPENAI_FILE_ID" ".+")))))
      (condition-case err
          (save-match-data
            (while (re-search-forward "\\[file_citation:\\([a-zA-Z0-9\\-]*\\)\\]" end)
              (when-let* ((key (match-string 1))
                          (elt (alist-get key nodes nil nil #'string-equal)))
                (let* ((oldstr (match-string 0))
                       (oldlen (length oldstr))
                       (newtext (format " [cite:%s]" (car elt)))
                       (newlen (length newtext))
                       (beg (match-beginning 0))
                       (props (and (< beg (match-end 0))
                                   (text-properties-at beg))))
                  ;; Perform replacement treating NEWTEXT literally.
                  (replace-match newtext nil t)
                  ;; Adjust `end' to account for change in buffer length.
                  (setq end (+ end (- newlen oldlen)))
                  ;; Restore text properties from the original match's start to the newly inserted text.
                  (when props
                    (add-text-properties beg (+ beg newlen) props))))))
        (search-failed nil)))))

;; (add-to-list 'gptel-post-response-functions #'gptel-openai-assistant-replace-annotations-with-filename)
(add-to-list 'gptel-post-response-functions #'amsha/gptel--replace-file-id-with-cite)

;;; setup *********************************************************************************
(bind-keys :package gptel
           ("C-c o q m" . gptel-menu)
           ("C-c o q b" . gptel)
           ("C-c o q Q" . gptel-send)
           ("C-c o q y" . amsha/gptel-yank)
           :map gptel-mode-map
           ("C-c DEL" . amsha/erase-buffer-with-confirmation))

;; Not sure where this is getting bound! 🤷
(evil-define-key '(normal visual) gptel-mode-map (kbd "RET") nil)

(setf gptel-use-curl t
      gptel-backend gptel-openai-response-backend
      gptel-model 'gpt-5-mini
      gptel-default-mode #'org-mode

      gptel-org-branching-context t
      gptel-expert-commands t
      gptel-highlight-methods '(face)
      (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n"
      (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"

      gptel-quick-backend gptel-openai-response-backend
      ;; the gpt 5 models are reasoning models and the max tokens set is too small with quick.
      ;; See https://github.com/openai/openai-python/issues/2546
      gptel-quick-model 'gpt-4.1-mini)

(custom-set-faces
 '(gptel-response-highlight ((t (:background "#112233" :extend t)))))

;; (put 'o3-mini :request-params '(:reasoning_effort "high" :stream :json-false))

(add-to-list 'yank-excluded-properties 'gptel)
(add-hook 'gptel-mode-hook (lambda () (gptel-highlight-mode +1)))

(advice-add 'gptel--display-tool-calls :before #'amsha/notify-tool-calls)

;; Add fsm-last to the request handlers
(cl-pushnew 'gptel--fsm-last (alist-get 'DONE gptel-request--handlers))
(cl-pushnew 'gptel--fsm-last (alist-get 'ERRS gptel-request--handlers))

(defvar amsha/gptel--openrouter
  (gptel-make-openai "OpenRouter"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (gethash 'openrouter-apk configurations)                   ;can be a function that returns the key
    :models '(deepseek/deepseek-r1-distill-llama-70b:free)))

(defvar amsha/gptel-anthropic-api
 (gptel-make-anthropic "Claude"          ;Any name you want
  :stream t                             ;Streaming responses
  :key (gethash 'anthropic-api configurations)))

(condition-case err
    (setf (gptel-backend-models gptel--openai) (append (gptel-backend-models gptel--openai)
                                                   (--map (prog1 it (put it :capabilities '(reasoning)))
                                                          '(gpt-4o-search-preview gpt-4o-mini-search-preview))))
  (error (message "ERROR %s" err)))

(provide 'gptel-extensions)
;;; gptel-extensions.el ends here
