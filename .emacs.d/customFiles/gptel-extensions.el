;;; llm                     -*- lexical-binding: t; -*-

;;; Commentary:
;;my custom org setup
;;; Code:

(require 'gptel)

;; Additions to global fsm ***************************************************************

(defun gptel--wait-again-p (info)
  "Check if the fsm should WAIT."
  (plist-get info :wait))

(defun gptel--delay-p (info)
  "Test fsm transition to DELAY."
  (plist-get info :delay))

(defun gptel--await-p (info)
  "Test fsm transition to AWAIT."
  (plist-get info :await))

(defun gptel-handle-on-wait-again (fsm)
  "Handle on-wait-callback."
  (plist-put (gptel-fsm-info fsm) :wait nil))

(defun gptel-handle-on-wait-callback (fsm)
  "Handle on-wait-callback."
  (when-let ((callback (plist-get (gptel-fsm-info fsm) :on-wait-callback)))
    (funcall callback)
    (plist-put (gptel-fsm-info fsm) :on-wait-callback nil)))

(defun gptel--handle-delay (fsm)
  "Handle the delay state."
  (let ((delay (plist-get (gptel-fsm-info fsm) :delay)))
    (cl-assert (numberp delay))
    (plist-put (gptel-fsm-info fsm) :delay nil)
    (run-at-time delay nil (lambda () (gptel--fsm-transition fsm)))))

;; Adding TYPE -> WAIT & TYPE -> DELAY transitions
(setf (alist-get 'TYPE gptel-request--transitions) '((gptel--await-p . AWAIT)
                                                     (gptel--delay-p . DELAY)
                                                     (gptel--error-p . ERRS)
                                                     (gptel--tool-use-p . TOOL)
                                                     (gptel--wait-again-p . WAIT)
                                                     (t . DONE)))

;; MAYBE: this is useful to be connected to other states as well?
;; From DELAY go to what was expected
(setf (alist-get 'DELAY gptel-request--transitions) '((gptel--await-p . AWAIT)
                                                      (gptel--error-p . ERRS)
                                                      (gptel--tool-use-p . TOOL)
                                                      (gptel--wait-again-p . WAIT)
                                                      (t . DONE)))


;; From AWAIT go to what was expected
(setf (alist-get 'AWAIT gptel-request--transitions)
      '((gptel--delay-p . DELAY)
        (gptel--error-p . ERRS)
        (gptel--tool-use-p . TOOL)
        (gptel--wait-again-p . WAIT)
        (t . DONE)))

(push '(gptel--await-p . AWAIT) (alist-get 'INIT gptel-request--transitions))
(push '(gptel--await-p . AWAIT) (alist-get 'WAIT gptel-request--transitions))
(push '(gptel--await-p . AWAIT) (alist-get 'TOOL gptel-request--transitions))

(setf (alist-get 'DELAY gptel-request--handlers) '(gptel--handle-delay))

(setf (alist-get 'WAIT gptel-request--handlers) '(gptel-handle-on-wait-callback gptel-handle-on-wait-again gptel--handle-wait))

(cl-defgeneric gptel-backend--on-start-of-state (backend state info)
  "When a the gptel fsm starts a new STATE, this will be called.
This method is called before the handlers of the new state are invoked.")

(cl-defmethod gptel-backend--on-start-of-state ((backend gptel-backend) state info))

(cl-defgeneric gptel-backend--on-end-of-state (backend state info)
  "When a the gptel fsm transitions to a new state, this will be called with the old STATE.")

(cl-defmethod gptel-backend--on-end-of-state ((backend gptel-backend) state info))

(defun gptel--fsm-transition-handle-backend-on-start-on-end (oldfn fsm &optional new-state)
  "Advice for `gptel--fsm-transition'.
Invokes the `gptel-backend--on-start-of-state' & `gptel-backend--on-end-of-state'."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    (gptel-backend--on-end-of-state backend (gptel-fsm-state fsm) info)
    ;; the start is being called before the handlers of the next state..
    (gptel-backend--on-start-of-state backend (gptel--fsm-next fsm) info)
    (funcall oldfn fsm new-state)))

(advice-add #'gptel--fsm-transition :around #'gptel--fsm-transition-handle-backend-on-start-on-end)

;; Tool use ******************************************************************************
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

;; Openai assistant api ******************************************************************
(defvar-local gptel-openai-assistant-thread-id nil)

;; TODO: Get this from api?
(defvar-local gptel-openai-assistant-assistant-id (gethash 'openai-assistant-id configurations))


(cl-defstruct (gptel-openai-assistant
               (:constructor gptel-openai--make-assistant)
               (:copier nil)
               (:include gptel-openai))
  messages-data)

(defvar url-http-end-of-headers)
(defun gptel--openai-assistant-url-retrive (method data url info callback)
  "Get data from URL with DATA using METHOD (POST/GET).
INFO is info from gptel
CALLBACK is called with the response from calling url-retrive."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (append `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))
                   ("OpenAI-Beta" . "assistants=v2"))))
        (url-request-data data))
    (url-retrieve (cl-typecase url
                    (string url)
                    (function (funcall url))
                    (t (error "Unknown value for url (%s) in step" url)))
                  (lambda (_)
                    (pcase-let ((`(,response ,http-status ,http-msg ,error)
                                 (custom--url-parse-response))
                                (buf (current-buffer)))
                      (plist-put info :http-status http-status)
                      (plist-put info :status http-msg)
                      (when error
                        (plist-put info :error error))
                      (with-current-buffer (plist-get info :buffer)
                        (funcall callback response))
                      (kill-buffer buf)
                      ))
                  nil t t)))

;; copied from `gptel--url-parse-response' beacuse we don't want the following:
;; (gptel--parse-response backend response proc-info)
(defun custom--url-parse-response ()
  "Parse response from url-retrive."
  (when gptel-log-level             ;logging
    (save-excursion
      (goto-char url-http-end-of-headers)
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode (buffer-substring-no-properties (point-min) (point)))
                    "response headers"))
      (gptel--log (buffer-substring-no-properties (point) (point-max))
                  "response body")))
  (if-let* ((http-msg (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position))))
            (http-status
             (save-match-data
               (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                    (match-string 1 http-msg))))
            (response (progn (goto-char url-http-end-of-headers)
                             (condition-case nil
                                 (gptel--json-read)
                               (error 'json-read-error)))))
      (cond
       ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
       ((or (memq url-http-response-status '(200 100))
            (string-match-p "\\(?:1\\|2\\)00 OK" http-msg))
        (list response
              http-status http-msg))
       ((plist-get response :error)
        (list nil http-status http-msg (plist-get response :error)))
       ((eq response 'json-read-error)
        (list nil http-status (concat "(" http-msg ") Malformed JSON in response.") "json-read-error"))
       (t (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                "Could not parse HTTP response.")))
    (list nil (concat "(" http-msg ") Could not parse HTTP response.")
          "Could not parse HTTP response.")))

(defun gptel-openai-assistant-start-thread (info &optional callback)
  "Use the threads endpoint to start new thread.
Set the `gptel-openai-assistant-thread-id' of the buffer.
INFO is the info plist from gptel.
CALLBACK is invoked without any args after successfully creating a thread."
  (gptel--openai-assistant-url-retrive
   "POST" nil "https://api.openai.com/v1/threads"
   info
   (lambda (response)
     (with-current-buffer
         (plist-get info :buffer)
       (setq-local gptel-openai-assistant-thread-id (plist-get response :id))
       (when callback (funcall callback))))))

(defun gptel-openai-assistant-add-message (info callback)
  "Use the messages endpoint to start new thread.
Needs the `gptel-openai-assistant-thread-id' of the buffer to be set.
INFO is the info plist from gptel.
CALLBACK is invoked without any args after successfully creating a thread."
  (gptel--openai-assistant-url-retrive
   "POST"
   (encode-coding-string
    (gptel--json-encode (gptel-openai-assistant-messages-data (plist-get info :backend)))
    'utf-8)
   (with-current-buffer
       (plist-get info :buffer)
     (if gptel-openai-assistant-thread-id
         (format "https://api.openai.com/v1/threads/%s/messages"
                 gptel-openai-assistant-thread-id)
       (user-error "No thread in current buffer to add messages!")))
   info
   (lambda (response)
     (when callback (funcall callback)))))

(cl-defmethod gptel--request-data ((backend gptel-openai-assistant) prompts)
  (setf (gptel-openai-assistant-messages-data backend)
        (list :role "user"
              :content
              `[(:type "text"
                       :text ,(plist-get (car (last prompts)) :content))]))
  (let ((prompts-plist
         `(:assistant_id ,gptel-openai-assistant-assistant-id
                         :stream ,(or (and gptel-stream gptel-use-curl
                                           (gptel-backend-stream backend))
                                      :json-false))))
    (when gptel-temperature
      (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-max-tokens
      (plist-put prompts-plist (if (memq gptel-model '(o1-preview o1-mini))
                                   :max_completion_tokens :max_tokens)
                 gptel-max-tokens))
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params backend)
     (gptel--model-request-params  gptel-model))))

(cl-defmethod gptel--parse-response ((_ gptel-openai-assistant) response info)
  ;; TODO: peridically poll the run and check if it's completed. If so, get last message
  ;; https://platform.openai.com/docs/assistants/deep-dive#polling-for-updates
  (error "not implemented yet!"))

(cl-defmethod gptel-curl--parse-stream ((_ gptel-openai-assistant) info)
  (let* ((content-strs))
    (condition-case err
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (unless (looking-at " *\\[DONE\\]")
              (when-let* ((response (gptel--json-read))
                          (content (map-nested-elt
                                    response '(:delta :content 0 :text :value)))
                          )
                (if-let* ((annotations (map-nested-elt
                                        response '(:delta :content 0 :text :annotations)))
                          (_ (length> annotations 0)))
                    (cl-loop for annotation across annotations
                             for file-id = (map-nested-elt
                                            annotation '(:file_citation :file_id))
                             if file-id
                             do (push (format "[[file_citation:%s][%s]]" file-id content) content-strs))
                  (push content content-strs))))))
      ((json-parse-error json-end-of-file search-failed)
       (goto-char (match-beginning 0)))
      (error
       (signal (car err) (cdr err))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--handle-openai-assistant-await (fsm)
  (let* ((info (gptel-fsm-info fsm))
         (await-manual-state (plist-get info :await)))
    (when (gptel-openai-assistant-p (plist-get info :backend))
     ;; This tells us await manual is happening from after the init
      (pcase await-manual-state
        (:send-message
         (plist-put info :await nil)
         (cl-labels ((send-message ()
                       (gptel-openai-assistant-add-message
                        info
                        (lambda ()
                          (plist-put info :wait t)
                          (gptel--fsm-transition fsm)))))
           (if gptel-openai-assistant-thread-id
               (send-message)
             (gptel-openai-assistant-start-thread
              info
              (lambda ()
                (if (plist-get info :error)
                    (gptel--fsm-transition fsm)
                  (send-message)))))))
        (:await-runs-complete
         (plist-put info :await nil)
         ;; TODO: The polling can happen here
         (error "Not implemented yet"))))))
     
(cl-defmethod gptel-backend--on-end-of-state ((backend gptel-openai-assistant) state info)
  (when (eq state 'INIT)
    (plist-put info :await :send-message)))

;;;###autoload
(defun gptel-openai-make-assistant ()
  "Create a openai-assistant backend."
  (gptel-openai--make-assistant
   :name "gptel-openai-assistant"
   :host "api.openai.com"
   :key 'gptel-api-key
   :models (gptel--process-models gptel--openai-models)
   :header (lambda () (when-let (key (gptel--get-api-key))
                        `(("Authorization" . ,(concat "Bearer " key))
                          ("OpenAI-Beta" . "assistants=v2"))))
   :protocol "https"
   :endpoint "/v1/threads/thread_id/runs"
   :url (lambda ()
          (if gptel-openai-assistant-thread-id
              (format "https://api.openai.com/v1/threads/%s/runs" gptel-openai-assistant-thread-id)
            (user-error "No thread in current buffer to add messages!")))
   :stream t))

;;;###autoload
(defun gptel-openai-assistant-create-new-thread ()
  "Create a new thread in the current buffer."
  (interactive)
  (gptel-openai-assistant-start-thread `(:buffer ,(buffer-name))))

(push 'gptel--handle-openai-assistant-await (alist-get 'AWAIT gptel-request--handlers))

(setf (alist-get "openai-assistant" gptel--known-backends
                 nil nil #'equal)
      (gptel-openai-make-assistant))

(provide 'gptel-extensions)
;;; orgZ.el ends here
