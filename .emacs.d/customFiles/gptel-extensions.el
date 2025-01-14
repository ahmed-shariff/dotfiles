;;; llm                     -*- lexical-binding: t; -*-

;;; Commentary:
;;my custom org setup
;;; Code:

(require 'gptel)

(defun gptel--wait-again-p (info)
  "Check if the fsm should WAIT."
  (plist-get info :wait))

(defun gptel--delay-p (info)
  "Test fsm transition to DELAY."
  (plist-get info :delay))

(defun gptel-handle-on-wait-callback (fsm)
  "Handle on-wait-callback."
  (when-let ((callback (plist-get (gptel-fsm-info fsm) :on-wait-callback)))
    (funcall callback)))

(defun gptel--handle-delay (fsm)
  "Handle the delay state."
  (let ((delay (plist-get (gptel-fsm-info fsm) :delay)))
    (cl-assert (numberp delay))
    (plist-put (gptel-fsm-info fsm) :delay nil)
    (run-at-time delay nil (lambda () (gptel--fsm-transition fsm)))))

;; Adding TYPE -> WAIT & TYPE -> DELAY transitions
(setf (alist-get 'TYPE gptel-request--transitions) '((gptel--delay-p . DELAY)
                                                     (gptel--error-p . ERRS)
                                                     (gptel--tool-use-p . TOOL)
                                                     (gptel--wait-again-p . WAIT)
                                                     (t . DONE)))

;; MAYBE: this is useful to be connected to other states as well?
;; From DELAY go to what was expected
(setf (alist-get 'DELAY gptel-request--transitions) '((gptel--error-p . ERRS)
                                                     (gptel--tool-use-p . TOOL)
                                                     (gptel--wait-again-p . WAIT)
                                                     (t . DONE)))

(setf (alist-get 'DELAY gptel-request--handlers) '(gptel--handle-delay))

(setf (alist-get 'WAIT gptel-request--handlers) '(gptel-handle-on-wait-callback gptel--handle-wait))

(defvar-local gptel-openai-assistant-thread-id nil)

(cl-defgeneric gptel-backend--on-start-of-state (backend state info))

(cl-defmethod gptel-backend--on-start-of-state ((backend gptel-backend) state info)
  (em "start of state" state))

(cl-defgeneric gptel-backend--on-end-of-state (backend state info))

(cl-defmethod gptel-backend--on-end-of-state ((backend gptel-backend) state info)
  (em "end of state" state))

(defun gptel--fsm-transition-handle-backend-on-start-on-end (oldfn fsm &optional new-state)
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    (gptel-backend--on-end-of-state backend (gptel-fsm-state fsm) info)
    ;; the start is being called before the handlers of the next state..
    (gptel-backend--on-start-of-state backend (gptel--fsm-next fsm) info)
    (funcall oldfn fsm new-state)))

(advice-add #'gptel--fsm-transition :around #'gptel--fsm-transition-handle-backend-on-start-on-end)

;; (unless (memq 'gptel--openai-assistant-reset-backend (alist-get 'ERRS gptel-request--handlers))
;;   (push 'gptel--openai-assistant-reset-backend (alist-get 'ERRS gptel-request--handlers)))
;; (unless (memq 'gptel--openai-assistant-reset-backend (alist-get 'DONE gptel-request--handlers))
;;   (push 'gptel--openai-assistant-reset-backend (alist-get 'DONE gptel-request--handlers)))

;; (advice-add #'gptel-request :around #'gptel-request--handle-openai-assistant-startup)

;; issues:
;; - not all methods recieve the backend as a parameter. Often the backend can be obtained from the info
;; - gptel--request-data needs the stream parameter....
;; - the :parser and :stream values of the info need to be updated on the fly for what I am trying to do here!
;;   - actually, the process sentinal/filter methods also need to be updated!!!

;; This gets created when a new session/thread is started
(cl-defstruct (gptel-openai-assistant-session
               (:constructor gptel-openai--make-assistant-session)
               (:copier nil)
               (:include gptel-openai))
  ;; (session-step-endpoint-urls
  ;;  #'gptel-openai-assistant--step-endpoint-url
  ;;  (:documentation
  ;;   "This is a function that take one argument, the current step, and returns the url.")
  ;;  (:type 'function))
  (steps
   '(:threads :messages :runs)
   :documentation "The sequence of the steps.")
  (step-idx -1 :documentation "Current step in steps sequence.")
  cached-prompts
  (step
   :threads
   :documentation "The current step of the session."))

(defvar gptel-openai-assistant--known-steps nil)

(cl-defstruct (gptel-openai-assistant-step
               (:constructor gptel-openai-assistant--make-step))
  (url
   :documentation "String of function that returns the url as string")
  (request-data-fn
   :documentation "Takes backend and prompts as arg. See `gptel--request-data'")
  (parse-response-fn
   :documentation "Takes response and info as args. See `gptel--parse-response'")
  (parse-stream-fn
   :documentation "Takes info as args. See `gptel-curl--parse-stream'"))

(cl-defun gptel-openai-assistant--add-known-step (step &key url request-data-fn parse-response-fn parse-stream-fn)
  (setf (alist-get step gptel-openai-assistant--known-steps)
        (gptel-openai-assistant--make-step
         :url url :request-data-fn request-data-fn :parse-response-fn parse-response-fn :parse-stream-fn parse-stream-fn)))

(gptel-openai-assistant--add-known-step
 :threads
 :url "https://api.openai.com/v1/threads"
 :parse-response-fn (lambda (response info)
                      (with-current-buffer (plist-get info :buffer)
                        (setq-local gptel-openai-assistant-thread-id (plist-get response :id))
                        nil))
 :parse-stream-fn (lambda (info)
                    (goto-char (point-min))
                    (re-search-forward "?\n?\n" nil t)
                    (condition-case err
                        (when-let* ((response (gptel--json-read))
                                    (_ (equal "thread" (plist-get response :object))))
                          (gptel-openai-assistant--progress-step info)
                          (with-current-buffer (plist-get info :buffer)
                            (setq-local gptel-openai-assistant-thread-id (plist-get response :id)))
                          nil)
                      (json-parse-error
                       (goto-char (match-beginning 0)))
                      (error
                       (signal (car err) (cdr err))))))

(gptel-openai-assistant--add-known-step
 :messages
 :url (lambda ()
        (if gptel-openai-assistant-thread-id
            (format "https://api.openai.com/v1/threads/%s/messages" gptel-openai-assistant-thread-id)
          (user-error "No thread in current buffer to add messages!")))
 :request-data-fn (lambda (backend prompts)
                    (list :role "user"
                          :content
                          `[(:type "text"
                             :text ,(plist-get (car (last prompts)) :content))]))
 :parse-stream-fn (lambda (info)
                    (goto-char (point-min))
                    (re-search-forward "?\n?\n" nil t)
                    (condition-case nil
                        (when-let* ((response (gptel--json-read))
                                    (_ (equal "thread.message" (plist-get response :object))))
                          (gptel-openai-assistant--progress-step info)
                          nil)
                      (json-parse-error
                       (goto-char (match-beginning 0)))
                      (error
                       (signal (car err) (cdr err))))))

(gptel-openai-assistant--add-known-step
 :runs
 :url (lambda ()
        (if gptel-openai-assistant-thread-id
            (format "https://api.openai.com/v1/threads/%s/runs" gptel-openai-assistant-thread-id)
          (user-error "No thread in current buffer to add messages!")))
 :request-data-fn (lambda (backend prompts)
                    (let ((prompts-plist
                           `(:assistant_id ,(gethash 'openai-assistant-id configurations)
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
 :parse-response-fn (lambda (response info)
                      ;; TODO: peridically poll the run and check if it's completed. If so, get last message
                      ;; https://platform.openai.com/docs/assistants/deep-dive#polling-for-updates
                      (error "not implemented yet!"))
 :parse-stream-fn (lambda (info)
                    (let* ((content-strs))
                      (condition-case err
                          (while (re-search-forward "^data:" nil t)
                            (save-match-data
                              (if (looking-at " *\\[DONE\\]")
                                  (prog1 nil
                                    (gptel-openai-assistant--progress-step info))
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
                      (apply #'concat (nreverse content-strs)))))

;; (defun gptel-openai-assistant--wait-or-update-step (info)
;;   (if (length>
;;        (gptel-openai-assistant-session-steps (plist-get info :backend))
;;        (1+ (gptel-openai-assistant-session-step-idx (plist-get info :backend))))
;;       (progn
;;         (plist-put info :wait t)
;;         (plist-put info :on-wait-callback
;;                    (lambda ()
;;                      (gptel-openai-assistant--update-session-step info))))
;;     (plist-put info :wait nil)))

;; (defun gptel-openai-assistant--update-session-step (info &optional backend)
;;   (cl-assert (or info backend))
;;   (let* ((backend (or backend
;;                           (plist-get info :backend)))
;;          (new-step
;;           (when-let* ((step-idx (cl-incf (gptel-openai-assistant-session-step-idx backend)))
;;                       (_ (length> (gptel-openai-assistant-session-steps backend) step-idx)))
;;             (nth step-idx (gptel-openai-assistant-session-steps backend))))
;;          (target-buffer (or (and info
;;                                  (plist-get info :buffer))
;;                             ;; When being called first time, assuming it is
;;                             ;; done from the current-buffer
;;                             (current-buffer)))
;;          stream)
;;     (setf 
;;      (gptel-openai-assistant-session-step backend) new-step
;;      (gptel-backend-url backend) (with-current-buffer target-buffer
;;                                    (when new-step
;;                                      (if-let (step (alist-get new-step gptel-openai-assistant--known-steps))
;;                                          (let ((url (gptel-openai-assistant-step-url step)))
;;                                            (em (cl-typecase url
;;                                              (string url)
;;                                              (function (funcall url))
;;                                              (t (error "Unknown value for url (%s) in step (%s)" url new-step)))))
;;                                        (error "Unknown step %s in update session step" new-step))))

;;      ;; This doesn't currently work unless I have set the process sentinel/fileter functions
;;      ;; stream (pcase new-step
;;      ;;          (:threads nil)
;;      ;;          (:messages nil)
;;      ;;          (:runs t)))
;;      )

;;     (when info ;; not first step
;;       (when new-step
;;         (plist-put info :data (gptel--request-data backend (gptel-openai-assistant-session-cached-prompts backend))))
;;       (plist-put info :wait new-step)
;;       (plist-put info :on-wait-callback nil)
;;       ;; ;; KLUDGE: copied from `gptel-request'. Can this be managed upstream?
;;       ;; (plist-put info :stream (and stream
;;       ;;                              ;; TODO: model parameters?
;;       ;;                              gptel-stream gptel-use-curl
;;       ;;                              (gptel-backend-stream backend)))
;;       ;; KLUDGE: copied from `gptel-curl-get-response'. Can this be managed upstream?
;;       ;; (plist-put info :parser (cl--generic-method-function
;;       ;;                          (if stream
;;       ;;                              (cl-find-method
;;       ;;                               'gptel-curl--parse-stream nil
;;       ;;                               (list (aref backend 0) t))
;;       ;;                            (cl-find-method
;;       ;;                             'gptel--parse-response nil
;;       ;;                             (list (aref backend 0) t t)))))
;;       )))

(defvar url-http-end-of-headers)
(defun gptel--openai-assistant-url-retrive (method data url info callback)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (append `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))
                   ("OpenAI-Beta" . "assistants=v2"))))
        (url-request-data data))
    (url-retrieve (cl-typecase url
                    (string url)
                    (function (funcall url))
                    (t (error "Unknown value for url (%s) in step (%s)" url new-step)))
                  (lambda (_)
                    (pcase-let ((`(,response ,http-status ,http-msg ,error)
                                 (custom--url-parse-response))
                                (buf (current-buffer)))
                      (plist-put info :http-status http-status)
                      (plist-put info :status http-msg)
                      (when error
                        (plist-put info :error error))
                      (funcall callback response)
                      ;; (kill-buffer buf)))
                      ))
                  nil t t)))

(defun custom--url-parse-response ()
  "Parse response from BACKEND with PROC-INFO."
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

(defun gptel-openai-assistant-start-thread (info callback)
  (gptel--openai-assistant-url-retrive
   "POST" nil "https://api.openai.com/v1/threads"
   info
   (lambda (response)
     (with-current-buffer
         (plist-get info :buffer)
       (em (setq-local gptel-openai-assistant-thread-id (plist-get response :id)))
       (when callback (funcall callback))))))

(defun gptel-openai-assistant-add-message (info callback)
  (gptel--openai-assistant-url-retrive
   "POST"
   (encode-coding-string
    (gptel--json-encode (plist-get info :data))
    'utf-8)
   (with-current-buffer
       (plist-get info :buffer)
     (if gptel-openai-assistant-thread-id
         (format "https://api.openai.com/v1/threads/%s/messages"
                 gptel-openai-assistant-thread-id)
       (user-error "No thread in current buffer to add messages!")))
   info
   (lambda (response)
     (em response)
     (when callback (funcall callback)))))

(cl-defmethod gptel--request-data ((backend gptel-openai-assistant-session) prompts)
  (when (eq 0 (gptel-openai-assistant-session-step-idx backend))
    (setf (gptel-openai-assistant-session-cached-prompts backend) prompts))
  (let* ((step (gptel-openai-assistant-session-step backend))
         (step-info (alist-get step gptel-openai-assistant--known-steps)))
    (if step-info
        (when-let (fn (gptel-openai-assistant-step-request-data-fn step-info))
          (funcall fn backend prompts))
      (error "Unknown step %s in request data" step-info))))

(cl-defmethod gptel--parse-response ((_ gptel-openai-assistant-session) response info)
  (prog1
      (let* ((step (gptel-openai-assistant-session-step (plist-get info :backend)))
             (step-info (alist-get step gptel-openai-assistant--known-steps)))
        (if step-info
            (when-let (fn (gptel-openai-assistant-step-parse-response-fn step-info))
              (funcall fn response info))
          (error "Unknown step %s in parse response" step)))
    (gptel-openai-assistant--update-session-step info)))

(cl-defmethod gptel-curl--parse-stream ((_ gptel-openai-assistant-session) info)
  (let* ((step (gptel-openai-assistant-session-step (plist-get info :backend)))
         (step-info (alist-get step gptel-openai-assistant--known-steps)))
    (if step-info
        (when-let (fn (gptel-openai-assistant-step-parse-stream-fn step-info))
          (funcall fn info))
      (error "Unknown step %s response" new-step))))

(cl-defmethod gptel-backend--on-start-of-state ((backend gptel-openai-assistant-session) state info)
  (pcase state
    ('DONE (gptel--openai-assistant-reset-backend backend))
    ('WAIT
     (let* ((new-step
             (when-let* ((step-idx (cl-incf (gptel-openai-assistant-session-step-idx backend)))
                         (_ (length> (gptel-openai-assistant-session-steps backend) step-idx)))
               (nth step-idx (gptel-openai-assistant-session-steps backend))))
            (target-buffer (plist-get info :buffer)))
       (setf 
        (gptel-openai-assistant-session-step backend) new-step
        (gptel-backend-url backend) (with-current-buffer target-buffer
                                      (when new-step
                                        (if-let (step (alist-get new-step gptel-openai-assistant--known-steps))
                                            (let ((url (gptel-openai-assistant-step-url step)))
                                              (em (cl-typecase url
                                                    (string url)
                                                    (function (funcall url))
                                                    (t (error "Unknown value for url (%s) in step (%s)" url new-step)))))
                                          (error "Unknown step %s in update session step" new-step)))))

       (plist-put info :data (gptel--request-data backend (gptel-openai-assistant-session-cached-prompts backend)))
       (plist-put info :wait new-step)))))

(cl-defmethod gptel-backend--on-end-of-state ((backend gptel-openai-assistant-session) state info)
  (when (eq state 'INIT)
    (unless (eq (gptel-openai-assistant-session-step-idx backend) -1)
      (gptel--openai-assistant-reset-backend backend))
    (setf (gptel-openai-assistant-session-steps backend)
          (with-current-buffer (plist-get info :buffer)
            (if gptel-openai-assistant-thread-id
                '(:messages :runs)
              '(:threads :messages :runs))))
    (gptel-openai-assistant--update-session-step backend)))

(defun gptel--openai-assistant-reset-backend (backend)
  (em "resetting openai assistant backend")
  (when (gptel-openai-assistant-session-p backend)
    (setf (gptel-openai-assistant-session-cached-prompts backend) nil
          (gptel-openai-assistant-session-step-idx backend) -1
          (gptel-openai-assistant-session-steps backend) '(:threads :messages :runs))))

(defun gptel-openai-assistant--progress-step (info)
  (plist-put info :wait (length>
                         (gptel-openai-assistant-session-steps (plist-get info :backend))
                         (1+ (gptel-openai-assistant-session-step-idx (plist-get info :backend))))))

;;;###autoload
(defun gptel-openai-assistant-make-session (steps)
  (gptel-openai--make-assistant-session
   :name "gptel-openai-assistant"
   :step (car steps)
   :steps steps
   :host "api.openai.com"
   :key 'gptel-api-key
   :models (gptel--process-models gptel--openai-models)
   :header (lambda () (when-let (key (gptel--get-api-key))
                        `(("Authorization" . ,(concat "Bearer " key))
                          ("OpenAI-Beta" . "assistants=v2"))))
   :protocol "https"
   :endpoint "/v1/threads/thread_id/runs"
   :stream t))

;;;###autoload
(defun gptel-openai-assistant-start-thread ()
  "Start a assistant thread in the current buffer."
  (interactive)
  ;; The info hasn't yet been created either.
  ;; FIXME: is this needed?
  (when (or (not gptel-openai-assistant-thread-id)
            (y-or-n-p "There is already a thread in the buffer. Create a new thread? "))
    (if (gptel-openai-assistant-session-p gptel-backend)
        (setf (gptel-openai-assistant-session-steps gptel-backend) '(:threads))
      (setq-local gptel-backend (gptel-openai-assistant-make-session '(:threads))))
    (setf (gptel-openai-assistant-session-cached-prompts gptel-backend) nil
          (gptel-openai-assistant-session-step-idx gptel-backend) -1)
    (gptel-openai-assistant--update-session-step nil gptel-backend)
    (gptel-request nil :stream t)))

;;;###autoload
(defun gptel-openai-assistant-add-message ()
  "Start a assistant thread in the current buffer."
  (interactive)
  ;; The info hasn't yet been created either.
  ;; FIXME: is this needed?
  (if (gptel-openai-assistant-session-p gptel-backend)
      (setf (gptel-openai-assistant-session-steps gptel-backend) '(:messages))
    (setq-local gptel-backend (gptel-openai-assistant-make-session '(:messages))))
  (setf (gptel-openai-assistant-session-cached-prompts gptel-backend) nil
        (gptel-openai-assistant-session-step-idx gptel-backend) -1)
  (gptel-openai-assistant--update-session-step nil gptel-backend)
  (gptel-request nil :stream t))

;;;###autoload
(defun gptel-openai-assistant-start-run ()
  "Start a assistant thread in the current buffer."
  (interactive)
  ;; The info hasn't yet been created either.
  ;; FIXME: is this needed?
  (if (gptel-openai-assistant-session-p gptel-backend)
      (setf (gptel-openai-assistant-session-steps gptel-backend) '(:runs))
    (setq-local gptel-backend (gptel-openai-assistant-make-session '(:runs))))
  (setf (gptel-openai-assistant-session-cached-prompts gptel-backend) nil
        (gptel-openai-assistant-session-step-idx gptel-backend) -1)
  (gptel-openai-assistant--update-session-step nil gptel-backend)
  (gptel-request nil :stream t))

;;;###autoload
(defun gptel-openai-assistant-send ()
  "Add message to the current thread and run it. Create thread if one is not initialized."
  (interactive)
  (if (gptel-openai-assistant-session-p gptel-backend)
      (setf (gptel-openai-assistant-session-steps gptel-backend)
            (if gptel-openai-assistant-thread-id
                '(:messages :runs)
              '(:threads :messages :runs)))
    (setq-local gptel-backend
                (gptel-openai-assistant-make-session
                 (if gptel-openai-assistant-thread-id
                     '(:messages :runs)
                   '(:threads :messages :runs)))))
  (setf (gptel-openai-assistant-session-cached-prompts gptel-backend) nil
        (gptel-openai-assistant-session-step-idx gptel-backend) -1)
  (gptel-openai-assistant--update-session-step nil gptel-backend)
  (gptel-request nil :stream t))
;; )

(setf (alist-get "openai-assistant" gptel--known-backends
                 nil nil #'equal)
      (gptel-openai-assistant-make-session '(:threads :messages :runs)))


(provide 'gptel-extensions)
;;; orgZ.el ends here
