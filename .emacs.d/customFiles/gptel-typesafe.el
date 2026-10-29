;;; gptel-typesafe.el --- TypeSafe System One for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; System One evaluates named, typed questions about supplied state; it is
;; not a chat model.  This extension uses gptel's transport and request FSM.
;; Callbacks receive JSON text on success, nil on failure, or `abort'.
;; Parsed results are available in INFO under :typesafe-response and
;; :typesafe-answers.  JSON arrays must be vectors; objects may be plists
;; or hash tables as accepted by `gptel--json-encode'.
;; Images use JSON assets with inline base64 data; multipart is unsupported.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'gptel-request)

(defgroup gptel-typesafe nil
  "TypeSafe System One requests through gptel."
  :group 'gptel)

(defcustom gptel-typesafe-key nil
  "TypeSafe API key, or a function returning it.
A symbol naming a variable containing the key is also accepted.  With nil,
gptel's normal API-key/auth-source lookup is used for api.typesafe.ai."
  :type '(choice (const nil) string function symbol)
  :group 'gptel-typesafe)

(cl-defstruct (gptel-typesafe
               (:include gptel-backend)
               (:constructor gptel-typesafe--create))
  "System One backend, including request-specific options."
  questions assets images long-context state state-supplied-p)

(defun gptel-typesafe--headers (info)
  "Return authentication and feature headers for request INFO."
  (let* ((backend (plist-get info :backend))
         (gptel-backend backend)
         (key (gptel--get-api-key))
         (questions (plist-get (plist-get info :data) :questions))
         (box-p nil))
    (if (hash-table-p questions)
        (maphash (lambda (_name question)
                   (when (equal (plist-get question :type) "box")
                     (setq box-p t)))
                 questions)
      (cl-loop for (_name question) on questions by #'cddr
               when (equal (plist-get question :type) "box")
               do (setq box-p t)))
    (unless (and (stringp key) (not (string-empty-p key)))
      (user-error "No TypeSafe API key configured"))
    (append
     (list (cons "Authorization" (concat "Bearer " key)))
     (when (or box-p (gptel-typesafe-images backend)
               (plist-member (plist-get info :data) :assets))
       '(("TypeSafe-Danger-Zone-Images" . "true")))
     (when (gptel-typesafe-long-context backend)
       '(("TypeSafe-Danger-Zone-Long-Context" . "true"))))))

(cl-defun gptel-make-typesafe
    (name &key (key 'gptel-typesafe-key)
          (models '(jev-latest jev-preview)) questions assets
          images long-context curl-args register)
  "Create a TypeSafe backend named NAME.
KEY follows gptel's key convention.  MODELS contains model names or specs.
QUESTIONS is a nonempty named-question object (a plist or hash table).
Each question is a plist with :type and optional :instructions/:criteria.
ASSETS is an optional named image-asset object using inline base64 data.
IMAGES and LONG-CONTEXT enable the respective danger-zone headers.
CURL-ARGS supplies additional curl options.  REGISTER, when non-nil, makes
this backend selectable in gptel; by default it remains unregistered.
For direct `gptel-request' calls, the prompt buffer is sent as text state.
Use `gptel-system-one' instead to send structured state."
  (let ((backend
         (gptel-typesafe--create
          :name name :host "api.typesafe.ai" :protocol "https"
          :endpoint "/v1/systemone"
          :url "https://api.typesafe.ai/v1/systemone"
          :header #'gptel-typesafe--headers :key key
          :models (gptel--process-models models) :stream nil
          :curl-args curl-args :questions questions :assets assets
          :images images :long-context long-context)))
    (when register
      (setf (alist-get name gptel--known-backends nil nil #'equal) backend))
    backend))

(cl-defmethod gptel--parse-buffer ((_backend gptel-typesafe) _max-entries)
  "Use the current buffer up to point as System One text state.
Chat turn markers are not interpreted; MAX-ENTRIES is not applicable."
  (buffer-substring-no-properties (point-min) (point)))

(cl-defmethod gptel--parse-list ((_backend gptel-typesafe) prompts)
  "Represent legacy conversation PROMPTS as a JSON array of strings."
  (unless (cl-every #'stringp prompts)
    (user-error "TypeSafe prompt lists must contain strings"))
  (vconcat prompts))

(cl-defmethod gptel--request-data ((backend gptel-typesafe) prompts)
  "Build a System One payload from BACKEND and PROMPTS."
  (when (and gptel-use-tools gptel-tools)
    (user-error "TypeSafe System One does not support chat tools"))
  (when gptel--schema
    (user-error "TypeSafe uses typed questions, not a JSON output schema"))
  (let ((questions (gptel-typesafe-questions backend)))
    (unless (or (and (hash-table-p questions)
                     (> (hash-table-count questions) 0))
                (and (consp questions) (keywordp (car questions))
                     (plistp questions) (zerop (% (length questions) 2))))
      (user-error "TypeSafe questions must be a nonempty plist or hash table"))
    (let ((data
           (list :model (gptel--model-name gptel-model)
                 :state (if (gptel-typesafe-state-supplied-p backend)
                            (gptel-typesafe-state backend)
                          prompts)
                 :questions questions)))
      (when (gptel-typesafe-assets backend)
        (plist-put data :assets (gptel-typesafe-assets backend)))
      data)))

(cl-defmethod gptel--parse-response ((_backend gptel-typesafe) response info)
  "Return RESPONSE as JSON text and store typed results in INFO.
Malformed success responses set :error, allowing the FSM to fail normally."
  (if (not (and (listp response)
                (stringp (plist-get response :model))
                (consp (plist-get response :answers))
                (listp (plist-get response :usage))
                (numberp (plist-get (plist-get response :usage) :input_tokens))
                (numberp (plist-get (plist-get response :usage) :output_tokens))))
      (progn
        (plist-put info :error "Malformed TypeSafe response: expected model, answers and usage")
        nil)
    (plist-put info :typesafe-response response)
    (plist-put info :typesafe-answers (plist-get response :answers))
    (let ((tokens (list :input (plist-get (plist-get response :usage) :input_tokens)
                        :output (plist-get (plist-get response :usage) :output_tokens)
                        :cached 0)))
      (plist-put info :tokens tokens)
      (plist-put info :tokens-full tokens))
    (gptel--json-encode response)))

(cl-defun gptel-system-one
    (state questions &key callback backend (model 'jev-latest)
           (assets nil assets-p) (images nil images-p)
           (long-context nil long-context-p) context dry-run)
  "Evaluate QUESTIONS about STATE using TypeSafe System One.
STATE is a string, JSON object (plist/hash table), or JSON array (vector).
QUESTIONS is a nonempty plist or hash table of named question plists.
BACKEND defaults to a new, unregistered TypeSafe backend.  MODEL names the
model to use.  ASSETS, IMAGES and LONG-CONTEXT override corresponding
backend options when supplied, including when explicitly nil.
CALLBACK follows `gptel-request': JSON text on success, nil on failure,
or `abort', plus an INFO plist.  Typed answers and the full parsed result
are in INFO under :typesafe-answers and :typesafe-response.  Without a
callback, gptel inserts the JSON response at point.
CONTEXT is callback context, not model state.  DRY-RUN builds the request
without sending it.  Return gptel's request FSM in either case.
Each call copies BACKEND so concurrent calls do not share request options.
No chat system prompt, tools, context injection or transforms are inherited."
  (unless (or (stringp state) (hash-table-p state)
              (and (vectorp state) (not (stringp state)))
              (and (consp state) (keywordp (car state)) (plistp state)))
    (user-error "TypeSafe state must be a string, object, or vector"))
  (unless (or (null backend) (gptel-typesafe-p backend))
    (user-error "BACKEND must be a gptel-typesafe backend"))
  (unless (or (and (stringp model) (not (string-empty-p model)))
              (and model (symbolp model)))
    (user-error "MODEL must be a nonempty string or non-nil symbol"))
  (let* ((request-backend (copy-gptel-typesafe
                           (or backend (gptel-make-typesafe "TypeSafe"))))
         (request-model (if (symbolp model) model (gptel--intern model))))
    (setf (gptel-typesafe-state request-backend) (copy-tree state t)
          (gptel-typesafe-state-supplied-p request-backend) t
          (gptel-typesafe-questions request-backend) (copy-tree questions t))
    (when assets-p
      (setf (gptel-typesafe-assets request-backend) (copy-tree assets t)))
    (when images-p
      (setf (gptel-typesafe-images request-backend) images))
    (when long-context-p
      (setf (gptel-typesafe-long-context request-backend) long-context))
    (unless (memq request-model (gptel-backend-models request-backend))
      (setf (gptel-backend-models request-backend)
            (cons request-model (gptel-backend-models request-backend))))
    (let ((gptel-backend request-backend)
          (gptel-model request-model))
      (gptel-request
       "" :callback callback :context context :dry-run dry-run :stream nil
       :system nil
       :transforms
       (list (lambda ()
               ;; The prompt buffer copies buffer-local values from the source
               ;; buffer.  Set these here, rather than relying on let bindings
               ;; alone surviving that copy.
               (setq-local gptel-backend request-backend
                           gptel-model request-model
                           gptel-system-prompt nil
                           gptel-use-tools nil
                           gptel-use-context nil
                           gptel--schema nil
                           gptel-stream nil)))))))

(provide 'gptel-typesafe)
;;; gptel-typesafe.el ends here
