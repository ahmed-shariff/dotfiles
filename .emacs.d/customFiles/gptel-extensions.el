;;; llm                     -*- lexical-binding: t; -*-

;;; Commentary:
;;my custom org setup
;;; Code:

(require 'gptel)

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


(use-package gptel-openai-assistant
  :after gptel
  :straight (gptel-openai-assistant :type git :host github :repo "ahmed-shariff/gptel-openai-assistant")
  :config
  (setf (alist-get "openai-assistant" gptel--known-backends
                   nil nil #'equal)
        (gptel-make-openai-assistant "openai-assistant" :key (gptel--get-api-key))
        gptel-openai-assistant-assistant-id (gethash 'openai-assistant-id configurations))

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

  (add-to-list 'gptel-post-response-functions #'amsha/gptel--replace-file-id-with-cite))



(provide 'gptel-extensions)
;;; orgZ.el ends here
