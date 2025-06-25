;;; textops.el --- Text Operations: Tense, Tone, Merger, etc via LLM -*- lexical-binding: t; -*-

;; Requires: gptel.el

;;; Commentary:
;; Provides interactive commands for text operations using LLMs, routing requests via `gptel-request'
;; as per prompt templates from the masson25_textos paper.

;;; Code:

(require 'gptel)
(require 'json)

(defun textops--region-or-line ()
  "Return region text or current line's text trimmed."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (string-trim
     (thing-at-point 'line t))))

(defun textops--insert-or-show (response _info)
  "Insert RESPONSE at point or display it."
  (if (and response (stringp response))
      (progn
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert response))
    (message "No response: %s" response)))

;;;; TextTenseChanger
(defun textops-text-tense-changer (text tense)
  "Rewrite using TEXT, changing segment between START and END to TENSE."
  (interactive
   (let* ((main-text (textops--region-or-line))
          (target (read-string "Text to change tense: "))
          (tense (completing-read "Tense: " '("past" "present" "future"))))
     (list start end target tense)))
  (let* ((label (format "PART_TO_%s" (upcase tense)))
         (prompt (format "%s: %s\n\nRewrite this text by changing %s to the %s tense."
                         label
                         (string-trim text)
                         label
                         tense)))
    (gptel-request prompt :callback #'textops--insert-or-show)))

;;;; TextConsolidater
(defun textops-text-consolidater ()
  "Combine region/line into one sentence."
  (interactive)
  (let* ((text (textops--region-or-line))
         (prompt (format "%s\n\nCombine this text into one sentence." text)))
    (gptel-request prompt :callback #'textops--insert-or-show)))

;; ;;;; TextReorganizer
;; (defun textops-text-reorganizer ()
;;   "Reorganize words in region/line in 4 ways."
;;   (interactive)
;;   (let* ((text (textops--region-or-line))
;;          (prompt (format "%s\n\nReorganize the words in 4 different ways." text)))
;;     (gptel-request prompt :callback #'textops--insert-or-show)))

;;;; TextToneChanger
(defun textops-text-tone-changer ()
  "Rewrite sentence with different tones."
  (interactive)
  (let* ((text (textops--region-or-line))
         (num-tones (read-number "Number of tone adjustments: " 1))
         (scales
          (cl-loop repeat num-tones
                   collect (list :lowAdj (read-string "Low adjective (e.g. 'casual'): ")
                                 :highAdj (read-string "High adjective: ")
                                 :initialValue (read-number "Current value (0–10): ")
                                 :target (read-number "Target value (0–10): ")))))
         (scale-lines (mapconcat
                       (lambda (s)
                         (format "- On a scale from 0 being %s and 10 being %s, this is currently a %d => change it to a %d by making it more %s"
                                 (plist-get s :lowAdj)
                                 (plist-get s :highAdj)
                                 (plist-get s :initialValue)
                                 (plist-get s :target)
                                 (if (> (plist-get s :target) (plist-get s :initialValue))
                                     (plist-get s :highAdj) (plist-get s :lowAdj))))
                       scales "\n"))
         (prompt (format "%s\n\nRewrite the sentence with the following tones:\n%s\n\nRespond with a JSON object with the property 'sentence'"
                         text scale-lines)))
    (gptel-request prompt
                   :callback
                   (lambda (response _info)
                     (if (string-prefix-p "{" (string-trim-left response))
                         (let* ((json-object-type 'plist)
                                (res (ignore-errors (json-read-from-string response))))
                           (if-let ((sentence (plist-get res :sentence)))
                               (textops--insert-or-show sentence nil)
                             (insert response)))
                       (textops--insert-or-show response nil)))))

;;;; TextDistributer
(defun textops-text-distributer ()
  "Break text into sentences, one per idea."
  (interactive)
  (let* ((text (textops--region-or-line))
         (prompt (format "%s\n\nBreak this text into multiple sentences. One sentence per idea." text)))
    (gptel-request prompt :callback #'textops--insert-or-show)))

;;;; TextTonePicker
(defun textops-text-tone-picker ()
  "Rate a sentence on N scales."
  (interactive)
  (let* ((text (textops--region-or-line))
         (num (read-number "Number of scales (N): "))
         (scaledefs
          (cl-loop for i from 1 to num
                   collect (list
                            :key (read-string (format "Scale name #%d: " i))
                            :low (read-string "Low adjective: ")
                            :high (read-string "High adjective: "))))
         (scale-summary (mapconcat
                         (lambda (sc)
                           (format "{%s: /* rating on scale from 0 being %s to 10 being %s */}"
                                   (plist-get sc :key) (plist-get sc :low) (plist-get sc :high)))
                         scaledefs ", "))
         (prompt (format "%s\n\nRate this sentence on N scales. Respond using JSON like\n%s"
                         text scale-summary)))
    (gptel-request prompt :callback (lambda (resp _info) (insert resp)))))

;;;; TextPluralizer
(defun textops-text-pluralizer (start end text to-plural)
  "Pluralize or singularize a segment."
  (interactive
   (let* ((start (read-string "Text before part to plural/singular (empty for none): "))
          (target (read-string "Text to pluralize/singularize: "))
          (end (read-string "Text after: "))
          (op (completing-read "Operation: " '("pluralize" "singularize"))))
     (list start end target (string= op "pluralize"))))
  (let* ((label (if to-plural "PART_TO_PLURAL" "PART_TO_SINGULAR"))
         (opword (if to-plural "pluralizing" "singularizing"))
         (prompt (format "%s %s %s\n\n%s: %s\n\nRewrite this text by %s %s."
                         (string-trim start)
                         label
                         (string-trim end)
                         label (string-trim text)
                         opword label)))
    (gptel-request prompt :callback #'textops--insert-or-show)))

;; gptel-rewrite?
;; ;;;; TextPrompter
;; (defun textops-text-prompter ()
;;   "Rewrite <blank> in a text as per instruction."
;;   (interactive)
;;   (let* ((previousText (read-string "Text before blank: "))
;;          (input (read-string "Text for <blank>: "))
;;          (followingText (read-string "Text after blank: "))
;;          (action (read-string "INSTRUCTION: ")))
;;     (let ((prompt (format "%s <blank> %s\n<blank>: %s\n\nINSTRUCTION: %s\nRewrite <blank>. Follow INSTRUCTION\n<blank>:"
;;                           previousText followingText input action)))
;;       (gptel-request prompt :callback #'textops--insert-or-show))))

;; ;;;; TextSmudger
;; (defun textops-text-smudger ()
;;   "Return a rewritten version of text."
;;   (interactive)
;;   (let* ((text (textops--region-or-line))
;;          (prompt (format "%s\n\nReturn a rewritten version." text)))
;;     (gptel-request prompt :callback #'textops--insert-or-show)))

;;;; TextResizer
(defun textops-text-resizer ()
  "Shorten or lengthen each sentence in region/line to a target length."
  (interactive)
  (let* ((text (textops--region-or-line))
         (sentences (split-string text "\\(?:[.!?]\\)[ \n]+" t))
         (do-shorten (y-or-n-p "Shorten? (otherwise lengthen): "))
         (len (read-number "Target length in words: "))
         (num (read-number
               (if do-shorten "Remove how many words? " "Add how many words? ")))
         (prompts
          (mapcar (lambda (s)
                    (format "%s\n\n%s to %d words by %s %d words."
                            s
                            (if do-shorten "Shorten" "Lengthen")
                            len
                            (if do-shorten "removing" "adding")
                            num))
                  sentences)))
    (dolist (prompt prompts)
      (gptel-request prompt :callback #'textops--insert-or-show))))

;;;; TextRepair
(defun textops-text-repair ()
  "Fix the grammar of region or line."
  (interactive)
  (let* ((text (textops--region-or-line))
         (prompt (format "%s\n\nFix the grammar." text)))
    (gptel-request prompt :callback #'textops--insert-or-show)))

;;;; TextMerger
(defun textops-text-merger ()
  "Merge region/line with other text."
  (interactive)
  (let* ((a (read-string "Text A: "))
         (b (read-string "Text B: "))
         (op (completing-read
              "Operation: "
              '(("merge" . "Write the sentence resulting from merging A and B.")
                ("both" . "Write a sentence using ONLY the elements that appear in both A and B.")
                ("subtract" . "Write the sentence resulting from subtracting A from B. The idea expressed from A should be removed. Feel free to update the rest of the text so that it still make sense.")
                ("exclude" . "Write the sentence resulting from excluding A from B."))))
         (operationPrompt (cdr (assoc op
                                      '(("merge" . "Write the sentence resulting from merging A and B.")
                                        ("both" . "Write a sentence using ONLY the elements that appear in both A and B.")
                                        ("subtract" . "Write the sentence resulting from subtracting A from B. The idea expressed from A should be removed. Feel free to update the rest of the text so that it still make sense.")
                                        ("exclude" . "Write the sentence resulting from excluding A from B.")))))))
    (let ((prompt (format "A: %s\nB: %s\n\n%s" a b operationPrompt)))
      (gptel-request prompt :callback #'textops--insert-or-show)))

;;;; TextEraser
(defun textops-text-eraser ()
  "Remove a segment without adding new words."
  (interactive)
  (cl-assert (use-region-p) nil "No region selected.")
  (let* ((start (read-string "Text before part to delete (empty for none): "))
         (end   (read-string "Text after (empty for none): "))
         (part-to-remove (buffer-substring-no-properties (region-beginning) (region-end)))
         (full-sentence (thing-at-point 'line t))
         (prompt (format "PART_TO_REMOVE: %s FULL_SENTENCE: %s\n\nRemove PART_TO_REMOVE from the FULL_SENTENCE without adding new words. You can reorganize the words and the sentence, but you can't add new words."
                         (string-trim start)
                         (string-trim end))))
    ;; TODO: set the region to replace to the whole sentence.
    (gptel-request prompt :callback #'textops--insert-or-show)))

;;; Command menu

;;;###autoload
(defvar textops-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'textops-text-tense-changer)
    (define-key map (kbd "c") #'textops-text-consolidater)
    (define-key map (kbd "o") #'textops-text-tone-changer)
    (define-key map (kbd "d") #'textops-text-distributer)
    (define-key map (kbd "g") #'textops-text-repair)
    (define-key map (kbd "m") #'textops-text-merger)
    (define-key map (kbd "l") #'textops-text-pluralizer)
    (define-key map (kbd "s") #'textops-text-smudger)
    (define-key map (kbd "e") #'textops-text-eraser)
    (define-key map (kbd "z") #'textops-text-resizer)
    (define-key map (kbd "k") #'textops-text-tone-picker)
    map)
  "Keymap for TextOps commands.")

;;;###autoload
(defun textops-command-dispatch ()
  "Prompt for a text operation command."
  (interactive)
  (let* ((cmpl (let ((minibuffer-allow-text-properties t))
                 (mapcar #'car (where-is-internal textops-command-map nil t))))
         (choice (completing-read "Text operation: " cmpl nil t)))
    (call-interactively (lookup-key textops-command-map (kbd choice)))))

(global-set-key (kbd "C-c o q t") textops-command-map)

(provide 'textops)

;;; textops.el ends here
