;;; transactions-tables.el                     -*- lexical-binding: t; -*-

;;; Commentary:
;;my custom org setup
;;; Code:

(require 'tabulated-list)
(require 'map)
(require 'configurations)

(define-derived-mode transactions-tables-mode tabulated-list-mode "MyTable"
  "boo"
 (setq tabulated-list-format [("Date" 20 t)
                               ("Logo SRC" 10 t)
                               ("Logo ALT" 20 t)
                               ("Details" 40 t)
                               ("Amount" 20 t)])
 (setq tabulated-list-padding 3)
 (tabulated-list-print)
 (tabulated-list-init-header)
 (let ((inhibit-read-only t))
   (goto-char (point-max))
   (let ((idx 0))
     (insert "\n  " (propertize
                     (apply #'concat
                            (mapcar (lambda (el)
                                      (format "%s - %s; " (cl-incf idx) el))
                                    (em transactions-tables-current-categories)))
                     'details t)))))

(defvar-keymap transactions-tables-mode-map
  :doc "Keymap for `transactions-tables-mode'."
  ;; :parent tabulated-list-mode-map
  "n" #'next-line
  "p" #'previous-line
  "j" #'next-line
  "k" #'previous-line
  "s" #'transactions-tables-set-category-table
  "1" #'transactions-tables-category-1
  "2" #'transactions-tables-category-2
  "3" #'transactions-tables-category-3
  "4" #'transactions-tables-category-4
  "5" #'transactions-tables-category-5
  "6" #'transactions-tables-category-6
  "7" #'transactions-tables-category-7
  "8" #'transactions-tables-category-8
  "9" #'transactions-tables-category-9)

(defvar transactions-tables-categories (gethash 'transactions-tables-categories-alist configurations))

(defvar transactions-tables-current-categories nil)

;;;###autoload
(defun transactions-tables-set-category-table ()
  (interactive nil transactions-tables-mode)
  (setq transactions-tables-current-categories
        (alist-get (completing-read "Cetegory table: " (map-keys transactions-tables-categories))
                   transactions-tables-categories
                   nil nil #'string-equal))
  (transactions-tables-mode))

;;;###autoload
(defun transactions-tables-unmark-all ()
  (interactive nil transactions-tables-mode)
  (save-excursion
    (goto-char (point-min))
    (bookmark-bmenu-ensure-position)
    (while (not (eobp))
      (tabulated-list-put-tag " " t))))

;;;###autoload
(defun transactions-tables-category-1 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "1" t))

;;;###autoload
(defun transactions-tables-category-2 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "2" t))

;;;###autoload
(defun transactions-tables-category-3 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "3" t))

;;;###autoload
(defun transactions-tables-category-4 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "4" t))

;;;###autoload
(defun transactions-tables-category-5 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "5" t))

;;;###autoload
(defun transactions-tables-category-5 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "5" t))

;;;###autoload
(defun transactions-tables-category-6 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "6" t))

;;;###autoload
(defun transactions-tables-category-7 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "7" t))

;;;###autoload
(defun transactions-tables-category-8 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "8" t))

;;;###autoload
(defun transactions-tables-category-9 ()
  (interactive nil transactions-tables-mode)
  (tabulated-list-put-tag "9" t))

;;;###autoload
(defun amsha/transactions-table ()
  (interactive)
  (when-let (buff (get-buffer "*transactions table*"))
    (kill-buffer buff))
  (switch-to-buffer "*transactions table*")
  (let ((items))
    (with-temp-buffer
      (clipboard-yank)
      (goto-char (point-min))
      (condition-case nil
          (while (re-search-forward "\\(.*\\);\\(.*\\);\\(.*\\);\\(.*\\);\\(.*\\)")
            (unless (string= "Date" (match-string 1))
              (push (list (match-string 0)
                          (vector (match-string 1)
                                  (match-string 2)
                                  (match-string 3)
                                  (match-string 4)
                                  (match-string 5)))
                    items)))
        (search-failed nil)))
    (setq tabulated-list-entries items))
  (transactions-tables-mode))

(defun amsha/transactions-table-process ()
  (interactive)
  (goto-char (point-min))
  (let ((items (append '(nil) (--map nil transactions-tables-current-categories)))
        (new-buf "*transaction-table-processed*")
        cmd)
   (while (not (eobp))
     (setq cmd (char-after))
     (when (and (not (eq cmd ?\s))
                (tabulated-list-get-id))
       (dolist (idx (number-sequence 1 9))
         (when (equal cmd (string-to-char (number-to-string idx)))
           (--> (aref (cadr
                       (assoc (tabulated-list-get-id)
                              tabulated-list-entries #'string-equal))
                      4)
                (string-replace "$" "" it)
                (push (* (if (eq (aref it 0) ?−) -1 1)
                             (string-to-number (string-replace "−" "" it)))
                      (nth idx items))))))
     (forward-line))
   (with-current-buffer (get-buffer-create new-buf)
     (setq tabulated-list-format [("Category" 20 t)
                                  ("Items" 40 t)
                                  ("Sum" 20 t)])
     (setq tabulated-list-entries
           (--map
            (list "" (vector (car it)
                             (format "%s" (cdr it))
                             (format "%s" (apply #'+ (cdr it)))))
            (-zip transactions-tables-current-categories
                  (cdr items))))
     (tabulated-list-print)
     (tabulated-list-mode)
     (tabulated-list-init-header))
   (display-buffer new-buf)))

(provide 'transactions-tables)
;;; transactions-tables.el ends here
