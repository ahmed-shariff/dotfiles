;;; package -- Summary  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; -*- emacs-lisp -*-
;; -*- lexical-binding: t -*-
;; 
;; (require 'package)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;mepla setup****************************************************
;; (add-to-list 'package-archives
;; 	     '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("org" . "http://orgmode.org/elpa/") t);'("elpy" . "http://jorgenschaefer.github.io/packages/"))
;	     '("melpa" . "http://melpa.org/packages/")
;	     '("org" . "http://orgmode.org/elpa/"))
; (add-to-list 'load-path "~/.emacs.d/customFiles")

;; (setq load-prefer-newer t) ;;mkaing sure older byte compiled files are not loaded

(setq-default default-directory "~/")

(let ((default-directory  "~/.emacs.d/customFiles/"))
  (normal-top-level-add-to-load-path `("."))
  (normal-top-level-add-subdirs-to-load-path))
;; (add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; (when (< emacs-major-version 24)
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))


;; ;;auto-compile *****************************************************
;; ;; To make sure newer files are being byte compiled 
;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

;(package-initialize)      ;; Initialize & Install Package


(defun maximize (&optional f)
  (interactive)
  ;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
  ;; 	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  ;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
  ;; 	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
  ;; (toggle-frame-maximized))
  (set-frame-parameter nil 'fullscreen 'maximized))

(defun enlarge-window-horizontally-to (percentage)
  "Enlarge window horizontally by PERCENTAGE."
  (interactive (list (string-to-number (completing-read "Percentage: " '("10" "20" "50" "60" "75" "85")))))
  (enlarge-window-horizontally (truncate (- (* (frame-width) (/ (float percentage) 100)) (window-width)))))

(defvar made-transparent nil)

(defun toggle-frame-alpha ()
  "Toggle frame transparency."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha (if made-transparent '(100 . 100) '(98 . 92)))
  (setq made-transparent (not made-transparent)))

(maximize)
(toggle-frame-alpha)
(add-hook 'after-make-frame-functions 'maximize)
(setq ns-auto-hide-menu-bar t)
(tool-bar-mode 0)

(server-start)
(require 'configurations)

;;straight.el setup*************************************************
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; for emacs >= 27
(setq straight-use-package-by-default t
      package-enable-at-startup nil
      straight-recipes-gnu-elpa-use-mirror t
      straight-vc-git-default-protocol 'https
      straight-host-usernames '((github . "ahmed-shariff"))
      backup-directory-alist `(("." . "~/.backups_emacs"))
      backup-by-copying t
      delete-old-versions t
      fill-column 80
      ;; Following vertico readme
      read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t
      confirm-kill-emacs #'y-or-n-p
      ;;tramp settings ***************************************************
      ;; See https://stackoverflow.com/questions/6954479/emacs-tramp-doesnt-work for more details
      tramp-terminal-type "dumb"
      initial-buffer-choice "*splash*"
      inhibit-startup-screen t
      split-width-threshold 0
      split-height-threshold nil
      banner-text-list (string-split
                        (with-temp-buffer
                         (insert-file "~/.emacs.d/customFiles/banner_text.txt")
                         (buffer-string))
                        "\n")
      banner-images-list (append
                          (directory-files
                           (file-truename "~/.emacs.d/.cache/doom-banners/splashes/gnu-sm/")
                           t "png")
                          (directory-files
                           (file-truename "~/.emacs.d/.cache/doom-banners/splashes/emacs/")
                           t "png"))
      )

;; ;;enabling company***********************************************
;; (add-hook 'after-init-hook 'global-company-mode)
;; ;; ;;makes completion start automatically rather than waiting for 3 chars / 0.5sec
;; (setq company-minimum-prefix-length 1)
;; (setq company-idle-delay 0.1)
;;;; GC issue
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 100000000))) ;; This is set in lsp-mode
(add-hook 'focus-out-hook 'garbage-collect)
;; ;; ;;company quickhelp gives docstring info
;; (company-quickhelp-mode 1)
;; (setq company-quickhelp-delay nil)


(defvar my-package-list '(org org-contrib elgrep
					   ;; org-capture-pop-frame
					   use-package spaceline-all-the-icons
					   org-bullets latex-math-preview csproj-mode plantuml-mode
					   docker dockerfile-mode ascii-art-to-unicode org-ref yasnippet-snippets 2048-game
					   expand-region diminish amx flx
					   dashboard dired-single ibuffer-vc projectile micgoline dired-hide-dotfiles
					   dired-sidebar stumpwm-mode hledger-mode vlf elpy
					   yasnippet jedi sr-speedbar latex-preview-pane slime slim-mode))

(mapcar #'straight-use-package
	my-package-list)

;; (defun straight-visit-package-projectile (&rest args)
;;   "Open projectile after visiting straight repo."
;;   (let ((b (current-buffer))
;;         (p (persp-current-name)))
;;     (projectile-find-file)
;;     (persp-add-buffer b)
;;     (with-perspective p
;;       (persp-remove-buffer b))))

;; (advice-add 'straight-visit-package :after #'straight-visit-package-projectile)
;; (advice-remove 'straight-visit-package #'straight-visit-package-projectile)

;; custom variables*******************************************
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(bookmark-save-flag 1)
 ;; '(company-c-headers-path-system
   ;; '("/usr/include/" "/usr/local/include/" "/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/"))
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(ede-project-directories '("/media/Files/Research/FoodClassification/deployment"))
 '(elpy-rpc-python-command "python3")
 '(explicit-shell-file-name "/bin/zsh")
 '(helm-minibuffer-history-key "M-p")
 '(ledger-post-amount-alignment-at :decimal)
 '(ledger-reconcile-default-commodity nil)
 '(ledger-reports
   '(("asd" "ledger")
     ("a" "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-export-backends '(ascii html icalendar latex md))
 '(prolog-system 'swi)
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           '(("^\\* .*\\(([0-9]\\{4\\})\\)" 1 'org-tag t)
             ("^\\* .*\\(([0-9]\\{4\\})\\).*\\(\\[.*\\]\\)" 2 'org-level-7 t)
             ("`\\([a-zA-Z].*[a-zA-Z\\.]\\)`" 1 'org-quote t))
           'append)
     (org-download-image-dir . "../figures/notes")
     (eval font-lock-add-keywords nil
           '(("^\\* .*\\(([0-9]\\{4\\})\\)" 1 'org-tag t)
             ("^\\* .*\\(([0-9]\\{4\\})\\).*\\(\\[.*\\]\\)" 2 'org-tag t)
             ("`\\([a-zA-Z].*[a-zA-Z\\.]\\)`" 1 'org-quote t))
           'append)
     (dired-omit-files . "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.log$")
     (magit-todos-exclude-globs "Assets/Oculus/" "Assets/TextMesh Pro/")
     (magit-todos-exclude-globs "Assets/Oculus/")
     (magit-todos-exclude-globs . "Assets/Oculus/")
     (org-download-image-dir . "figures/notes")
     (org-download-image-dir . "figures")))
 '(sml/mode-width 15)
 '(sml/shorten-modes t)
 '(sml/theme 'dark)
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(company-scrollbar-bg ((t (:background "#000000"))) t)
 ;; '(company-scrollbar-fg ((t (:background "#555555"))) t)
 ;; '(company-tooltip ((t (:inherit default :background "#000000"))))
 ;; '(company-tooltip-annotation ((t (:inherit font-lock-builtin-face))))
 ;; '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 ;; '(company-tooltip-selection ((t (:inherit highlight))))
 '(markdown-code-face ((t (:inherit consolas))))
 '(org-block-begin-line ((t (:background "#112424" :overline t))))
 '(org-block-end-line ((t (:background "#112424" :overline nil :underline t))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "dark turquoise"))))
 '(org-special-keyword ((t (:inherit outline-1 :foreground "sienna")))))

(setq-default indent-tabs-mode nil)

;(exec-path-from-shell-initialize)

;;allout
(allout-mode)

;;syntax highlight
(global-font-lock-mode 1)

;;add proper word wrapping
(global-visual-line-mode t)

(electric-pair-mode 1)

(global-hl-line-mode 1)

(setq view-read-only t)

(repeat-mode 1)

(customize-set-value 'create-lockfiles nil "It's not being ignored propperly?")

(add-hook 'prog-mode-hook
    (lambda ()
      ;; (linum-on)
      (setq indent-tabs-mode nil)
      (infer-indentation-style)))

;;fix for issues with ACL on WLS ***********************************
;; from https://github.com/microsoft/WSL/issues/6004
(when (eq system-type 'windows-nt)
  (defun fp/ignore-wsl-acls (orig-fun &rest args)
    "Ignore ACLs on WSL. WSL does not provide an ACL, but emacs
expects there to be one before saving any file. Without this
advice, files on WSL can not be saved."
    (if (string-match-p "^//wsl\$/" (car args))
        (progn (message "ignoring wsl acls") "")
      (apply orig-fun args)))

  (advice-add 'file-acl :around 'fp/ignore-wsl-acls))

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun yank-pop-forwards (arg)
  "Ha ha ha. 
ARG : the arg"
  (interactive "p")
  (yank-pop (- arg)))

(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)

;; from https://emacs.stackexchange.com/questions/32140/python-mode-indentation
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
   Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

;; https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
(defun amsha/lookup-key-prefix (key)
  "Search for KEY as prefix in all known keymaps.

E.g.: (amsha/lookup-key-prefix (kbd \"C-c o o\"))"
  (let (vals)
    (mapatoms (lambda (ob)
                (when (and (boundp ob) (keymapp (symbol-value ob)))
                  (when (let ((m (lookup-key (symbol-value ob) key)))
                          (and m (or (symbolp m) (keymapp m))))
                    (push ob vals))
                  (when (functionp (lookup-key (symbol-value ob) key))
                    (push ob vals))))
              obarray)
    (-uniq vals)))

;; (setq use-package-compute-statistics t)

;; from https://karthinks.com/software/it-bears-repeating/
(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

(defvar em-error nil "If non-nil the `em' macro will signal error")

(defun toggle-em-error ()
  "Toggle `em-error'."
  (setf em-error (not em-error)))

(defmacro em (&rest args)
  "Call `messaage' ARGS passed as args of `message' & retur the first argument passed.
Used for debugging."
  ;; `(signal 'error ""))
  (unless (featurep 's) (require 's))
  (unless (featurep 'dash) (require 'dash))
  `(progn
     (when em-error
       (signal 'error "em is erroring"))
     (message ,(format ">>>>>    %s" (s-join "," (-repeat (length args) " %s"))) ,@args)
     ,(car args)))

(defmacro plist-multi-put (plist &rest args)
  "Put KEY VALUES list in setq."
  (let ((list nil)
        (plist-sym (gensym)))
    (while args
      (push `(setq ,plist-sym (plist-put ,plist-sym ,(pop args) ,(pop args))) list))
    `(let ((,plist-sym ,plist))
       ,@(nreverse list)
       ,plist-sym)))

;; from https://github.com/daviwil/dotfiles/blob/master/Emacs.org
(defun amsha/visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun amsha/show-startup ()
  (with-current-buffer (get-buffer-create initial-buffer-choice)
    (let ((inhibit-read-only t)
          (fancy-splash-image (nth (random (length banner-images-list)) banner-images-list))
          (visual-fill-column-width 200)
          (footer-text (nth (random (length banner-text-list)) banner-text-list)))
      (amsha/visual-fill)
      (erase-buffer)
      (setq default-directory "~/")
      (make-local-variable 'startup-screen-inhibit-startup-screen)
      (fancy-splash-head)
      (insert "\n"
              ;; copied from `fancy-splash-head'
              ;; (insert (propertize " " 'display
              ;;                     `(space :align-to (+ ,(- (/ (length footer-text) 2)
              ;;                                              3)
              ;;                                          (-0.5 . ,footer-text)))))
              (propertize footer-text 'face 'font-lock-doc-face
                          ;; copied from `dashboard-center-text'
                          'line-prefix (propertize " " 'display `(space . (:align-to (- center ,(/ (float visual-fill-column-width) 2)))))
                          'indent-prefix (propertize " " 'display `(space . (:align-to (- center ,(/ (float visual-fill-column-width) 2))))))
              "\n\n"))
    (use-local-map splash-screen-keymap)
    (setq tab-width 22
	  buffer-read-only t)
    (set-buffer-modified-p nil)
    (if (and view-read-only (not view-mode))
	(view-mode-enter nil 'kill-buffer))
  (switch-to-buffer initial-buffer-choice)
  (goto-char (point-max))))

(add-hook 'emacs-startup-hook #'amsha/show-startup)

(defun visual-fill-set-width-buffer-local (width)
  "Set visual fill column width for this buffer."
  (interactive (list (read (read-string (format "Width [%s]: " visual-fill-column-width) nil nil
                                        (format "%s" visual-fill-column-width)))))
  (if (numberp width)
      (setq-local visual-fill-column-width width)
    (user-error "%s is not a number" width)))

(defun git-message ()
  (format "[%s] %s"
          (gethash 'system-name configurations "Check system-name in configurations.el")
          (format-time-string "%Y-%m-%dT%H:%M:%S%:z")))

(defun amsha/get-project-root-overlooking-submodules (&optional filename)
  "Get the project root by looking for the root directory by running
git rev-parse --show-superproject-working-tree --show-toplevel | head -1"
  (unless (featurep 'magit)
    (require 'magit))
  (let ((default-directory (magit--safe-default-directory (or filename default-directory))))
    (when-let (project-root (magit-git-string "rev-parse" "--show-superproject-working-tree" "--show-toplevel"))
      (format "%s/" project-root))))

(defun copy-buffer-file-name (buffer-file-name)
  "Copy the file name without the directory. If directory, copy the directory-name.
 If buffer/non-exisitant file just copy buffer-file-name."
  (kill-new
   (if (f-exists-p buffer-file-name)
       (f-filename buffer-file-name)
     buffer-file-name)))

(defun copy-file-full-path (file-name)
  "Copy the full path of the file-name"
  (kill-new (file-truename file-name)))

(defun copy-current-file-buffer-name ()
  "Copy the current buffers filename or FILE to the kill ring."
  (interactive)
  (copy-buffer-file-name (buffer-name (window-buffer (minibuffer-selected-window)))))

(defun copy-current-file-full-path ()
  "Copy the current buffers filename to the kill ring."
  (interactive)
  (copy-file-full-path (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(defun copy-current-directory ()
  "Copy the current direcory. If prefix arg, copy project root."
  (interactive)
  (kill-new (-->
             (file-truename
              (buffer-file-name (window-buffer
                                 (minibuffer-selected-window))))
             (if current-prefix-arg
                 (amsha/get-project-root-overlooking-submodules it)
               (file-name-directory it)))))

(defun single-linify (beg end)
  "Make a paragraph single-lined by replacing line break with space.
  BEG The begining of a region
  END The end of a region"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((insertion (replace-regexp-in-string "\n" " " (buffer-substring beg end))))
    (when (and beg end)
      (delete-region beg end)
      (insert insertion))))

(defun terminal-in-directory ()
  "Option terminal in current directory, in project root if prefix."
  (interactive)
  (shell-command
   (format "%s %s"
           (if (eq system-type 'windows-nt) "wt -d" "tilix --window-style=disable-csd-hide-toolbar")
           (-->
             (file-truename
              (buffer-file-name (window-buffer
                                 (minibuffer-selected-window))))
             (if current-prefix-arg
                 (amsha/get-project-root-overlooking-submodules it)
               (file-name-directory it))))))

(defun amsha/downlad-raname-move-file (url newname dir)
  (url-copy-file url (expand-file-name newname dir)))

(defun amsha/get-uml-link (link)
  (interactive "slink: ")
  (let ((split-link (s-split "/" link))
	(formated-link '()))
    (dolist (el split-link)
      (if (or (s-matches-p ".*\.com" el)
		(s-matches-p ".*\.org" el))
	  (push (s-concat (s-replace "." "-" el) ".uml.idm.oclc.org") formated-link)
	(push el formated-link)))
    (kill-new (s-join "/" (reverse formated-link)))))

;; font setup****************************************************
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(set-face-attribute 'default nil
                       :font "Fira Code"
                       :weight 'normal
                       :height 100)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "Fira Code"
                    :weight 'normal
                    :height 100)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    ;; :font "Cantarell"
                    :font "Iosevka Aile"
                    :height 105
                    :weight 'normal)

(setq font-name-used "Noto Emoji")
(when (member font-name-used (font-family-list))
  (set-fontset-font
   t 'unicode (font-spec :family font-name-used) nil 'prepend))

(use-package exec-path-from-shell
  :demand
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :commands (get-gitignore-from-github)
  :custom
  (magit-clone-always-transient t)
  (magit-git-executable "git")
  (transient-default-level 7)
  :config

  (defun get-gitignore-from-github ()
    "Get a gitignore file from github/gitignore repo."
    ;; Expecting the repo (https://github.com/github/gitignore) to be cloned in ~/.emacs.d/.cache/gitignore
    (interactive)
    (unless (featurep 'magit-gitignore)
      (require 'magit-gitignore))
    (let ((gitignore-location (expand-file-name ".cache/gitignore" user-emacs-directory)))
      (magit--with-safe-default-directory gitignore-location
        (magit-run-git-with-editor "pull"))
      (let* ((gitignore-files (--map (cons (f-base it) it) (f-files gitignore-location (lambda (f) (string= "gitignore" (f-ext f))))))
             (target-gitignore-content (f-read-text (cdr (assoc (completing-read "Add gitignore for: " gitignore-files) gitignore-files)))))
        (magit-with-toplevel
          (magit--gitignore target-gitignore-content ".gitignore")))))

  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)

  (defvar home-dir-magit-files '("~/" "~/.emacs.d/customFiles/"))

  (defun home-dir-magit-process-environment (env)
    "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel)."
    (let* ((default (file-name-as-directory (expand-file-name default-directory)))
           (dot-dirs (--map (expand-file-name it) home-dir-magit-files)))
      (when (member default dot-dirs)
        (let ((gitdir (expand-file-name "~/.dotfiles/")))
          (push (format "GIT_WORK_TREE=%s" (car dot-dirs)) env) ;; car of dot-dirs should be ~/
          (push (format "GIT_DIR=%s" gitdir) env))))
    env)

  (advice-add 'magit-process-environment
              :filter-return #'home-dir-magit-process-environment)

  (transient-append-suffix 'magit-commit '(1 0 -1)
    '("/m" "commit with gm"
      (lambda ()
        (interactive)
        (let ((msg (git-message)))
          (when (y-or-n-p (format "Commit with message \"%s\" " (propertize msg 'face 'magit-tag)))
            (magit-run-git-with-editor "commit" "-m" msg))))))

  (defmacro magit-sync-repo (name git-directory git-message &optional add-directories)
    "Creats an interactive function with name `sync-<NAME>'.
Calling the function will execute pull inside the GIT-DIRECORY, commit any changes
with the GIT-MESSAGE and push from the directory.

If ADD-DIRECTORIES are provided, before commit the changes in those directories
would be added.

GIT-MESSAGE can be string, a symbol that evaluates to a string or a function
that returns a string."
    (let* ((func-name (format "sync-%s" name))
           (message-prefix (concat func-name ": %s")))
      `(defun ,(intern func-name) ()
         ,(format "Sync content in %s with git." git-directory)
         (interactive)
         (magit--with-safe-default-directory ,git-directory
           (message ,(concat "sync-" name ": %s")
                    (-if-let* ((_f (progn (message ,message-prefix "pulling")
                                          (magit-git-string-ng "pull" "--autostash" "--rebase")
                                          ;; fail if there are unmerged files
                                          (not (magit-git-string-ng "diff" "--diff-filter=U"))))
                               (_c (magit-with-toplevel
                                     (message ,message-prefix "commiting")
                                     (magit-stage-1 "-u")
                                     ;; anything in the following dir's not in gitignore should be added
                                     ,@(when (and add-directories
                                                  (or (listp add-directories)
                                                      (user-error "`add-directories' is not a list")))
                                         (mapcar (lambda (d) `(magit-git-string-p "add" ,d)) add-directories))
                                     ;; (magit-git-string-p "add" "brain/research_papers")
                                     ;; (magit-git-string-p "add" "brain/roam-notes")
                                     ;; (magit-git-string-p "add" "brain/work/figures")
                                     ;; commit only if there is anything being staged
                                     (if (not (magit-git-string-ng "diff" "--cached"))
                                         (message ,message-prefix "nothing to commit")
                                       (magit-git-string-ng "commit" "-m"
                                                            ,(pcase git-message
                                                               ((pred functionp) `(funcall ',git-message))
                                                               ((or (pred stringp)
                                                                    (pred symbolp)) git-message)
                                                               (t (user-error "`git-message' is not a function, string or symbol."))))
                                       'has-diff)))
                               (_f (if (eq _c 'has-diff)
                                       (progn
                                         (message ,message-prefix "pushing")
                                         (magit-run-git-with-editor "push"))
                                     t)))
                        "success"
                      "failed")))))))

(use-package forge
  :after magit)

(use-package hl-todo
  :hook prog-mode-hook)

(use-package flycheck
  :defer t
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c e"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map))

(use-package beacon
  :defer 2
  :custom
  (beacon-push-mark nil)
  (beacon-color "#cc342b")
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.3)
  :config
  (beacon-mode))

(use-package visual-fill-column
  :defer 2
  :hook ((org-mode LaTeX-mode latex-mode markdown-mode org-roam-mode) . amsha/visual-fill))

;; Loading symlink-fix (https://www.emacswiki.org/emacs/symlink-fix.el)*************
;; Had to install this to resolve the symlink issues that cropped up with using org in both OS's
(unless (eq system-type 'windows-nt)
    (progn
      (setq symlink-overload-expand-file-name-p t)
      (require 'symlink-fix)
      (setq expand-file-name-resolve-symlinks-p t)))
  ;; (set-face-attribute 'default nil
                      ;; :family "Consolas" :height 105))

(use-package topspace
  :custom
  (topspace-empty-line-indicator (propertize "~" :forground "gray33")))

;;enable ido mode
;; (require 'ido)
;; (ido-mode t)

(use-package golden-ratio
  :defer t)

(use-package diminish
  :defer 1
  :config
  (diminish 'visual-line-mode)
  (diminish 'ivy-mode)
  (diminish 'projectile-mode "P"))

(use-package flycheck-package
  :defer t)

(use-package package-build
  :defer t)

(use-package persistent-scratch
  :init (persistent-scratch-setup-default))

(use-package powershell)


;;evil *************************************************************
(use-package evil
  :defer 1
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil)
  :config
  (setq evil-toggle-key "C-S-z"
        evil-shift-width 4
        ;;evil-want-integration t
        evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-kill-on-visual-paste nil
        ;; Moving the xref funtion to the top
        evil-goto-definition-functions '(evil-goto-definition-xref
                                         evil-goto-definition-imenu
                                         evil-goto-definition-semantic
                                         evil-goto-definition-search)
        evil-cross-lines t
        evil-undo-system 'undo-redo)

  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "\C-e" 'evil-end-of-line)
  (evil-global-set-key 'motion "\C-a" 'evil-first-non-blank)
  (evil-global-set-key 'motion (kbd "TAB") nil)
  (evil-global-set-key 'motion (kbd "RET") nil)
  (evil-global-set-key 'normal "\C-t" nil)
  (evil-global-set-key 'insert "\M-q" 'evil-force-normal-state)
  (evil-global-set-key 'visual "\M-q" 'evil-force-normal-state)
  (evil-global-set-key 'normal "\M-q" 'evil-force-normal-state)
  (evil-global-set-key 'replace "\M-q" 'evil-force-normal-state)

  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'view-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'emacs)

  (add-hook 'before-save-hook (lambda (&rest _) "Switch to normal mode before saving buffer" (evil-force-normal-state)))
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  ;; Allow org-noter to bind q
  ;; (evil-collection-define-key 'normal 'evil-collection-eldoc-doc-buffer-mode-map "q" nil)
  ;; (evil-collection-define-key 'normal 'special-mode-map "q" nil)
  ;; (define-key special-mode-map "q" nil)
  (evil-collection-define-key 'normal 'pdf-view-mode-map "\C-u" nil)
  (evil-collection-define-key 'normal 'pdf-view-mode-map "B" 'pdf-history-backward)
  )

(use-package evil-lion
  :after evil
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC"))

(use-package evil-embrace
  :after evil
  :config
  (global-evil-surround-mode 1)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-multiedit
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "M-d")   #'evil-multiedit-match-symbol-and-next
    (kbd "M-D")   #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global
    "R"           #'evil-multiedit-match-all
    (kbd "M-d")   #'evil-multiedit-match-and-next
    (kbd "M-D")   #'evil-multiedit-match-and-prev)
  (evil-define-key '(visual normal) 'global
    (kbd "C-M-d") #'evil-multiedit-restore)

  (with-eval-after-load 'evil-mutliedit
    (evil-define-key 'multiedit 'global
      (kbd "M-d")   #'evil-multiedit-match-and-next
      (kbd "M-S-d") #'evil-multiedit-match-and-prev
      (kbd "RET")   #'evil-multiedit-toggle-or-restrict-region)
    (evil-define-key '(multiedit multiedit-insert) 'global
      (kbd "C-n")   #'evil-multiedit-next
      (kbd "C-p")   #'evil-multiedit-prev)))

;; Keybindings set in evil-collections
(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode  1))

(use-package evil-org
  :ensure t
  :after (org evil)
  :hook (org-mode . (lambda ()
                      (evil-org-mode)
                      (evil-define-key 'normal 'evil-org-mode
                        (kbd "M-k") 'org-metaup
                        (kbd "M-j") 'org-metadown)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (with-eval-after-load 'org-super-agenda
    (setq org-super-agenda-header-map (copy-keymap org-agenda-mode-map))))

(use-package evil-textobj-tree-sitter
  :defer t
  :after evil
  :straight (evil-textobj-tree-sitter :type git :host github :repo "meain/evil-textobj-tree-sitter"
                                      :files (:defaults "queries" "treesit-queries"))
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  ;; Goto start of next function
  (define-key evil-normal-state-map
              (kbd "]f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer")))

  ;; Goto start of previous function
  (define-key evil-normal-state-map
              (kbd "[f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

  ;; Goto end of next function
  (define-key evil-normal-state-map
              (kbd "]F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

  ;; Goto end of previous function
  (define-key evil-normal-state-map
              (kbd "[F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

;;selectrum  *******************************************************
;; (use-package prescient
;;   :config
;;   (prescient-persist-mode +1))

;;(use-package company-prescient)
;; (use-package selectrum-prescient
;;   :config
;;   (selectrum-prescient-mode +1)
;;   (setq selectrum-prescient-enable-filtering nil))

(use-package evil-numbers
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "C-c +") #'evil-numbers/inc-at-pt)
  (evil-define-key 'normal 'global (kbd "C-c -") #'evil-numbers/dec-at-pt)
  (evil-define-key 'normal 'global (kbd "<kp-add>") #'evil-numbers/inc-at-pt)
  (evil-define-key 'normal 'global (kbd "<kp-substract>") #'evil-numbers/dec-at-pt))

(use-package vundo
  :commands (vundo)
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display nil)

  ;; Better contrasting highlight.
  (custom-set-faces
    '(vundo-node ((t (:foreground "#808080"))))
    '(vundo-stem ((t (:foreground "#808080"))))
    '(vundo-highlight ((t (:foreground "#FFFF00"))))))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  :config
  ;; (defun amsha/without-if-bang (pattern _index _total)
  ;;   (cond
  ;;    ((equal "!" pattern)
  ;;     '(orderless-literal . ""))
  ;;    ((string-prefix-p "!" pattern)
  ;;     `(orderless-without-literal . ,(substring pattern 1)))))

  (defun amsha/match-components-literally (orig-fun &rest args)
    "Funtion to add as advice for interactive functions that will always use lietral completion."
    (interactive (lambda (spec) (advice-eval-interactive-spec spec)))
    (let ((orderless-matching-styles '(orderless-literal)))
      (apply orig-fun args)))

  (advice-add #'org-set-property :around #'amsha/match-components-literally))

(use-package corfu
  :demand t
  ;; :bind (:map corfu-map
         ;; ("C-q" . corfu-quick-complete))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)      ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (global-corfu-minibuffer t)

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  (corfu-echo-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (corfu-indexed-mode))

(use-package kind-icon
  :defer t
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package vertico
  :straight (vertico :includes (vertico-directory vertico-quick vertico-indexed)
                     :files (:defaults "extensions/*.el"))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t
        ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
        vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package recentf
  :init
  (recentf-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-quick
  :after (vertico doom-themes)
  :ensure nil
  :bind (:map vertico-map
              ("\C-q" . vertico-quick-insert)
              ("\M-q" . vertico-quick-exit))
  :custom-face
  (vertico-quick1 ((t :background ,(doom-color 'base1)
                      :height 0.8
                      :box (:line-width 3
                            :color ,(doom-color 'base1)))))
  (vertico-quick2 ((t :background ,(doom-color 'base1)
                      :height 0.8
                      :box (:line-width 3
                                        :color ,(doom-color 'base1))))))

;; (use-package vertico-indexed
;;   :after vertico
;;   :ensure nil
;;   :config
;;   (vertico-indexed-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia)
  :straight (all-the-icons-completion :type git :host github :repo "iyefrat/icon-affixation")
  :init (all-the-icons-completion-mode))

;;embark & consult**************************************************
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   :map minibuffer-mode-map
   ("M-d" . delete-minibuffer-contents)
   ("C-," . embark-select-with-clear-buffer)
   ("C-<return>" . embark-act-all-with-default-action))
  :custom
  (embark-confirm-act-all nil)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  
  (define-key embark-file-map "o" nil)
  (define-key embark-file-map "ocn" #'copy-buffer-file-name)
  (define-key embark-file-map "ocf" #'copy-file-full-path)

  (defun embark-select-with-clear-buffer ()
    "Run `embark-select' and `delete-minibuffer-contents'."
    (interactive)
    (when (minibufferp)
      (embark-select)
      (delete-minibuffer-contents)))

  (defun embark-non-propmter-with-default-action (_ _)
    "For `embark-prompter' that just returns the default action."
    embark--command)

  (defun embark-act-all-with-default-action (&optional arg)
    "`embark-act-all' but with default action without prompting."
    (interactive)
    (let ((embark-prompter #'embark-non-propmter-with-default-action))
      (embark-act-all arg)))

  (defun embark-minibuffer-exit (other &rest rest)
    "If embark-selection has been run, run `embark-act-all-with-default-action'."
    (if embark--selection
        (apply #'embark-act-all-with-default-action rest)
      (apply other rest)))

  (advice-add 'vertico-exit :around #'embark-minibuffer-exit)

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ;; ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Selectrum, Vertico etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark :preview-key "M-."
   consult--source-buffer :hidden t :default nil)

  (defvar consult--source-perspective
    (list :name     "Perspective"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :action   #'consult--buffer-action
          :default  t
          :items
          (lambda () (let ((current-persp-buffers (persp-get-buffer-names)))
                       (consult--buffer-query :sort 'visibility
                                              :as #'buffer-name
                                              :predicate
                                              (lambda (it)
                                                (member (buffer-name it) current-persp-buffers)))))))

  (defvar consult--source-persp-harpoon
    (list :name     "Persp Harpoon"
          :narrow   ?h
          :category 'buffer
          :state    #'consult--buffer-state
          :action   #'consult--buffer-action
          :items    #'persp-harpoon--get-buffers-for-completion))

  (push 'consult--source-persp-harpoon consult-buffer-sources)
  (push 'consult--source-perspective consult-buffer-sources)

  (defvar consult--source-dogears
    (list :name     "Dogears"
          :narrow   ?d
          :category 'dogears
          :items    (lambda ()
                      (mapcar
                       (lambda (place)
                         (propertize (dogears--format-record place)
                                     'consult--candidate place))
                       dogears-list))
          :action   (lambda (cand)
                      (dogears-go (get-text-property 0 'consult--candidate cand)))))
  
  (defun consult-dogears ()
    (interactive)
    (consult--multi '(consult--source-dogears)))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package embark-org
;;   :after embark)

;; consult-todo***********************************************************************
(use-package consult-todo
  :straight (:host github :repo "liuyinz/consult-todo")
  :commands (consult-todo consult-todo-all)
  :after (hl-todo consult))

;; gptel ***********************************************************
(use-package gptel
  :bind
  (("C-c o q m" . gptel-menu)
   ("C-c o q b" . gptel)
   ("C-c o q Q" . gptel-send))
  :custom
  (gptel-api-key (gethash 'openai-apk configurations))
  (gptel-use-curl t)
  (gptel-backend gptel--openai)
  (gptel-model 'o4-mini)
  :config
  (require 'gptel-extensions)
  ;; (put 'o3-mini :request-params '(:reasoning_effort "high" :stream :json-false))


  (add-to-list 'yank-excluded-properties 'gptel)

  (setf gptel-org-branching-context t
        gptel-expert-commands t
        (gptel-backend-models gptel--openai) (append (gptel-backend-models gptel--openai)
                                                     (--map (prog1 it (put it :capabilities '(reasoning)))
                                                            '(gpt-4o-search-preview gpt-4o-mini-search-preview)))
        (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n"
        (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"))

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


;; (use-package consult-gh
;;   :straight (consult-gh :type git :host github :repo "armindarvish/consult-gh")
;;   :after consult
;;   :config
;;   (require 'consult-gh-transient)
;;   (require 'consult-gh-embark)
;;   (require 'consult-gh-forge)
;;   (consult-gh-embark-mode +1)
;;   (consult-gh-forge-mode +1))

(use-package browser-hist
  :config
  (setq
   browser-hist-db-paths (list (cons 'chrome (file-truename "~/AppData/Roaming/Opera Software/Opera Stable/Default/History")))
   browser-hist-default-browser 'chrome)
  :commands (browser-hist-search))

(use-package consult-omni
  :straight (consult-omni :type git :host github :repo "armindarvish/consult-omni" :branch "main" :files (:defaults "sources/*.el"))
  :after consult
  :commands (consult-omni consult-omni-web consult-omni-local consult-omni-scholar)
  :bind
  (("C-c o m n" . consult-omni)
   ("C-c o m s" . consult-omni-scholar)
   ("C-c o m l" . consult-omni-local)
   ("C-c o m w" . consult-omni-web))
  :custom
  ;;; General settings that apply to all sources
  (consult-omni-show-preview t) ;;; show previews
  (consult-omni-preview-key "C-o") ;;; set the preview key to C-o
  (consult-omni-highlight-matches-in-minibuffer t) ;;; highlight matches in minibuffer
  (consult-omni-highlight-matches-in-file t) ;;; highlight matches in files
  (consult-omni-default-count 5) ;;; set default count
  (consult-omni-default-page 0) ;;; set the default page (default is 0 for the first page)

  ;; optionally change the consult-omni debounce, throttle and delay.
  ;; Adjust these (e.g. increase to avoid hiting a source (e.g. an API) too frequently)
  (consult-omni-dynamic-input-debounce 0.8)
  (consult-omni-dynamic-input-throttle 1.6)
  (consult-omni-dynamic-refresh-delay 0.8)

  ;; Optionally set backend for http request (either 'url, 'request, or 'plz)
  (consult-omni-http-retrieve-backend 'url)

  :config
  ;;; Either load all source modules or a selected list
  ;; Select a list of modules you want to aload, otherwise all sources all laoded
  (setq consult-omni-sources-modules-to-load
        (list 'consult-omni-apps
              'consult-omni-bing
              ;; 'consult-omni-brave-autosuggest
              ;; 'consult-omni-brave
              'consult-omni-browser-history
              'consult-omni-buffer
              'consult-omni-calc
              'consult-omni-chatgpt
              ;; 'consult-omni-consult-notes
              'consult-omni-dict
              'consult-omni-doi
              'consult-omni-duckduckgo
              ;; 'consult-omni-elfeed
              ;; 'consult-omni-fd
              'consult-omni-find
              ;; 'consult-omni-gh
              'consult-omni-git-grep
              'consult-omni-google
              'consult-omni-google-autosuggest
              'consult-omni-gptel
              'consult-omni-grep
              'consult-omni-invidious
              'consult-omni-line-multi
              'consult-omni-locate
              'consult-omni-man
              'consult-omni-mdfind
              ;; 'consult-omni-mu4e
              ;; 'consult-omni-notes
              ;; 'consult-omni-notmuch
              ;; 'consult-omni-numi
              'consult-omni-org-agenda
              ;; 'consult-omni-pubmed
              'consult-omni-projects
              ;; 'consult-omni-ripgrep
              ;; 'consult-omni-ripgrep-all
              'consult-omni-scopus
              ;; 'consult-omni-stackoverflow
              'consult-omni-wikipedia
              'consult-omni-youtube
              ))
  (consult-omni-sources-load-modules)

  ;; set multiple sources for consult-omni-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-omni-sources-alist'.
  (setq consult-omni-multi-sources '(
                                     "calc"
                                     "File"
                                     "Buffer"
                                     "Bookmark"
                                     "Apps"
                                     "gptel"
                                     ;; "Brave"
                                     "Dictionary"
                                     ;; "Google"
                                     "Wikipedia"
                                     "OpenAlex"
                                     ;; "elfeed"
                                     ;; "mu4e"
                                     ;; "buffers text search"
                                     ;; "Notes Search"
                                     "Org Agenda"
                                     ;; "GitHub"
                                     ;; "YouTube"
                                     "Invidious"
                                     "org-roam nodes"
                                     ))

  ;;; Per source customization

  ;; Set API KEYs. It is recommended to use a function that returns the string for better security.
  ;; (setq consult-omni-google-customsearch-key "YOUR-GOOGLE-API-KEY-OR-FUNCTION")
  ;; (setq consult-omni-google-customsearch-cx "YOUR-GOOGLE-CX-NUMBER-OR-FUNCTION")
  ;; (setq consult-omni-brave-api-key "YOUR-BRAVE-API-KEY-OR-FUNCTION")
  ;; (setq consult-omni-stackexchange-api-key "YOUR-STACKEXCHANGE-API-KEY-OR-FUNCTION")
  ;; (setq consult-omni-pubmed-api-key "YOUR-PUBMED-API-KEY-OR-FUNCTION")
  (setq consult-omni-openai-api-key (gethash 'openai-apk configurations))
  ;; add more keys as needed here.

  ;; gptel settings
  (setq consult-omni-gptel-cand-title #'consult-omni--gptel-make-title-short-answer)

  ;; default terminal
  (setq consult-omni-embark-default-term #'vterm)

  ;; default video player
  (setq consult-omni-embark-video-default-player "vlc")

  ;; pretty prompt for launcher
  (setq consult-omni-open-with-prompt "  ")

  ;;; Pick you favorite autosuggest command.
  (setq consult-omni-default-autosuggest-command #'consult-omni-google-autosuggest) ;;or any other autosuggest source you define

  ;;; Set your shorthand favorite interactive command
  (setq consult-omni-default-interactive-command #'consult-omni-multi)

  ;;; Optionally Set back-end for notes search to ripgrep-all (requires ripgrep-all)
  ;; (setq consult-omni-notes-backend-command "rga")
  )

(use-package consult-omni-extensions
  :after consult-omni
  :straight nil)

;;ivy-mode *********************************************************
(use-package amx
  :init (amx-mode 1))

;; (use-package ivy
;;   :diminish (ivy-mode . "")             ; does not display ivy in the modeline
;;   ;; :init
;;   ;; (ivy-mode 1)                          ; enable ivy globally at startup
;;   :bind (;; ("C-c g" . counsel-git)
;; 	 ;; ("C-c j" . counsel-git-grep) 
;; 	 ;; ("C-c k" . counsel-ag)       
;; 	 ;; ("C-x l" . counsel-locate)   
;; 	 ;; ("C-S-o" . counsel-rhythmbox)
;; 	 ;;("C-x C-f" . counsel-find-file)
;; 	 :map ivy-minibuffer-map        ; bind in the ivy buffer
;; 	 ("RET" . ivy-alt-done))
;; 	 ;;      ("s-<"   . ivy-avy)
;; 	 ;;      ("s->"   . ivy-dispatching-done)
;; 	 ;;      ("s-+"   . ivy-call)
;; 	 ;;      ("s-!"   . ivy-immediate-done)
;; 	 ;;      ("s-["   . ivy-previous-history-element)
;; 	 ;;      ("s-]"   . ivy-next-history-element))
;;   :config
;;   (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and
;;   (setq ivy-height 15)                   ; set height of the ivy window
;;   (setq ivy-count-format "(%d) ")     ; count format, from the ivy help page
;;   (setq ivy-display-style 'fancy)
;;   (setq ivy-format-function 'ivy-format-function-line) ; Make highlight extend all the way to the right
;;   ;; TODO testing out the fuzzy search
;;   (setq ivy-re-builders-alist
;;       '(;; (read-file-name-internal . ivy--regex-fuzzy)
;; 	;; (internal-complete-buffer . ivy--regex-fuzzy)
;; 	;; (execute-extended-command . ivy--regex-fuzzy)
;; 	;; (amx . ivy--regex-fuzzy)
;; 	(t . ivy--regex-fuzzy))))

;; (use-package all-the-icons-ivy
;;   :config
;;   (all-the-icons-ivy-setup))

;; This block for some magical reason seems to be intefering with magit so commenting out.
;; (use-package consult-dir
;;   :custom
;;   (consult-dir-project-list-function 'consult-dir-projectile-dirs)
;;   :bind (("c-x c-d" . consult-dir)
;;          :map minibuffer-local-completion-map
;;          ("c-x c-d" . consult-dir)
;;          ("c-x c-j" . consult-dir-jump-file)))

;; (use-package all-the-icons-ivy-rich
;;   :init (all-the-icons-ivy-rich-mode 1))

;; (use-package ivy-rich
;;   :init (ivy-rich-mode 1))

;; (use-package ivy-hydra
;;   :after (ivy hydra))

;; helm setup ************************************************************************
;; (use-package helm
;;   ;; :init (helm-mode 1)
;;   ;; :bind (("M-y" . helm-show-kill-ring))
;;   )

;;which key **************************************************************************
(use-package which-key :config (which-key-mode))

;;hydra ******************************************************************************
(use-package hydra)
(use-package use-package-hydra)

;;expand-region **********************************************************************
(use-package expand-region
  :bind (("C-=" . hydra-expand-region/body))
  :hydra (hydra-expand-region ()
  "
  [_r_] Expand region    [_w_] Word                [_o_] Inside-quotes    [_c_] Comment
  [_R_] Contract region  [_s_] Symbol              [_O_] Outside-quotes   [_u_] Url    
                       [_S_] Symbol-with-prefix  [_p_] Inside-pairs     [_e_] Email  
                       [_n_] Next-accessor       [_P_] Outside-pairs    [_f_] Defun  
  [_q_] quit             [_m_] Method-call
"
  ("r" er/expand-region)
  ("R" er/contract-region)
  ("w" er/mark-word)
  ("s" er/mark-symbol)
  ("S" er/mark-symbol-with-prefix)
  ("n" er/mark-next-accessor)
  ("m" er/mark-method-call)
  ("o" er/mark-inside-quotes)
  ("O" er/mark-outside-quotes)
  ("p" er/mark-inside-pairs)
  ("P" er/mark-outside-pairs)
  ("c" er/mark-comment)
  ("u" er/mark-url)
  ("e" er/mark-email)
  ("f" er/mark-defun)
  ("q" nil)))

;;embrace ****************************************************************************
(use-package embrace
  :bind ("C-," . embrace-commander)
  :hook ((org-mode . embrace-org-mode-hook)
         (LaTeX-mode . embrace-LaTeX-mode-hook)))

;;mulitple cursor ********************************************************************
(use-package multiple-cursors
  :bind (("C-c m" . hydra-multiple-cursors/body))
  :hydra (hydra-multiple-cursors (:hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("|" mc/vertical-align)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil)))


;;cycle at point *********************************************************************
;; (use-package cycle-at-point
;;   :commands cycle-at-point
;;   :bind ("M-P" . cycle-at-point))

;;grugru (alternative to cycle-at-point)**********************************************
(use-package grugru
  :defer t
  :after hl-todo
  :requires dash
  :commands (grugru grugru-select grugru-edit)
  :bind ("M-P" . grugru-select)
  :config
  (grugru-default-setup)
  (grugru-highlight-mode)
  (grugru-edit-load)

  (grugru-define-multiple
    (csharp-mode . ((symbol "private" "public" "internal" "protected")))
    (word . (--map (propertize (car it) 'face (cdr it)) hl-todo-keyword-faces))))

;;rainbow delimeters******************************************************************
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimeters-mode)
  (add-hook 'csharp-mode-hook #'rainbow-delimeters-mode)
  ;; (add-hook 'csharp-tree-sitter-mode-hook #'rainbow-delimeters-mode)
  (add-hook 'java-mode-hook #'rainbow-delimeters-mode)
  (add-hook 'python-mode-hook #'rainbow-delimeters-mode))

;;volatile-highlights*****************************************************************
(use-package volatile-highlights
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  (volatile-highlights-mode t))

;;highlight-indent-mode***************************************************************
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-auto-character-face-perc 20))

;;elgrep******************************************************************************
(use-package elgrep
  :defer t
  :commands (elgrep elgrep-r)
  :config
  (defun elgrep-r ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively 'elgrep))))

;;deadgrep****************************************************************************
(use-package deadgrep
  :commands deadgrep)

;;helpful ****************************************************************************
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; ;;lsp-bridge *************************************************************************
;; (use-package lsp-bridge
;;   :bind-keymap ("C-x l" . lsp-bridge-keymap)
;;   :straight (lsp-bridge :host github :repo "manateelazycat/lsp-bridge"
;;                         ;; :files (:defaults "acm/*.el" "acm/**" "*.el" "langserver" "multiserver" "core"))
;;                         :files ("acm" "core" "langserver" "multiserver" "test" "*.py" "*.el"))
;;   ;; :hook (csharp-mode . lsp-bridge-mode)
;;   :custom
;;   (lsp-bridge-enable-log nil) ;; enable when debugging
;;   (lsp-bridge-csharp-lsp-server "omnisharp-mono")
;;   (lsp-bridge-get-project-path-by-filepath #'amsha/get-project-root-overlooking-submodules)
;;   (lsp-bridge-get-workspace-folder #'amsha/get-project-root-overlooking-submodules)
;;   (lsp-bridge-user-langserver-dir (expand-file-name "langserver" user-emacs-directory))
;;   :config
;;   (global-lsp-bridge-mode)
;;   (setq lsp-bridge-keymap (let ((m (make-sparse-keymap)))
;;                             (define-key m (kbd "w r") #'lsp-bridge-restart-process)
;;                             (define-key m (kbd "r r") #'lsp-bridge-rename)
;;                             (define-key m (kbd "g d") #'lsp-bridge-find-def)
;;                             (define-key m (kbd "g r") #'lsp-bridge-find-references)
;;                             (define-key m (kbd "a a") #'lsp-bridge-code-action)
;;                             m))

;;   (defun amsha/start-lsp-server-in-project-root (old-func &rest r)
;;     (let ((default-directory (amsha/get-project-root-overlooking-submodules)))
;;       (apply old-func r)))

;;   (advice-add 'lsp-bridge-start-process :around #'amsha/start-lsp-server-in-project-root))

;;lsp-mode ***************************************************************************
(use-package lsp-ui
  :after (lsp-mode)
  :init
  (add-hook 'python-mode-hook #'lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover t
	lsp-ui-sideline-delay 1
        lsp-ui-sideline-diagnostic-max-lines 4
        lsp-ui-sideline-diagnostic-max-line-length 150))


;; (use-package company-box
  ;; :hook (company-mode . company-box-mode))


;; about using lsp-csharp for unity, just make sure you have installed latest omnisharp-roslyn and have mono >= 6 installed on your machine
;; the omnisharp-roslyn that lsp-mode install does not work for unity projects because it needs a recent mono version installed and the mono built-in on omnisharp-roslyn doesn't have msbuild, some libs that unity require

;; With the tsserver for web-mode, may have to manually install the tsserver using npm (https://github.com/typescript-language-server/typescript-language-server/issues/336)

;; (setq lsp-use-plists t)

(use-package lsp-mode
  :hook ((python-mode . lsp)
         (csharp-mode . lsp)
         ;; (csharp-tree-sitter-mode . lsp)
	 (java-mode . lsp)
         (js2-mode . lsp)
         (web-mode . lsp)
         (tex-mode . lsp)
         (latex-mode . lsp)
         (rust-mode . lsp)
         (go-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . amsha/lsp-mode-setup-completion))
  
  :init
  (setq lsp-keymap-prefix "C-x l")
  ;; (add-hook 'prog-mode-hook #'lsp)
  ;; (setq lsp-auto-guess-root t)
  ;; (setq lsp-log-io t)
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-lsp-clients
  (defun amsha/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  ;; (setq lsp-client-packages (seq-remove (lambda (pkg) (equal pkg 'lsp-tex)) lsp-client-packages))
  :config

  ;; Removing unnecessary clients
  (ht-remove lsp-clients 'texlab)
  (ht-remove lsp-clients 'texlab-tramp)
  (ht-remove lsp-clients 'digestif)
  (ht-remove lsp-clients 'digestif-tramp)

  ;; (lsp-register-client
  
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
  ;;                   :major-modes '(python-mode)
  ;;                   :remote? t
  ;;                   :server-id 'pyls))

  (defhydra flycheck-lsp-action (lsp-command-map "f")
    ("n" flycheck-next-error)
    ("p" flycheck-previous-error)
    ("s" save-buffer)
    ("a" lsp-execute-code-action))

  (setq 
   ;; lsp-pyls-configuration-sources ["flake8"]
   ;; lsp-pyls-plugins-jedi-completion-enabled nil
   ;; lsp-pyls-plugins-pydocstyle-enabled t
   ;; lsp-pyls-plugins-pyflakes-enabled nil
   ;; lsp-pyls-plugins-pycodestyle-max-line-length 110
   ;; lsp-pyls-plugins-rope-completion-enabled nil
   ;; lsp-pyls-plugins-pycodestyle-enabled nil
   ;; lsp-pyls-plugins-yapf-enabled nil
   ;;lsp--delay-timer 1
   gc-cons-threshold 200000000
   read-process-output-max (* 1024 1024 4) ;; 4mb
   lsp-idle-delay 0.500
   lsp-enable-file-watchers nil
   ;; lsp-csharp-server-path (if (eq system-type 'windows-nt)
   ;;      		      (file-truename "~/packages_external/omnisharp-win-x64/OmniSharp.exe")
   ;;      		    nil)
   lsp-pylsp-plugins-flake8-max-line-length 110
   lsp-pylsp-plugins-flake8-enabled nil
   lsp-pylsp-plugins-autopep8-enabled nil
   lsp-pylsp-plugins-yapf-enabled t
   lsp-pylsp-plugins-pylint-enabled nil
   lsp-completion-provider :none ;; we use Corfu!
   )
  (lsp-register-custom-settings '(("omnisharp.useGlobalMono" "always"))))


;; (use-package lsp-jedi
;;   :straight t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     ;; (add-to-list 'lsp-enabled-clients 'jedi)
;;     ))

(use-package python
  :straight (:type built-in)
  :ensure nil
  :config
  (setq
   python-shell-interpreter "poetry"
   python-shell-completion-setup-code ""
   python-shell-interpreter-interactive-arg ""
   python-shell-interpreter-args "run python -i"))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-disabled-clients 'pylsp))
  (setq lsp-pyright-use-library-code-for-types t ;; set this to nil if getting too many false positive type errors
        lsp-pyright-stub-path (file-truename "~/.emacs.d/python-type-stubs"))) ;; example

(use-package lsp-java
  :defer t)

;; (use-package lsp-python-ms
;;   :straight t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

(use-package lsp-treemacs
  :after (lsp-mode)
  :custom
  (treemacs-missing-project-action 'remove)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package dap-mode
  :after lsp-mode
  :bind (:map lsp-command-map
         ("d" . dap-hydra))
  :hook ((dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
         (python-mode . dap-ui-mode)
         (python-mode . dap-mode))
  :config
  (dap-auto-configure-mode)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-ui-controls-mode -1))

;; (use-package lsp-grammarly
;;   :after (lsp)
;;   :ensure t
;;   ;; :hook (text-mode . (lambda ()
;;   ;;                      (require 'lsp-grammarly)
;;   ;;                      (lsp-deferred))))  ; or lsp-deferred
;;   :custom
;;   (lsp-grammarly-domain "academic")

;;   )

;; (use-package lsp-ltex
;;   :after lsp-mode
;;   :ensure t
;;   ;; :hook (text-mode . (lambda ()
;;   ;;                      (require 'lsp-ltex)
;;   ;;                      (lsp)))  ; or lsp-deferred
;;   :init
;;   (setq lsp-ltex-version "16.0.0"
;;         lsp-ltex-enabled t))  ; make sure you have set this, see below

(use-package lsp-ltex-plus
  :ensure t
  :straight (lsp-ltex-plus :type git :host github :repo "emacs-languagetool/lsp-ltex-plus")
  ;; :hook (text-mode . (lambda ()
  ;;                      (require 'lsp-ltex-plus)
  ;;                      (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-plus-version "18.4.0"
        lsp-ltex-plus-enabled t
        lsp-ltex-plus-java-maximum-heap-size 2000
        lsp-ltex-plus-sentence-cache-size 2000))

;; (use-package lsp-origami
;;   :hook ((lsp-after-open-hook . lsp-origami-try-enable)))

(use-package dap-python
  :after lsp
  :straight nil
  :config
  (setq dap-python-debugger 'debugpy)

  (dap-register-debug-template
   "Python :: Run pytest - from anywhere"
   (list :type "python"
         ;; in place of running the whole test, a specific test can be set here
         ;; eg: :args "test/test_process_config.py::test_process_toml"
         :args nil
         :cwd "${workspaceFolder}"
         ;; if this is nil it will append the buffer file name, which stops pytest from running all tests
         :program ""
         :module "pytest"
         :request "launch"
         ;; https://code.visualstudio.com/docs/python/testing#_pytest-configuration-settings for more info
         :environment-variables '(("PYTEST_ADDOPTS" . "--no-cov"))
         :name "Python :: Run pytest - from anywhere")))

(use-package dap-unity
  :straight nil)

(use-package dap-java :straight nil
  :after (lsp-java))

(use-package rustic
  :defer t)

(use-package go-mode
  :defer t)

;;avy *******************************************************************************
;; see https://karthinks.com/software/avy-can-do-anything/#kill-a-candidate-word-sexp-or-line for more cool stuff
(use-package avy
  :bind (("C-S-s" . avy-isearch)
	 ("C-'" . avy-goto-char-timer)
	 ("M-g l" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1))
  :config
  (avy-setup-default)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; pdf
(use-package pdf-tools
  :defer t
  :straight (pdf-tools :type git :host github :repo "vedang/pdf-tools"
                       :fork (:host github :repo "ahmed-shariff/pdf-tools" :branch "windows-fix-308"))
  :config
  (when (gethash 'use-pdf-tools configurations t)
    (when (eq system-type 'gnu/linux)
      (setq pdf-tools-directory "/home/amsha/.emacs.d/straight/repos/pdf-tools/"))
    (pdf-tools-install t)

    (add-to-list 'pdf-view-incompatible-modes 'display-line-numbers-mode)

    (defun amsha/pdf-view-mode-hook ()
      (pdf-misc-size-indication-minor-mode)
      (set (make-local-variable 'evil-normal-state-cursor) (list nil))
      (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
      (evil-emacs-state))
    (add-hook 'pdf-view-mode-hook #'amsha/pdf-view-mode-hook)

    (defun pdf-toggle-edit-on-annotation-created ()
      "Toggle if annotations active function should be called.

See `pdf-annot-activate-created-annotations' for more details."
      (interactive)
      (setq-local pdf-annot-activate-created-annotations (not pdf-annot-activate-created-annotations))
      (message "Toggled pdf-annot-activate-created-annotations to: %s" pdf-annot-activate-created-annotations))))

;;delete-selection-mode
(delete-selection-mode t)
(require 'vlf-setup)

;; Flycheck: On the fly syntax checking
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode)
  :config
  ;; stronger error display
  (defface flycheck-error
    '((t (:foreground "red" :underline (:color "Red1" :style wave) :weight bold)))
    "Flycheck face for errors"
    :group "flycheck")
  (setq flycheck-check-syntax-automatically '(mode-enabled new-line save)))

(use-package all-the-icons)
;;flycheck
;;(require 'flyspell)

;;ispell
;; for hunspell on windows: http://www.nextpoint.se/?p=656
(use-package ispell
  :config
  (setq ispell-program-name "hunspell"))

(use-package flyspell)

;; On windows
;; 1. install https://packages.msys2.org/packages/mingw-w64-x86_64-enchant
;; 2. from the jinx dir in builds:
;;    gcc -I. -O2 -Wall -Wextra -fPIC -shared -o jinx-mod.dll jinx-mod.c -I/mingw64/include/enchant-2 -L/mingw64/lib -L/mingw64/lib/enchant-2 -lenchant-2
;;(use-package jinx
;;  :hook (emacs-startup . global-jinx-mode)
;;  :bind (("M-$" . jinx-correct)
;;         ("C-M-$" . jinx-languages)))

;; (require 'micgoline)
;; (setq powerline-default-separator 'arrow-fade)
;; (use-package spaceline-all-the-icons 
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))
;;(rich-minority-mode 1)

;; (use-package eyeliner :straight nil
;;   :straight (eyeliner :type git
;;                       :host github
;;                       :repo "dustinlacewell/eyeliner")
;;   :config
;;   (require 'eyeliner)
;;   (setq eyeliner/left-hand-segments
;;         '(("%l:%c ")
;; 	  (eyeliner/buffer-name :skip-alternate t)
;;           (eyeliner/mode-icon)
;; 	  (eyeliner/buffer-modified)
;;           (eyeliner/branch-icon :skip-alternate t :tight-right t)
;;           (eyeliner/branch-name)
;;           (eyeliner/project-name :skip-alternate t))
;; 	eyeliner/right-hand-segments
;; 	'((" %q "))
;; 	eyeliner/cool-color "medium blue")
;;   (eyeliner/install))

(use-package doom-modeline
  ;;:hook (after-init . doom-modeline-mode)
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon (display-graphic-p)
	doom-modeline-minor-modes (featurep 'minions)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-peacock t)
  (custom-set-faces
   `(org-block-begin-line ((t (:background "#112424"))))
   `(org-block-end-line ((t (:background "#112424")))))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package dired-sort
;;   :straight nil
;;   :after dired)

;; (use-package dired+
;;   :after dired
;;   :config
;;   (diredp-toggle-find-file-reuse-dir 1)
;;   ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;;   (add-hook 'dired-mode-hook (lambda () (dired-omit-mode))))

(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ([return] . 'dired-single-buffer)
              ([mouse-1] . 'dired-single-buffer-mouse)
              ("." . 'hydra-dired/body)
              ("^" . 'amsha/dired-go-up))
  :config
  (defun amsha/dired-go-up ()
    (interactive)
    (dired-single-buffer ".."))

  :hydra
  (hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue)))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"           "Home")
     ("p" "~/Project"    "Home")
     ("d" "~/Downloads/" "Downloads")))
  :config
  (require 'vc)
  (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (evil-make-overriding-map dirvish-mode-map 'normal)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map          ; Dirvish inherits `dired-mode-map'
   ("?"   . dirvish-dispatch)     ; contains most of sub-menus in dirvish extensions
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; (use-package ranger
;;   :hook
;;   ((dired-mode ranger-mode) . (lambda () (visual-line-mode -1)))
;;   :custom
;;   (dired-mouse-drag-files t)
;;   (ranger-override-dired 'ranger)
;;   (ranger-preview-delay 0.5)
;;   (ranger-width-preview 0.4)
;;   (ranger-return-to-ranger t)
;;   :config
;;   (ranger-override-dired-mode t))

;;ibuffer****************************************************************
(use-package ibuffer
  :bind ("C-x C-b" . 'ibuffer))

;; ;;org mode*******************************************************
;; ;;rest in ~/.emacs.d/CustomLoadFiles/orgZ.el
(require 'orgZ)

;; ;;yasnippet setup************************************************
(use-package yasnippet
  :defer t
  :init (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
        (append yas-snippet-dirs
                `("~/.emacs.d/snippets"))                 ;; personal snippets
        yas-indent-line 'fixed))

;;slime and cl setup*********************************************
(use-package slim-mode
  :config
  ;;(load (expand-file-name "/home/amsha/quicklisp/slime-helper.el"))
  ;; (setq package-enable-at-startup nil)
  ;;(setq inferior-lisp-program "F:/Binaries/ccl/wx86cl64.exe")
  (setq inferior-lisp-program "sbcl")
  (setq slime-auto-connect 'ask)
  (setq slime-net-coding-system 'utf-8-unix))

(use-package slime
  :config
  (slime-setup
   '(slime-fancy slime-asdf slime-references slime-indentation slime-xref-browser))
  ;; (unless package-archive-contents 
  ;;   (package-refresh-contents))
  (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t))

;;python setup*************************************************
 ;(eval-after-load "company"
 ;  '(add-to-list 'company-backends 'company-anaconda)); :with company-capf)))
 ;(add-hook 'python-mode-hook 'anaconda-mode)
 ;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;(add-hook 'python-mode-hook '(company-anaconda 'interactive))
;;(add-to-list 'company-backends 'company-jedi)
;; (use-package elpy
;;   :straight t
;;   :init
;;   (elpy-enable))
					;(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
;; (use-package with-venv
;;   :after (lsp lsp-pyright)
;;   :config
;;   (with-venv-advice-add 'lsp-pyright-locate-venv)
;;   (with-venv-advice-add 'lsp-pylsp-get-pyenv-environment)
;;   (with-venv-advice-add 'dap-python--pyenv-executable-find))

(use-package poetry
  :defer t
  :straight (poetry :type git :host github :repo "cybniv/poetry.el"
                    :fork (:host github :repo "ahmed-shariff/poetry.el"))
  :init
  (poetry-tracking-mode)
  :config
  ;; (defun pyright-find-venv-with-poetry ()
  ;;   "lsp-pyright use poetry to get the venv."
  ;;   poetry-project-venv)

  ;; ;;(advice-add #'lsp-pyright-locate-venv :override 'pyright-find-venv-with-poetry))
  ;; (advice-add #'lsp-pylsp-get-pyenv-environment :override 'pyright-find-venv-with-poetry))
  )

;; (use-package linum-relative
;;   :demand
;;   :hook (prog-mode text-mode)
;;   :custom
;;   (linum-relative-backend 'display-line-numbers-mode)
;;   :config
;;   ;; (linum-relative-global-mode))
;;   )

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'visual))

(use-package ess
  :defer t)

;; (highlight-indentation-mode t)
;; (highlight-indentation-current-column t)
;; (use-package diminish)
;; (diminish 'highlight-indentation-mode)
;; (diminish 'highlight-indentation-current-column-mode)
;; (diminish 'elpy)
;; (diminish 'hs-minor-mode)
;; (diminish 'Projectile "Projectile")

;;prolog*******************************************************
;; (setq auto-mode-alist
;;   (cons (cons "\\.pl" 'prolog-mode)
;;      auto-mode-alist))

;; c/c++ setup*************************************************
(use-package semantic
  :defer t
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)

  (semantic-mode 1))

(use-package cc-mode
  :defer t
  :config

  ;; (add-to-list 'company-backends 'company-c-headers)
                                        ; (semantic-add-system-include "/usr/lib/gcc/x86_64-pc-linux-gnu/6.4.1/" 'c++-mode)
					;(add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1")
  (defun my-c-mode-common-hook ()
    ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
    (c-set-offset 'substatement-open 0)
    ;; other customizations can go here

    (setq c++-tab-always-indent t)
    (setq c-basic-offset 4)                  ;; Default is 2
    (setq c-indent-level 4)                  ;; Default is 2

    (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
                                        ; (setq tab-width 4)
    (setq indent-tabs-mode nil)  ; use spaces only if nil
    )

  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

;; (use-package ccls
;;   :straight t
;;   :config
;;   (setq ccls-executable "ccls")
;;   (setq lsp-prefer-flymake nil)
;;   (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;   :hook ((c-mode c++-mode objc-mode) .
;; 	 (lambda ()
;; 	   (require 'ccls)
;; 	   (lsp)
;; 	   (linum-mode))))


;;visual editing**********************************************************************************************************
;;set transparency********************************************
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
;; (set-frame-parameter (selected-frame) 'alpha '(98 . 92))
;; (add-to-list 'default-frame-alist '(alpha . (98 . 92)))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(mode-line ((t (:background "#004276" :foreground "#8cccff" :box nil :height 0.9))))
;;  '(mode-line-inactive ((t (:background "#666666" :foreground "#f9f9f9" :box nil :height 0.9))))
;;  '(sml/global ((t (:foreground "gray50" :inverse-video nil :height 0.9 :width normal)))))


;;arduino-mode***********************************************************
(use-package arduino-mode)

(use-package arduino-cli-mode
  :ensure t
  :hook arduino-mode
  ;; :mode "\\.ino\\'"
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))

;;projectile mode********************************************************
(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1)
  (setq projectile-git-command "git ls-files --recurse-submodules --exclude-standard -zc"
        projectile-cleanup-known-projects nil
        projectile-project-root-functions '(projectile-root-local
                                            projectile-root-marked
                                            amsha/get-project-root-overlooking-submodules
                                            projectile-root-bottom-up
                                            projectile-root-top-down
                                            projectile-root-top-down-recurring)
        projectile-current-project-on-switch 'move-to-end)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (defun projectile-set-buffer-directory (&optional arg)
    "Set the default directory to the root of a project if the current buffer is not a file buffer.
Used with atomic-chrome."
    (interactive "P")
    (if (buffer-file-name (current-buffer))
        (user-error "This is a file buffer.")
      (let ((projects (projectile-relevant-known-projects)))
        (if projects
            (projectile-completing-read
             "Switch to project: " projects
             :action (lambda (project)
                       (setq default-directory project))))))))


;;consult-projectile****************************************************************************
(use-package consult-projectile
  :config
  (advice-add 'projectile-find-file
              :override 'consult-projectile-find-file)
  (advice-add 'projectile-switch-project
              :override 'consult-projectile-switch-project)
  (advice-add 'projectile-find-dir
              :override 'consult-projectile-find-dir)
  (advice-add 'projectile-recentf
              :override 'consult-projectile-recentf)
  (advice-add 'projectile-switch-to-buffer
              :override 'consult-projectile-switch-to-buffer))

;; (use-package persp-projectile)  ;; for bridging the projectile functions with persp
;;   (define-key projectile-mode-map [remap projectile-switch-project] 'consult-projectile))

;;perspective mode******************************************************************************

(use-package perspective
  :after consult-projectile
  :custom (persp-mode-prefix-key (kbd "C-c w"))
  :bind (("C-x k" . persp-kill-buffer*))
  :hook (kill-emacs-hook . persp-state-save)
  :init (persp-mode 1)
  (setq persp-state-default-file "~/.emacs.d/.cache/perspective-state-default-file")
  ;; (setq projectile-switch-project-action (lambda ()
  ;;                                          (persp-switch (projectile-project-name))
  ;;                                          (projectile-find-file)))
  
  (defun consult-projectile-switch-persp (&optional project)
    "Create persp with projectile project."
    (interactive)
    (ignore-errors
      (persp-switch (projectile-project-name project))))
  
  (advice-add 'consult-projectile--file :before 'consult-projectile-switch-persp)
  ;; (advice-add 'consult-projectile-find-file :before 'consult-projectile-switch-persp)
  (advice-add 'consult-projectile-find-dir :before 'consult-projectile-switch-persp)
  (advice-add 'consult-projectile-recentf :before 'consult-projectile-switch-persp)
  (advice-add 'consult-projectile-switch-to-buffer :before 'consult-projectile-switch-persp)

  (defun amsha/launch-lsp-mode-after-switch ()
    (when (and (derived-mode-p 'prog-mode)
               (member (s-replace "-mode" "" (symbol-name major-mode))
                       (--map (cdr it) lsp-language-id-configuration)))
      (lsp)))

  (add-hook 'persp-switch-hook #'amsha/launch-lsp-mode-after-switch))

(use-package persp-harpoon
  :straight (persp-harpoon :type git :host github :repo "ahmed-shariff/persp-harpoon")
  :custom
  (persp-harpoon-keymap-prefix-key "C-c h")
  :config
  (persp-harpoon-mode t)
  (persp-harpoon-configure-for-perspective))

;; popper ****************************************************************************************************************

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          ("\\*Warnings\\*" . hide)
          help-mode
          compilation-mode)
        popper-group-function #'popper-group-by-perspective
        ;; Copied from `popper-select-popup-at-bottom'
        popper-display-function (lambda (buff alist)
                                  (let ((alist-modified (append alist
                                                                `((window-height . ,popper-window-height)
                                                                  (side . bottom)
                                                                  (slot . 1)))))
                                        (if (popper--suppress-p buff)
                                            (progn
                                              (message (format "Hidden buffer %s" (buffer-name buff)))
                                              ;; This is from the `display-buffer-no-window' function,
                                              ;; which returns 'fail when the 'allow-no-window is set in alist.
                                              'fail)
                                          (select-window (display-buffer-in-side-window buff alist-modified))))))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints
  
;;harpoon ****************************************************************************************************************
;; (use-package harpoon
;;   :demand t
;;   :bind
;;   (("C-c h h" . harpoon-toggle-quick-menu)
;;    ("C-c h H" . harpoon-quick-menu-hydra)
;;    ("C-c h <return>" . harpoon-add-file)
;;    ("C-c h c" . harpoon-clear)
;;    ("C-c h 1" . harpoon-go-to-1)
;;    ("C-c h 2" . harpoon-go-to-2)
;;    ("C-c h 3" . harpoon-go-to-3)
;;    ("C-c h 4" . harpoon-go-to-4)
;;    ("C-c h 5" . harpoon-go-to-5)
;;    ("C-c h 6" . harpoon-go-to-6)
;;    ("C-c h 7" . harpoon-go-to-7))
;;   :config
;;   (defun amsha/harpoon--get-file-buffers ()
;;     "Get the file buffers from harpoon."
;;     (--map (buffer-name (find-file-noselect it)) (delete "" (split-string (harpoon--get-file-text) "\n")))))

;;treemacs setup**********************************************************************************************************
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         35
          treemacs-python-executable             "python")

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

(use-package treemacs-perspective
  :after treemacs perspective
  :config (treemacs-set-scope-type 'Perspectives))

;; dogears *************************************************************************************
(use-package dogears
  :straight (dogears :host github :repo "alphapapa/dogears.el"
                     :files (:defaults (:exclude "helm-dogears.el")))
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . consult-dogears)
              ("M-g r" . dogears-remember)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (advice-add 'end-of-buffer :before #'dogears-remember)
  (advice-add 'beginning-of-buffer :before #'dogears-remember))

;; pomodoro*************************************************************************************
(use-package pomm
  :straight t
  :commands (pomm)
  :config
  (pomm-mode-line-mode)

  (setq pomm-audio-enabled nil)

  (defun pomm--play-sound-file (kind)
    "Alternative function for `pomm--maybe-play-sound'."
    (when (not (eq kind 'tick))
      (invert-face 'mode-line)
      (run-with-timer 0.1 nil #'invert-face 'mode-line)
      (when pomm-audio-enabled
          (when-let (sound (alist-get kind pomm-audio-files))
            (play-sound-file sound 0.05)))))

  (advice-add 'pomm--maybe-play-sound :override #'pomm--play-sound-file))

;;atomic-chrome*********************************************************************************
(use-package atomic-chrome
  :defer t
  :after projectile
  :init (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-url-major-mode-alist '(("overleaf\\.com" . latex-mode))
        atomic-chrome-extension-type-list '(ghost-text)
        atomic-chrome-buffer-open-style 'frame
        atomic-chrome-default-major-mode 'markdown-mode)

  (defun atomic-chrome-setup (socket url title text)
    (with-current-buffer (atomic-chrome-get-buffer-by-socket socket)
      (projectile-set-buffer-directory)))
      ;; (write-file ".temp-atomic-chrome-file.tex")))

  (advice-add 'atomic-chrome-create-buffer :after #'atomic-chrome-setup))

;;latex setup***********************************************************************************

;; (use-package company-auctex
;;   :after (auctex)
;;   :config
;;   (company-auctex-init))

(use-package reftex
  :config
  (keymap-set reftex-mode-map "C-c [" nil))

(use-package latex
  :straight auctex
  :defer t
  :bind (:map LaTeX-mode-map
         ("C-c [" . org-ref-insert-link)
         ("C-c o o" . org-ref-latex-click))
  :hook ((LaTeX-mode-hook . turn-on-outline-minor-mode)
         (latex-mode-hook . turn-on-outline-minor-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (latex-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . turn-on-reftex)
         (latex-mode-hook . turn-on-reftex))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-source-correlate-method '(
                                      (dvi . source-specials)
                                      (pdf . synctex))
        TeX-source-correlate-start-server t
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Default" "Hyperref")
        TeX-PDF-from-DVI "Dvips")
  (TeX-global-PDF-mode t)
  (setq outline-minor-mode-prefix "\C-c \C-o")
  
  (defun turn-on-outline-minor-mode ()
    "."
    (outline-minor-mode 1))

  (defun extract-bib-from-master-bibfile ()
    (interactive)
    (let ((new-buffer-name (format "bibliography-%s.bib" (gensym)))
          (results '()))
      (goto-char (point-min))
      (while (re-search-forward "\\\\bibitem.*\n.*{\\(.*\\)}" nil t)
        (let ((key (match-string 1)))
          (save-window-excursion
            (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
              (bibtex-completion-show-entry (list key))
              (bibtex-copy-entry-as-kill)
              (push (pop bibtex-entry-kill-ring) results)))))
      (with-current-buffer (get-buffer-create new-buffer-name)
        (insert (string-join results)))
      (pop-to-buffer new-buffer-name)))

  ;; After changing this value, run (font-latex-make-user-keywords) and (font-lock-fontify-buffer)
  (setq font-latex-user-keyword-classes '(("positive-comment"
                                           (("p" "{"))
                                           (:foreground "aquamarine")
                                           command)
                                          ("negative-comment"
                                           (("n" "{"))
                                           (:foreground "IndianRed")
                                           command)
                                          ("rev" (("rev" "{")) (:background "blue") command)
                                          ("autoref" (("autoref" "{")) 'font-lock-constant-face command)
                                          ("red" (("red" "{")) (:background "red") command)
                                          ("green" (("green" "{")) (:background "Springgreen4") command)
                                          ("mg-comment" (("mg" "{")) (:background "blue") command)
                                          ("shf-comment" (("shf" "{")) (:background "DarkGreen") command) 
                                          ("kf-comment" (("kf" "{")) (:background "Sienna") command)  
                                          ("dh-comment" (("dyh" "{")) (:background "Firebrick") command)
                                          ("rc-commen" (("rc" "{")) (:background "orange") command)
                                          ("ly-commen" (("ly" "{")) (:background "blue1") command)
                                          ("jj-commen" (("jj" "{")) (:background "magenta") command)
                                          ("ld-commen" (("ld" "{")) (:background "cyan") command)
                                          ("pp-comment" (("ppi" "{")) (:background "DarkSlateBlue") command))))

(use-package lsp-latex
  :config
  (setq lsp-tex-server 'texlab
        lsp-latex-texlab-executable "~/.emacs.d/var/texlab.exe"))

;; (defun activate-preview-mode ()
;;   (load "preview-latex.el" nil t t))

;; (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
;; (add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
;; ;; (add-hook 'LaTeX-mode-hook 'activate-preview-mode)
;; ;; (add-hook 'laTeX-mode-hook 'activate-preview-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'latex-mode-hook 'flyspell-mode)
;; (setq outline-minor-mode-prefix "\C-c \C-o")


;;magit******************************************************************
(use-package git-gutter
  :straight git-gutter-fringe
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (setq git-gutter-fr:side 'right-fringe)
  (unless nil ;; dw/is-termux
    (require 'git-gutter-fringe)
    (set-face-foreground 'git-gutter-fr:added "LightGreen")
    (fringe-helper-define 'git-gutter-fr:added nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
    (fringe-helper-define 'git-gutter-fr:modified nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"))

  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package magit-todos
  :straight (magit-todos :type git :host github :repo "alphapapa/magit-todos"
                         :fork (:host github :repo "ahmed-shariff/magit-todos"))
  :after (magit)
  :config
  (add-to-list 'magit-todos-exclude-globs "*.ipynb")
  (magit-todos-mode))

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 100  ;; TODO: find a way to get this from font attribute
                    :italic t))))

;;origami*****************************************************************************
;; (use-package origami
;;   :straight (origami :type git :host github :repo "elp-revive/origami.el")
;;   :bind ("C-+" . hydra-origami/body)
;;   :hook ((prog-mode . origami-mode))
;;   :hydra (hydra-origami (:color red)
;;    "
;;   [_o_] Open node    [_n_] Next fold       [_f_] toggle forward  [_s_] Show current only
;;   [_c_] Close node   [_p_] Previous fold   [_a_] toggle all      [_q_] quit
;;   "
;;    ("o" origami-open-node)
;;    ("c" origami-close-node)
;;    ("n" origami-next-fold)
;;    ("p" origami-previous-fold)
;;    ("f" origami-forward-toggle-node)
;;    ("a" origami-toggle-all-nodes)
;;    ("s" origami-show-only-node)
;;    ("q" nil)))

;; ;;hide/show
;; (defun toggle-selective-display (column)
;;   (interactive "P")
;;   (set-selective-display
;;    (or column
;;        (unless selective-display
;; 	 (1+ (current-column))))))

;; (defun toggle-hiding (column)
;;   (interactive "P")
;;   (if hs-minor-mode
;;       (if (condition-case nil
;; 	      (hs-toggle-hiding)
;; 	    (error t))
;; 	  (hs-show-all))
;;     (toggle-selective-display column)))

;; (load-library "hideshow")
;; (global-set-key (kbd "C-+") 'toggle-hiding)
;; (global-set-key (kbd "C-\\") 'toggle-selective-display)

;; (defun add-hs-hook (mode-list)
;;   "MODE-LIST:  list of modes that needs 'hs-minor-mode' hook."
;;   (dolist (mode mode-list)
;;     (add-hook mode 'hs-minor-mode)))

;; (add-hs-hook (list 'python-mode-hook 'lisp-mode-hook))

;;ts-fold*****************************************************************************
;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode 1))

;; (use-package tree-sitter-langs)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :bind ("C-+" . hydra-ts-fold/body)
  :hydra (hydra-ts-fold (:color red)
   "
  [_o_] open node    [_O_] open all nodes    [_t_] toggle
  [_c_] close node   [_C_] close all nodes   [_r_] open recurse    [_q_] quit
  "
   ("o" ts-fold-open)
   ("c" ts-fold-close)
   ("O" ts-fold-open-all)
   ("C" ts-fold-close-all)
   ("t" ts-fold-toggle)
   ("r" ts-fold-open-recursively)
   ("q" nil))
  ;; :config
  ;; (global-ts-fold-mode 1)
  )

(use-package ts-fold-indicators
  :after (ts-fold)
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold")
  ;; :config
  ;; (global-ts-fold-indicators-mode 1)
  )

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "team-name" ; add team name here
   :default t
   :client-id (gethash 'slack-client-id configurations "123231423499.234123421342")
   :client-secret (gethash 'slack-client-secret configurations "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
   :token (gethash 'slack-token configurations
		   "xoxp-111111111111-222222222222-333333333333-44444444444444444444444444444444")
   :subscribed-channels '() ;add chanel list here
   :full-and-display-names t))


(use-package visual-fill-column)

(use-package web-search
  :bind ("\C-c/" . web-search)
  :config
  (setq web-search-default-provider "Google"
        web-search-providers (mapcar (lambda (provider)
                                       (if (string-equal "DuckDuckGo" (car provider))
                                           (-replace-at 1 "https://duckduckgo.com/?q=%s" provider)
                                         provider))
                                     web-search-providers))
  (push '("google scholar ca" "https://scholar.google.ca/scholar?hl=en&as_sdt=0%%2C5&q=%s") web-search-providers)
  
  (defun amsha-web-search ()
    "Wrapper to quick pick providers and tag."
    (interactive)
    (let* ((query
            (let ((initial (if (use-region-p)
                               (buffer-substring-no-properties (region-beginning) (region-end))
                             (current-word)))
                  (prompt "Search: "))
              (read-string prompt initial)))
           (tags (web-search--tags))
           (selection-list (append
                            (mapcar (lambda (x) (cons (format x) :tag)) tags)
                            (mapcar (lambda (x) (cons (car x) :provider)) web-search-providers))))
      (ivy-read "Search in: " selection-list
                :preselect web-search-default-provider
                :action (lambda (selection)
                          (cond
                           ((eq (cdr selection) :provider) (web-search query (list (car selection))) nil)
                           ((eq (cdr selection) :tag) (web-search query nil (car selection)))))))))
    
;;alert mode************************************************************
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;;weather data**********************************************************
(use-package biome
  :straight (:host github :repo "SqrtMinusOne/biome")
  :custom
  (biome-query-coords
   '(("Kelowna"	49.88307 -119.48568)
     ("Kandy" 7.2906 80.6336))))

(use-package wttrin
  :straight (:host github :repo "ahmed-shariff/emacs-wttrin")
  :custom
  (wttrin-default-cities '("Kelowna" "Kandy")))

;;dashboard*************************************************************
(use-package dashboard
  :bind
  (("C-c o d" . dashboard-open))
  :hook ((dashboard-mode . (lambda () (setq default-directory "~/"))))
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-footer-messages banner-text-list)
  :config
  (defmacro amsha/dashboard-insert-section
      (section-name icon list list-size shortcut-id shortcut-char action &rest widget-params)
    "Add a section with SECTION-NAME and LIST of LIST-SIZE items to the dashboard.

SHORTCUT-CHAR is the keyboard shortcut used to access the section.
ACTION is theaction taken when the user activates the widget button.
WIDGET-PARAMS are passed to the \"widget-create\" function."
    `(progn
       (dashboard-insert-heading ,section-name
                                 (if (and ,list ,shortcut-char dashboard-show-shortcuts) ,shortcut-char)
                                 ,icon)
       (if ,list
           (when (and (dashboard-insert-section-list
                       ,section-name
                       (dashboard-subseq ,list ,list-size)
                       ,action
                       ,@widget-params)
                      ,shortcut-id ,shortcut-char)
             (dashboard-insert-shortcut ,shortcut-id ,shortcut-char ,section-name))
         (insert (propertize "\n    --- No items ---" 'face 'dashboard-no-items-face)))))

  ;; (defun dashboard-insert-sprints (list-size)
  ;;   "Add the list of LIST-SIZE items."
  ;;   (let ((sprints (condition-case nil (amsha/get-sprints '("INPROGRESS")) ((user-error))))
  ;;         (dashboard-set-file-icons nil))
  ;;     (amsha/dashboard-insert-section
  ;;      "Active Sprints:"
  ;;      (all-the-icons-octicon "globe"
  ;;                             :height 1.2
  ;;                             :v-adjust 0.0
  ;;                             :face 'dashboard-heading)
  ;;      sprints
  ;;      list-size
  ;;      'sprints
  ;;      (dashboard-get-shortcut 'sprints)
  ;;      `(lambda (&rest ignore)
  ;;         (org-id-goto (cdr (quote ,el))))
  ;;      (format "%s" (car el)))))

  (defun dashboard-insert-tasks (list-size)
    "Add the list of LIST-SIZE items."
    (let* ((files (org-agenda-files))
           (tasks (if files
                      (--sort
                       (string< (nth 2 it) (nth 2 other))
                       (org-ql-select files '(and (property "ID") (todo "INPROGRESS"))
                        :action (lambda () (list
                                            (format "%s %s %-40s: %s"
                                                    (--> "‣";;(subseq (org-entry-get (point) "TODO") 0 2)
                                                         (propertize it 'face (org-get-todo-face "INPROGRESS")))
                                                    (--> (alist-get "PRIORITY" (org-entry-properties) nil nil #'string-equal)
                                                         (format "%s "
                                                                 (propertize
                                                                  (concat
                                                                   (pcase it
                                                                     ("A" "★ ")
                                                                     ("B" "☆ ")
                                                                     ("C" "ℹ "))
                                                                   it)
                                                                  'face (pcase it
                                                                          ("A" '((:foreground "SeaGreen2")))
                                                                          ("B" '((:foreground "DarkOrange")))
                                                                          ("C" '((:foreground "pink4")))))))
                                                    (--> (buffer-file-name)
                                                         (format "%s/%s" (f-base (f-parent (f-parent it))) (file-name-base it))
                                                         (propertize it 'face 'shadow))
                                                    (amsha/org-repalce-link-in-string
                                                     (org-no-properties (org-get-heading t t t t))))
                                            (org-id-get)
                                            (alist-get "PRIORITY" (org-entry-properties) nil nil #'string-equal)))))
                    nil))
           (dashboard-set-file-icons nil))
      (amsha/dashboard-insert-section
       "Active Tasks:"
       (all-the-icons-octicon "checklist"
                              :height 1.2
                              :v-adjust 0.0
                              :face 'dashboard-heading)
       tasks
       list-size
       'tasks
       (dashboard-get-shortcut 'tasks)
       `(lambda (&rest ignore)
          (org-id-goto (cadr (quote ,el))))
       (format "%s" (car el)))))

  (defun dashboard-insert-day-to-day-task (list-size)
    "Add day-to-day tasks."
    (let* ((today-day-number (org-today))
             ;; (scheduled-day-number (org-time-string-to-absolute
                                    ;; (org-element-timestamp-interpreter scheduled-date 'ignore)))
             ;; (difference-days (- today-day-number scheduled-day-number))
             ;; (relative-due-date (org-add-props (org-ql-view--format-relative-date difference-days) nil
                                  ;; 'help-echo (org-element-property :raw-value scheduled-date)))
           (tasks (org-ql-select "~/Documents/org/brain/work/project_boards/day-to-day.org"
                    '(todo "INPROGRESS" "TODO")
                    :action (lambda ()
                              (--> (list
                                    (--> (cl-subseq (org-entry-get (point) "TODO") 0 1)
                                         (propertize it 'face (org-get-todo-face "INPROGRESS")))
                                    (--> (org-entry-get (point) "DEADLINE" t)
                                         (or (and it
                                                  (pcase (- today-day-number (org-time-string-to-absolute it))
                                                    ((and (pred (< 0)) diff) (format "%dd ago" diff))
                                                    ((and (pred (> 0)) diff) (format "in %dd " (* -1 diff)))
                                                    (_ "today")))
                                             "  -   ")
                                         (format " %-8s " it)
                                         (propertize it 'face 'highlight))
                                    (amsha/org-repalce-link-in-string
                                     (org-no-properties
                                      (org-get-heading t t t t)))
                                    (--> (if (<= (org-current-level) 3)
                                             "-"
                                           (save-excursion
                                             (while (progn (org-up-heading-safe)
                                                           (and (org-current-level)
                                                                (not (eq 3 (org-current-level))))))
                                             (org-get-heading t t t t)))
                                         (pcase (- today-day-number (org-time-string-to-absolute it))
                                           ((and (pred (< 0)) diff) (format "%s / %3dd ago" it diff))
                                           ((and (pred (> 0)) diff) (format "%s / in %3dd " it (* -1 diff)))
                                           (_ (format "%s /   today " it)))
                                         (s-replace "Friday" "Fri" it)
                                         (s-replace "Saturday" "Sat" it)
                                         (s-replace "Sunday" "Sun" it)
                                         (s-replace "Monday" "Mon" it)
                                         (s-replace "Tuesday" "Tue" it)
                                         (s-replace "Wednesday" "Wed" it)
                                         (s-replace "Thursday" "Thu" it)
                                         (propertize it 'face 'shadow)))
                                   (format "%s %s %s %s" (car it) (cadddr it) (cadr it) (caddr it))
                                        (progn
                                          (add-text-properties 0 (length it)
                                                               (list 'dashboard-file (buffer-file-name)
                                                                     'dashboard-loc (point))
                                                               it)
                                          it)))))

           (dashboard-set-file-icons nil))
      (amsha/dashboard-insert-section
       "day-to-day:"
       (all-the-icons-octicon "checklist"
                              :height 1.2
                              :v-adjust 0.0
                              :face 'dashboard-heading)
       tasks
       list-size
       'day-to-day
       (dashboard-get-shortcut 'day-to-day)
       `(lambda (&rest ignore)
          (let ((buffer (find-file-other-window (get-text-property 0 'dashboard-file ,el))))
            (with-current-buffer buffer
              (goto-char (get-text-property 0 'dashboard-loc ,el))
              (switch-to-buffer buffer))))
       el)))

  (defun dashboard-insert-links (list-size)
    "Add the list of LIST-SIZE items."
    (let ((dashboard-set-file-icons nil)
          (links (list (cons (concat (all-the-icons-octicon
                                      "logo-github"
                                      :height 1 :v-adjust 0.0 :face 'magit-hash)
                                     " Github")
                             "https://github.com/ahmed-shariff")
                       (cons (concat (all-the-icons-octicon
                                      "pulse"
                                      :height 1 :v-adjust 0.0 :face 'magit-hash)
                                     " gists")
                             "https://gist.github.com/ahmed-shariff")
                       (cons (concat (all-the-icons-octicon
                                      "home"
                                      :height 1 :v-adjust 0.0 :face 'magit-hash)
                                     " homepage")
                             "https://shariff-faleel.com"))))
      (amsha/dashboard-insert-section
       "Quick links:"
       (all-the-icons-octicon "info"
                              :height 1.2
                              :v-adjust 0.0
                              :face 'dashboard-heading)
       links
       list-size
       'quick-links
       (dashboard-get-shortcut 'quick-links)
       `(lambda (&rest ignore)
          (browse-url ,(cdr el)))
       (format "%s" (car el)))))

  ;; (add-to-list 'dashboard-item-generators  '(sprints . dashboard-insert-sprints))
  (add-to-list 'dashboard-item-generators  '(tasks . dashboard-insert-tasks))
  (add-to-list 'dashboard-item-generators  '(quick-links . dashboard-insert-links))
  (add-to-list 'dashboard-item-generators  '(day-to-day . dashboard-insert-day-to-day-task))
  ;; (add-to-list 'dashboard-item-shortcuts '(sprints . "s"))
  (add-to-list 'dashboard-item-shortcuts '(tasks . "t"))
  (add-to-list 'dashboard-item-shortcuts '(quick-links . "l"))
  (add-to-list 'dashboard-item-shortcuts '(day-to-day . "d"))
  (dashboard-modify-heading-icons '(;;(sprints . "globe")
                                    (day-to-day . "globe")
                                    (tasks . "checklist")
                                    (quick-links . "info")))

  ;; From https://github.com/emacs-dashboard/emacs-dashboard/issues/471
  (advice-add #'dashboard-replace-displayable :override #'identity)

  (defun amsha/dashboard-due-date-for-agenda-n-days () ;; 20 days
    (time-add (current-time) (* 86400 20)))

  (advice-add 'dashboard-due-date-for-agenda :override #'amsha/dashboard-due-date-for-agenda-n-days)

  (defun amsha/dashboard-agenda--formatted-time (oldfun)
    (let ((time-string (funcall oldfun))
          (today-day-number (org-today)))
      (format "%s  %s"
              time-string 
              (--> (org-entry-get (point) "DEADLINE" t)
                   (or (and it
                            (pcase (- today-day-number (org-time-string-to-absolute it))
                              ((and (pred (< 0)) diff) (format "%dd ago" diff))
                              ((and (pred (> 0)) diff) (format "in %dd " (* -1 diff)))
                              (_ "today")))
                       "  -  ")
                   (format " %-7s " it)
                   (propertize it 'face 'highlight)))))

  (advice-add 'dashboard-agenda--formatted-time :around #'amsha/dashboard-agenda--formatted-time)
  
  (setq dashboard-startup-banner "~/.emacs.d/customFiles/banner.png"
        dashboard-banner-logo-title nil
        dashboard-startup-banner banner-images-list
        dashboard-image-banner-max-height 200
        dashboard-image-banner-max-width 200
        dashboard-items '((quick-links . 5)
                          (day-to-day . 50)
                          ;; (sprints . 50)
                          (agenda . 50)
                          (tasks . 50)
                          (projects . 5)
                          (recents  . 5)
                          (registers . 5)
                          (bookmarks . 5))
        dashboard-set-navigator t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-agenda-sort-strategy '(time-up)
        dashboard-week-agenda t
        dashboard-agenda-prefix-format " %-11s %-20:c")
  (dashboard-setup-startup-hook))

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(use-package web-mode 
  :mode "\\.phtml\\'"
  :mode	"\\.tpl\\.php\\'"
  :mode	"\\.php\\'"
  :mode	"\\.[agj]sp\\'"
  :mode	"\\.as[cp]x\\'"
  :mode	"\\.erb\\'"
  :mode "\\.html?\\'"
  :mode "\\.js[x]?\\'"
  :mode "\\.ts[x]?\\'"
  :mode "\\.svelte\\'"
  :bind ("C-c s" . switch-web-js2)
  :config
  (setq web-mode-content-types-alist '(("jsx" . ".*\\.js[x]?"))
        web-mode-css-indent-offset 4
        web-mode-attr-value-indent-offset 4
        web-mode-indent-style 4
        web-mode-sql-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-attr-indent-offset 4
        web-mode-markup-indent-offset 4)

  (defun switch-web-js2 ()
    "Swicth between web mode and js2 mode."
    (interactive)
    (cond
     ((equal major-mode 'web-mode)
      (js2-mode))
     ((equal major-mode 'js2-mode)
      (web-mode))
     (t (error "Only when in web-mode or js2-mode")))))


(use-package web-narrow-mode
  :hook 'web-mode)

;; (use-package js2-mode
;;   :mode "\\.js\\'"
;;   :bind ("C-c s" . switch-web-js2))
    
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :defer t
  :hook ((json-mode . (lambda () (setq tab-width 2
                                       js-indent-level 2)))))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package plantuml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path "~/.emacs.d/customFiles/plantuml-mit-1.2023.12.jar")
  (setq org-plantuml-jar-path "~/.emacs.d/customFiles/plantuml-mit-1.2023.12.jar")
  (setq plantuml-default-exec-mode 'jar))
  ;; (setq plantuml-exec-mode "jar")
  ;; (plantuml-set-exec-mode "jar"))

;; csharp #####################################################################
;; (use-package tree-sitter)
;; (use-package tree-sitter-langs)

;; Unity setup#################################################################
;; To set up:
;; 1. build using `unity-build-code-shim'
;; 2. Add the code.exe at .emacs.d/var/unity ad the editor to unity
;; 3. Set the args to: emacsclient -n +$(Line):$(Column) $(File)
(use-package unity
  :defer t
  :straight (unity :type git :host github :repo "elizagamedev/unity.el"
                   :files ("*.el" "*.c"))
  :config
  (setq unity-vcvarsall-file (gethash 'unity-vcvarsall-file configurations))
  (unity-mode 1))

;; matlab setup#################################################################
;; (use-package matlab-mode)

;;Docker
(use-package docker
  :bind ("C-c d" . docker))

(use-package ahk-mode
  :defer t
  :mode	"\\.ahk\\'"
  :straight (ahk-mode :type git :host github :repo "punassuming/ahk-mode"
                      :fork (:host github :repo "tu10ng/ahk-mode")))

;;arxiv mode
(use-package arxiv-mode
  :commands (arxiv-read-new arxiv-read-recent arxiv-search)
  :straight (arxiv-mode :type git :host github :repo "fizban007/arxiv-mode"))

;; company-tabnin********************************************************

;; (use-package company-tabnine
;;   :config
;;   (when (gethash 'use-tabnine configurations t)
;;     (push 'company-tabnine company-backends))
;;   (defun tabnine-toggle ()
;;     "Toggle tabnine."
;;     (interactive)
;;     (--> '(company-tabnine :with company-yasnippet)
;;          (if (member it company-backends)
;;              (progn
;;                (company-tabnine-kill-process)
;;                (message "Started")
;;                (setq company-backends
;;                      (delete it company-backends)))
;;            (message "Stopped")
;;            (add-to-list 'company-backends it)))))


;; emacs discrod plugin
(use-package elcord
  :defer t
  :config
  (setq elcord-display-buffer-details nil)
  (elcord-mode))
;; (setq elcord-client-id (gethash 'elcord-client-id configurations))

(use-package buttercup)

;; processing-mode*******************************************************
(use-package processing-mode
  :mode "\\.pde\\'"
  :config
  (setq processing-location "~/packages/processing/processing-java"
        processing-sketchbook-dir "~/Documents/Processing/sketchbook"))

(use-package processing-snippets
  :after (yasnippet)
  :config
  (autoload 'processing-snippets-initialize "processing-snippets" nil nil nil)
  (processing-snippets-initialize))

;;code to run at the end!************************************************

;; (defun company-mode/backend-with-yas (backend)
;; "Adding yasnippet after company-mode.
;; BACKEND: the backend"
;;   (if (and (listp backend) (member 'company-yasnippet backend))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;;yasnippets company conflict resolution
;(provide .emacs)
;;; .emacs ends here
