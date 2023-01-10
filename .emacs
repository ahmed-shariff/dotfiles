;;; package -- Summary

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

(maximize)
(add-hook 'after-make-frame-functions 'maximize)
(setq ns-auto-hide-menu-bar t)
(tool-bar-mode 0)

(server-start)
(require 'configurations)

;;straight.el setup*************************************************
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
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
      straight-host-usernames '((github . "ahmed-shariff")))

(defvar my-package-list '(org org-contrib elgrep dired+
					   ;; org-capture-pop-frame
					   use-package spaceline-all-the-icons
					   org-bullets latex-math-preview csproj-mode csharp-mode plantuml-mode
					   docker dockerfile-mode ascii-art-to-unicode org-ref yasnippet-snippets 2048-game
					   avy expand-region diminish amx flx
					   dashboard dired-single ibuffer-vc projectile micgoline dired-hide-dotfiles
					   dired-sidebar magit stumpwm-mode all-the-icons-dired hledger-mode vlf elpy
					   yasnippet company-jedi jedi sr-speedbar latex-preview-pane
					   exec-path-from-shell slime-company slime
					   slim-mode python-mode flycheck company-quickhelp company-c-headers company-anaconda))

(mapcar #'straight-use-package
	my-package-list)

(defun straight-visit-package-projectile (&rest args)
  "Open projectile after visiting straight repo."
  (let ((b (current-buffer))
        (p (persp-current-name)))
    (projectile-find-file)
    (persp-add-buffer b)
    (with-perspective p
      (persp-remove-buffer b))))

(advice-add 'straight-visit-package :after #'straight-visit-package-projectile)

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
 '(company-c-headers-path-system
   '("/usr/include/" "/usr/local/include/" "/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/"))
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(ede-project-directories '("/media/Files/Research/FoodClassification/deployment"))
 '(elpy-rpc-python-command "python3")
 '(explicit-shell-file-name "/bin/zsh")
 '(ledger-post-amount-alignment-at :decimal)
 '(ledger-reconcile-default-commodity nil)
 '(ledger-reports
   '(("asd" "ledger")
     ("a" "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(org-export-backends '(ascii html icalendar latex md))
 '(prolog-system 'swi)
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((dired-omit-files . "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.log$")
     (magit-todos-exclude-globs "Assets/Oculus/")
     (magit-todos-exclude-globs . "Assets/Oculus/")
     (org-download-image-dir . "figures/notes")
     (eval font-lock-add-keywords nil
           '(("^\\* .*\\(([0-9]\\{4\\})\\)" 1 'org-tag t)
             ("^\\* .*\\(([0-9]\\{4\\})\\).*\\(\\[.*\\]\\)" 2 'org-level-7 t)
             ("`\\([a-zA-Z].*[a-zA-Z\\.]\\)`" 1 'org-quote t))
           'append)
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
 '(company-scrollbar-bg ((t (:background "#000000"))))
 '(company-scrollbar-fg ((t (:background "#555555"))))
 '(company-tooltip ((t (:inherit default :background "#000000"))))
 '(company-tooltip-annotation ((t (:inherit font-lock-builtin-face))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(markdown-code-face ((t (:inherit consolas))))
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

(customize-set-value 'create-lockfiles nil "It's not being ignored propperly?")

;; (setq use-package-compute-statistics t)

(defmacro em (&rest args)
  "Call `messaage' ARGS passed as args of `message' & retur the first argument passed.
Used for debugging."
  ;; `(signal 'error ""))
  `(progn
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

(use-package beacon
  :demand
  :custom
  (beacon-push-mark nil)
  (beacon-color "#cc342b")
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.3)
  :config
  (beacon-mode))

(setq backup-directory-alist `(("." . "~/.backups_emacs"))
      backup-by-copying t
      delete-old-versions t
      ;; Following vertico readme
      read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t)


;; from https://github.com/daviwil/dotfiles/blob/master/Emacs.org
(defun amsha/visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun git-message ()
  (format "[%s] %s"
          (gethash 'system-name configurations "Check system-name in configurations.el")
          (format-time-string "%Y-%m-%dT%H:%M:%S%:z")))

(use-package visual-fill-column
  :defer 
  :hook ((org-mode LaTeX-mode latex-mode markdown-mode org-roam-mode) . amsha/visual-fill))

;; Loading symlink-fix (https://www.emacswiki.org/emacs/symlink-fix.el)*************
;; Had to install this to resolve the symlink issues that cropped up with using org in both OS's
(if (not (eq system-type 'windows-nt))
    (progn
      (setq symlink-overload-expand-file-name-p t)
      (require 'symlink-fix)
      (setq expand-file-name-resolve-symlinks-p t))
  (set-face-attribute 'default nil
                      :family "Consolas" :height 105))

;;enable ido mode
;; (require 'ido)
;; (ido-mode t)

(use-package golden-ratio)

(use-package diminish)

(use-package persistent-scratch
  :init (persistent-scratch-setup-default))

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


(use-package powershell)


;;evil *************************************************************
(use-package evil
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
        evil-cross-lines t)

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

  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'view-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'emacs)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC"))

(use-package evil-embrace
  :after evil
  :config
  (global-evil-surround-mode 1)
  (evil-embrace-enable-evil-surround-integration))

;;selectrum  *******************************************************
;; (use-package prescient
;;   :config
;;   (prescient-persist-mode +1))

;;(use-package company-prescient)
;; (use-package selectrum-prescient
;;   :config
;;   (selectrum-prescient-mode +1)
;;   (setq selectrum-prescient-enable-filtering nil))

(use-package orderless
  :custom (completion-styles '(orderless))
  :config
  (defun amsha/without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (defun amsha/match-components-literally (orig-fun &rest args)
    "Funtion to add as advice for interactive functions that will always use lietral completion."
    (interactive (lambda (spec) (advice-eval-interactive-spec spec)))
    (let ((orderless-matching-styles '(orderless-literal)))
      (apply orig-fun args)))

  (advice-add #'org-set-property :around #'amsha/match-components-literally)
  
  (setq orderless-matching-styles '(orderless-literal orderless-regexp)
        orderless-style-dispatchers '(amsha/without-if-bang))

  ;; copied from https://github.com/oantolin/orderless/issues/30
  (defun cq-annotation (x y metadata)
    (when-let* ((ann (completion-metadata-get metadata 'annotation-function))
                (str (funcall ann x)))
      (string-match-p y str)))

  (defun cq-completion-all-completions-on-annotations (orig str table pred _point &optional metadata)
    (let ((filter))
      (setq str
            (replace-regexp-in-string "@[^ ]*" (lambda (x)
                                                    (setq x (substring x 1))
                                                    (unless (string-blank-p x)
                                                      ;;(push (substring x (1+ (string-match-p "=" x))) filter))
                                                      (push x filter))
                                                    "")
                                      str))
      ;; TODO correct point computation
      (when-let* ((result (funcall orig str table pred (length str) metadata)))
        ;; (when-let (queries (and filter
        ;;                         (or (alist-get cat cq-queries)
        ;;                             (and (eq cat 'consult-multi)
        ;;                                  (apply #'append (mapcar #'cdr cq-queries))))))
          (let* ((last (last result))
                 (base (cdr last))) ;; why is this here?
            (setcdr last nil)
            (setq result (seq-filter (lambda (str)
                                       ;; (when (eq cat 'consult-multi)
                                       ;;   (setq x (cdr (get-text-property 0 'consult-multi x))))
                                       (not (seq-find
                                             (lambda (f)
                                               ;; (or
                                               ;;  (when-let* ((pos (or (string-match-p "=" f) (length f))))
                                               ;;              ;;(key (intern-soft (substring f 0 pos))))
                                               ;;              ;; (fun (alist-get key queries)))
                                               ;;    (not (funcall fun x (if (< pos (length f)) (substring f (1+ pos)) "") metadata)))
                                               ;;  (when-let (fun (alist-get (aref f 0) queries))
                                               ;;    (not (funcall fun x (substring f 1) metadata)))))
                                               (cq-annotation str f metadata))
                                             filter)))
                                     result))
            (when result
              (setq result (nconc result base))))
        result)))

  ;; (advice-add #'completion-all-completions :around #'cq-completion-all-completions-on-annotations)
  ;; (advice-remove #'completion-all-completions #'cq-completion-all-completions-on-annotations)
)

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
        vertico-cycle t
        completion-in-region-function (lambda (&rest args)
                                        (apply (if vertico-mode
                                                   #'consult-completion-in-region
                                                 #'completion--in-region)
                                               args))))

(use-package savehist
  :init
  (savehist-mode))

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
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

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
         ("C-c h" . consult-history)
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
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark :preview-key (kbd "M-.")
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

  (push consult--source-perspective consult-buffer-sources)

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

;;tramp settings ***************************************************
;; See https://stackoverflow.com/questions/6954479/emacs-tramp-doesnt-work for more details
(setq tramp-terminal-type "dumb")

;;ivy-mode *********************************************************
(use-package amx
  :init (amx-mode 1))

(use-package ivy
  :diminish (ivy-mode . "")             ; does not display ivy in the modeline
  ;; :init
  ;; (ivy-mode 1)                          ; enable ivy globally at startup
  :bind (;; ("C-c g" . counsel-git)
	 ;; ("C-c j" . counsel-git-grep) 
	 ;; ("C-c k" . counsel-ag)       
	 ;; ("C-x l" . counsel-locate)   
	 ;; ("C-S-o" . counsel-rhythmbox)
	 ;;("C-x C-f" . counsel-find-file)
	 :map ivy-minibuffer-map        ; bind in the ivy buffer
	 ("RET" . ivy-alt-done))
	 ;;      ("s-<"   . ivy-avy)
	 ;;      ("s->"   . ivy-dispatching-done)
	 ;;      ("s-+"   . ivy-call)
	 ;;      ("s-!"   . ivy-immediate-done)
	 ;;      ("s-["   . ivy-previous-history-element)
	 ;;      ("s-]"   . ivy-next-history-element))
  :config
  (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and
  (setq ivy-height 15)                   ; set height of the ivy window
  (setq ivy-count-format "(%d) ")     ; count format, from the ivy help page
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line) ; Make highlight extend all the way to the right
  ;; TODO testing out the fuzzy search
  (setq ivy-re-builders-alist
      '(;; (read-file-name-internal . ivy--regex-fuzzy)
	;; (internal-complete-buffer . ivy--regex-fuzzy)
	;; (execute-extended-command . ivy--regex-fuzzy)
	;; (amx . ivy--regex-fuzzy)
	(t . ivy--regex-fuzzy))))

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
(use-package helm
  ;; :init (helm-mode 1)
  ;; :bind (("M-y" . helm-show-kill-ring))
  )

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

;;highlight-indent-mode***************************************************************
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-auto-character-face-perc 20))

;;elgrep******************************************************************************
(use-package elgrep
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

;;lsp-mode ***************************************************************************
(use-package lsp-ui
  :after (lsp-mode)
  :init
  (add-hook 'python-mode-hook #'lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover t
	lsp-ui-sideline-delay 1))


(use-package company-box
  :hook (company-mode . company-box-mode))


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
	 (lsp-mode . lsp-enable-which-key-integration))
  
  :init
  (setq lsp-keymap-prefix "C-x l")
  ;; (add-hook 'prog-mode-hook #'lsp)
  ;; (setq lsp-auto-guess-root t)
  ;; (setq lsp-log-io t)
  :config
  ;; (lsp-register-client
  
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
  ;;                   :major-modes '(python-mode)
  ;;                   :remote? t
  ;;                   :server-id 'pyls))
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
   )
  (lsp-register-custom-settings '(("omnisharp.useGlobalMono" "always"))))

;; (use-package lsp-jedi
;;   :straight t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     ;; (add-to-list 'lsp-enabled-clients 'jedi)
;;     ))

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

(use-package lsp-grammarly
  :after (lsp)
  :ensure t
  ;; :hook (text-mode . (lambda ()
  ;;                      (require 'lsp-grammarly)
  ;;                      (lsp-deferred))))  ; or lsp-deferred
  )

(use-package dap-python
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

(use-package rustic)

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

;; pdf
(use-package pdf-tools
  :defer 5
  :config
  (when (gethash 'use-pdf-tools configurations t)
    (when (eq system-type 'gnu/linux)
      (setq pdf-tools-directory "/home/amsha/.emacs.d/straight/repos/pdf-tools/"))
    (pdf-tools-install t)
    (add-hook 'pdf-view-mode-hook '(lambda ()
				     (pdf-misc-size-indication-minor-mode)))))

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
(setq ispell-program-name "hunspell")
(require 'ispell)
(require 'flyspell)

;; (require 'micgoline)
;; (setq powerline-default-separator 'arrow-fade)
;; (use-package spaceline-all-the-icons 
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))
;;(rich-minority-mode 1)

(diminish 'visual-line-mode)
(diminish 'ivy-mode)
(diminish 'projectile-mode "P")

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

(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(require 'dired-sort)

(defhydra hydra-dired (:hint nil :color pink)
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
  ("." nil :color blue))

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "." 'hydra-dired/body)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(use-package ranger
  :custom
  (ranger-override-dired 'ranger)
  :config
  (ranger-override-dired-mode t))

;; ;;org mode*******************************************************
;; ;;rest in ~/.emacs.d/CustomLoadFiles/orgZ.el
(require 'orgZ)

;; ;;enabling company***********************************************
(add-hook 'after-init-hook 'global-company-mode)
;; ;; ;;makes completion start automatically rather than waiting for 3 chars / 0.5sec
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.1)
;;;; GC issue
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 100000000))) ;; This is set in lsp-mode
(add-hook 'focus-out-hook 'garbage-collect)
;; ;; ;;company quickhelp gives docstring info
(company-quickhelp-mode 1)
(setq company-quickhelp-delay nil)

;; ;;yasnippet setup************************************************
(use-package yasnippet
  :init (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
        (append yas-snippet-dirs
                `("~/.emacs.d/snippets"))                 ;; personal snippets
        yas-indent-line 'fixed))

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

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font
   t 'unicode (font-spec :family "Segoe UI Emoji") nil 'prepend))

;;slime and cl setup*********************************************
(require 'slim-mode)
;;(load (expand-file-name "/home/amsha/quicklisp/slime-helper.el"))
(setq package-enable-at-startup nil)
;;(setq inferior-lisp-program "F:/Binaries/ccl/wx86cl64.exe")
(setq inferior-lisp-program "sbcl")
(setq slime-auto-connect 'ask)
(setq slime-net-coding-system 'utf-8-unix)
(require  'slime)
(slime-setup
 '(slime-fancy slime-asdf slime-references slime-indentation slime-xref-browser slime-company))
;; (unless package-archive-contents 
;;   (package-refresh-contents))
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

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
(setq python-shell-interpreter "python" python-shell-interpreter-args "-i")
(pyvenv-activate "~/virtualenv/pytorch")
(pyvenv-mode)

(use-package with-venv
  :after (lsp lsp-pyright)
  :config
  (with-venv-advice-add 'lsp-pyright-locate-venv)
  (with-venv-advice-add 'lsp-pylsp-get-pyenv-environment)
  (with-venv-advice-add 'dap-python--pyenv-executable-find))

(use-package poetry
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
(add-hook 'prog-mode-hook
    (lambda ()
      ;; (linum-on)
      (setq indent-tabs-mode nil)
      (infer-indentation-style)
      ;; (display-line-numbers-mode 1)
      (hl-todo-mode 1)))

;; (use-package linum-relative
;;   :demand
;;   :hook (prog-mode text-mode)
;;   :custom
;;   (linum-relative-backend 'display-line-numbers-mode)
;;   :config
;;   ;; (linum-relative-global-mode))
;;   )

(use-package display-line-numbers
  :hook (prog-mode text-mode)
  :custom
  (display-line-numbers-type 'visual))

(use-package ess)

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
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(add-to-list 'company-backends 'company-c-headers)
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

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
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
(set-frame-parameter (selected-frame) 'alpha '(98 . 92))
(add-to-list 'default-frame-alist '(alpha . (98 . 92)))


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(mode-line ((t (:background "#004276" :foreground "#8cccff" :box nil :height 0.9))))
;;  '(mode-line-inactive ((t (:background "#666666" :foreground "#f9f9f9" :box nil :height 0.9))))
;;  '(sml/global ((t (:foreground "gray50" :inverse-video nil :height 0.9 :width normal)))))

;;projectile mode********************************************************
(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1)
  (setq projectile-git-command "git ls-files --recurse-submodules -zc"))
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
  
  (defun consult-projectile--switch-persp (&optional project)
    (ignore-errors
      (persp-switch (projectile-project-name project))))
  
  (advice-add 'consult-projectile--file :before 'consult-projectile--switch-persp)
  (advice-add 'consult-projectile-find-file :before 'consult-projectile--switch-persp)
  (advice-add 'consult-projectile-find-dir :before 'consult-projectile--switch-persp)
  (advice-add 'consult-projectile-recentf :before 'consult-projectile--switch-persp)
  (advice-add 'consult-projectile-switch-to-buffer :before 'consult-projectile--switch-persp)

  (defun amsha/launch-lsp-mode-after-switch ()
    (when (and (derived-mode-p 'prog-mode)
               (member (s-replace "-mode" "" (symbol-name major-mode))
                       (--map (cdr it) lsp-language-id-configuration)))
      (lsp)))

  (add-hook 'persp-switch-hook #'amsha/launch-lsp-mode-after-switch))
  

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
  :init (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-url-major-mode-alist '(("overleaf\\.com" . latex-mode))
        atomic-chrome-extension-type-list '(ghost-text)
        atomic-chrome-buffer-open-style 'frame
        atomic-chrome-default-major-mode 'markdown-mode))

;;latex setup***********************************************************************************
(defun turn-on-outline-minor-mode ()
  "."
  (outline-minor-mode 1))

(use-package company-auctex
  :after (auctex)
  :config
  (company-auctex-init))

(use-package latex
  :straight auctex
  :defer t
  :bind (:map LaTeX-mode-map
         ("C-c [" . org-ref-insert-link))
  :hook ((LaTeX-mode-hook . turn-on-outline-minor-mode)
         (latex-mode-hook . turn-on-outline-minor-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (latex-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . turn-on-reftex))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t
        reftex-plug-into-AUCTeX t
        TeX-PDF-from-DVI "Dvips")
  (TeX-global-PDF-mode t)
  (setq outline-minor-mode-prefix "\C-c \C-o"))

(use-package lsp-latex
  :config
  (setq lsp-latex-texlab-executable "~/.emacs.d/var/texlab.exe"))

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
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-git-executable "git"
      transient-default-level 7)

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

;;ibuffer****************************************************************
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;origami*****************************************************************************
(use-package origami
  :bind ("C-+" . hydra-origami/body)
  :hook ((prog-mode . origami-mode))
  :hydra (hydra-origami (:color red)
   "
  [_o_] Open node    [_n_] Next fold       [_f_] toggle forward  [_s_] Show current only
  [_c_] Close node   [_p_] Previous fold   [_a_] toggle all      [_q_] quit
  "
   ("o" origami-open-node)
   ("c" origami-close-node)
   ("n" origami-next-fold)
   ("p" origami-previous-fold)
   ("f" origami-forward-toggle-node)
   ("a" origami-toggle-all-nodes)
   ("s" origami-show-only-node)
   ("q" nil)))

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

;;other stuff************************************************************
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
  :config
  (setq web-search-default-provider "DuckDuckGo")
  (setq web-search-providers (mapcar (lambda (provider)
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

(use-package dashboard
  :config
  (defun dashboard-insert-sprints (list-size)
    "Add the list of LIST-SIZE items."
    (let ((sprints (condition-case nil (amsha/get-sprints '("INPROGRESS")) ((user-error))))
          (dashboard-set-file-icons nil))
      (dashboard-insert-section
       (concat (all-the-icons-octicon "globe"
                                      :height 1.2 :v-adjust 0.0 :face 'dashboard-heading)
               " Active Sprints")
       sprints
       list-size
       'sprints
       (dashboard-get-shortcut 'sprints)
       `(lambda (&rest ignore)
          (org-id-goto (cdr (quote ,el))))
       (format "%s" (car el)))))

  (defun dashboard-insert-tasks (list-size)
    "Add the list of LIST-SIZE items."
    (let* ((files (org-agenda-files))
           (tasks (if files
                      (org-ql-select files '(and (property "ID") (todo "INPROGRESS"))
                        :action (lambda () (cons
                                            (format "%s %-40s: %s"
                                                    (--> "‣";;(subseq (org-entry-get (point) "TODO") 0 2)
                                                         (propertize it 'face (org-get-todo-face "INPROGRESS")))
                                                    (--> (buffer-file-name)
                                                         (format "%s/%s" (f-base (f-parent (f-parent it))) (file-name-base it))
                                                         (propertize it 'face 'shadow))
                                                    (org-no-properties (org-get-heading t t t t)))
                                            (org-id-get))))
                    nil))
           (dashboard-set-file-icons nil))
      (dashboard-insert-section
       (concat (all-the-icons-octicon "checklist"
                                      :height 1.2 :v-adjust 0.0 :face 'dashboard-heading)
               " Active Tasks")
       tasks
       list-size
       'tasks
       (dashboard-get-shortcut 'tasks)
       `(lambda (&rest ignore)
          (org-id-goto (cdr (quote ,el))))
       (format "%s" (car el)))))

  (add-to-list 'dashboard-item-generators  '(sprints . dashboard-insert-sprints))
  (add-to-list 'dashboard-item-generators  '(tasks . dashboard-insert-tasks))
  (add-to-list 'dashboard-item-shortcuts '(sprints . "s"))
  (add-to-list 'dashboard-item-shortcuts '(tasks . "t"))
  (dashboard-modify-heading-icons '((sprints . "milestone") (tasks . "check-circle-fill")))
  
  (setq dashboard-startup-banner "~/.emacs.d/customFiles/banner.png"
        dashboard-image-banner-max-height 200
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (tasks . 50)
                          (sprints . 50)
                          (agenda . 50)
                          (registers . 5)
                          (bookmarks . 5))
        dashboard-set-navigator t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-agenda-sort-strategy '(time-up)
        dashboard-week-agenda t
        dashboard-agenda-prefix-format " %-12s %-20:c")
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

(defun switch-web-js2 ()
  "Swicth between web mode and js2 mode."
  (interactive)
  (cond
   ((equal major-mode 'web-mode)
    (js2-mode))
   ((equal major-mode 'js2-mode)
    (web-mode))
   (t (error "Only when in web-mode or js2-mode"))))

(use-package web-mode 
  :mode "\\.phtml\\'"
  :mode	"\\.tpl\\.php\\'"
  :mode	"\\.php\\'"
  :mode	"\\.[agj]sp\\'"
  :mode	"\\.as[cp]x\\'"
  :mode	"\\.erb\\'"
  :mode "\\.html?\\'"
  :mode "\\.js[x]?\\'"
  :bind ("C-c s" . switch-web-js2)
  :config
  (setq web-mode-content-types-alist '(("jsx" . ".*\\.js[x]?"))
        web-mode-sql-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-attr-indent-offset 4
        web-mode-markup-indent-offset 4))

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
  :defer 5
  :hook ((json-mode . (lambda () (setq tab-width 2
                                       js-indent-level 2)))))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package plantuml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path "~/.emacs.d/customFiles/plantuml.jar")
  (setq org-plantuml-jar-path "~/.emacs.d/customFiles/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))
  ;; (setq plantuml-exec-mode "jar")
  ;; (plantuml-set-exec-mode "jar"))

;; csharp #####################################################################
;; (use-package tree-sitter)
;; (use-package tree-sitter-langs)

(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))


;; Unity setup#################################################################
;; To set up:
;; 1. build using `unity-build-code-shim'
;; 2. Add the code.exe at .emacs.d/var/unity ad the editor to unity
;; 3. Set the args to: emacsclient -n +$(Line):$(Column) $(File)
(use-package unity
  :straight (unity :type git :host github :repo "elizagamedev/unity.el"
                   :files ("*.el" "*.c"))
  :config
  (setq unity-vcvarsall-file (gethash 'unity-vcvarsall-file configurations))
  (unity-setup))

;;Docker
(use-package docker
  :bind ("C-c d" . docker))

;;arxiv mode
(use-package arxiv-mode
  :commands (arxiv-read-new arxiv-read-recent arxiv-search)
  :straight (arxiv-mode :type git :host github :repo "fizban007/arxiv-mode"))

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

;; company-tabnin********************************************************

(use-package company-tabnine
  :config
  (when (gethash 'use-tabnine configurations t)
    (push 'company-tabnine company-backends))
  (defun tabnine-toggle ()
    "Toggle tabnine."
    (interactive)
    (--> '(company-tabnine :with company-yasnippet)
         (if (member it company-backends)
             (progn
               (company-tabnine-kill-process)
               (message "Started")
               (setq company-backends
                     (delete it company-backends)))
           (message "Stopped")
           (add-to-list 'company-backends it)))))


;; emacs discrod plugin
(use-package elcord
  :config
  (setq elcord-display-buffer-details nil)
  (elcord-mode))
;; (setq elcord-client-id (gethash 'elcord-client-id configurations))


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

(defun company-mode/backend-with-yas (backend)
"Adding yasnippet after company-mode.
BACKEND: the backend"
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;;yasnippets company conflict resolution
;(provide .emacs)
;;; .emacs ends here
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun yank-pop-forwards (arg)
  "Ha ha ha. 
ARG : the arg"
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)

;; After chaning this value, run (font-latex-make-user-keywords) and (font-lock-fontify-buffer)
(setq font-latex-user-keyword-classes '(("positive-comment"
                                         (("p" "{"))
                                         (:foreground "aquamarine")
                                         command)
                                        ("negative-comment"
                                         (("n" "{"))
                                         (:foreground "IndianRed")
                                         command)
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
                                        ("pp-comment" (("ppi" "{")) (:background "DarkSlateBlue") command)))

