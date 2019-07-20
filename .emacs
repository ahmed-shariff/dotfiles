;;; package -- Summary

;;; Commentary:

;;; Code:

;; -*- emacs-lisp -*-
(require 'package)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;(unless package-archive-contents    ;; Refresh the packages descriptions
;  (package-refresh-contents))
;; (setq package-list '(;anaconda-mode
;;  			  company
;;  			  company-anaconda
;;  			  company-c-headers
;;  			  company-quickhelp
;;  			  dash
;;  			  epl
;;  			  f
;;  			  let-alist
;;  			  macrostep
;;  			  pos-tip
;;  			  python-mode
;;  			  pythonic
;;  			  rich-minority
;;  			  s
;;  			  seq
;; 			  flycheck
;;  			  slim-mode
;;  			  slime
;;  			  slime-company
;;  			  org
;; 			  vlf
;; 			  sr-speedbar
;; 			  micgoline
;; 			  yasnippet
;; 			  elpy
;; 			  ))     ;; List of packages to load



(setq package-list
      '(;anaconda-mode
	circe
	company-anaconda   
	company-auctex     
	company-c-headers  
	company-lua        
	company-php        
	company-quickhelp  
	elpy               
	flycheck           
	js2-mode           
	ledger-mode        
	magit              
	markdown-mode      
	markdown-mode+     
        markdown-preview-mode 
	pdf-tools          
	php-mode          
	projectile         
	python-mode
	pythonic
	rich-minority
	slack              
	slim-mode          
	slime-company      
	sr-speedbar        
	use-package        
	vlf                
	web-mode           
	web-narrow-mode    
	yasnippet          
 	))     ;; List of packages to load

;;mepla setup****************************************************
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") '("org" . "http://orgmode.org/elpa/"));'("elpy" . "http://jorgenschaefer.github.io/packages/"))
;	     '("melpa" . "http://melpa.org/packages/")
;	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'load-path "~/.emacs.d/customFiles")
(add-to-list 'load-path "~/.emacs.d/customFiles/arxiv-mode")
(add-to-list 'package-archives
            '("melpa-stable" . "https://stable.melpa.org/packages/"))


(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(dolist (package package-list)
  (unless (package-installed-p package)  ;; Make sure the Org package is
    (progn
      (print package)
      (package-install package))))           ;; installed, install it if not

(package-initialize)      ;; Initialize & Install Package


(defun fullscreen (&optional f)
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(add-hook 'after-make-frame-functions 'fullscreen)
(setq ns-auto-hide-menu-bar t)
(tool-bar-mode 0)

(server-start)

;; custom variables*******************************************
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(bookmark-save-flag 1)
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/")))
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(ede-project-directories
   (quote
    ("/media/Files/Research/FoodClassification/deployment")))
 '(explicit-shell-file-name "/bin/zsh")
 '(ledger-post-amount-alignment-at :decimal)
 '(ledger-reconcile-default-commodity nil)
 '(ledger-reports
   (quote
    (("asd" "ledger")
     ("a" "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(package-selected-packages
   (quote
    (plantuml-mode jupyter docker dockerfile-mode ascii-art-to-unicode org-ref yasnippet-snippets 2048-game org-brain avy org-capture-pop-frame company-lsp lsp-ui lsp-mode expand-region diminish amx flx counsel ivy dashboard dired-single ibuffer-vc projectile micgoline dired-hide-dotfiles dired-sidebar magit company-lua stumpwm-mode all-the-icons-dired hledger-mode vlf elpy company-auctex auctex pdf-tools yasnippet company-jedi jedi sr-speedbar latex-preview-pane exec-path-from-shell smart-mode-line-powerline-theme slime-company slim-mode python-mode flycheck company-quickhelp company-c-headers company-anaconda)))
 '(prolog-system (quote swi))
 '(sml/mode-width 15)
 '(sml/shorten-modes t)
 '(sml/theme (quote dark))
 '(use-package-always-ensure t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(micgoline-pl-active-green ((t (:inherit mode-line :background "light sky blue" :foreground "#FFFFFF"))))
 '(micgoline-pl-active-red ((t (:inherit mode-line :background "deep sky blue" :foreground "#FFFFFF"))))
 '(micgoline-pl-active-yellow ((t (:inherit mode-line :background "tomato1" :foreground "white"))))
 '(micgoline-pl-inactive-green ((t (:inherit mode-line-inactive :background "slate gray" :foreground "#000000"))))
 '(micgoline-pl-inactive-red ((t (:inherit mode-line-inactive :background "DeepSkyBlue3" :foreground "#FFFFFF"))))
 '(micgoline-pl-inactive-yellow ((t (:inherit mode-line-inactive :background "rosy brown" :foreground "#FFFFFF")))))



;(exec-path-from-shell-initialize)

(require 'configurations)

;;allout
(allout-mode)

;;syntax highlight
(global-font-lock-mode 1)

;;add proper word wrapping
(global-visual-line-mode t)

;;enable ido mode
;; (require 'ido)
;; (ido-mode t)

;;ivy-mode *********************************************************
(use-package amx :ensure t
  :init (amx-mode 1))

(use-package ivy :ensure t
  :diminish (ivy-mode . "")             ; does not display ivy in the modeline
  :init
  (ivy-mode 1)                          ; enable ivy globally at startup
  :bind (("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep) 
	 ("C-c k" . counsel-ag)       
	 ("C-x l" . counsel-locate)   
	 ("C-S-o" . counsel-rhythmbox)
	 
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
  (setq ivy-height 10)                   ; set height of the ivy window
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

;;expand-region **********************************************************************
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;lsp-mode ***************************************************************************
(use-package lsp-ui
  :init
  (add-hook 'python-mode-hook #'lsp-ui-mode))
(use-package company-lsp)
(use-package lsp-mode
  ;;:hook ((python-mode-hook) . lsp))
  :init
  ;(add-hook 'prog-mode-hook #'lsp)
  (setq lsp-auto-guess-root t)
  (setq lsp-print-io t)
  :config
  ;; (lsp-register-client
  
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
  ;;                   :major-modes '(python-mode)
  ;;                   :remote? t
  ;;                   :server-id 'pyls))
  (setq 
   lsp-pyls-configuration-sources ["flake8"]
   lsp-pyls-plugins-jedi-completion-enabled nil
   lsp-pyls-plugins-pydocstyle-enabled t
   lsp-pyls-plugins-pyflakes-enabled nil
   lsp-pyls-plugins-pycodestyle-max-line-length 110
   lsp-pyls-plugins-rope-completion-enabled nil
   lsp-pyls-plugins-pycodestyle-enabled nil
   lsp-pyls-plugins-yapf-enabled nil
   lsp--delay-timer 1))

;;avy *******************************************************************************
(use-package avy
  :bind (("C-S-s" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g l" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1))
  :config
  (avy-setup-default))

;;pdf
(pdf-tools-install)

;;delete-selection-mode
(delete-selection-mode t)
(require 'vlf-setup)

;;speedbar settings
(require 'sr-speedbar)
(global-set-key (kbd "M-s M-s") 'dired-sidebar-toggle-sidebar)
(rich-minority-mode 1)

;; Flycheck: On the fly syntax checking
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
; stronger error display
(defface flycheck-error
  '((t (:foreground "red" :underline (:color "Red1" :style wave) :weight bold)))
  "Flycheck face for errors"
  :group "flycheck")
(setq flycheck-check-syntax-automatically '(mode-enabled new-line))


;;flycheck
;;(require 'flyspell)

;;ispell
(setq ispell-program-name "aspell")
(require 'ispell)
(require 'flyspell)

(require 'micgoline)
(setq powerline-default-separator 'chamfer)


(use-package all-the-icons)
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(require 'dired-sort)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; ;;org mode*******************************************************
;; ;;rest in ~/.emacs.d/CustomLoadFiles/orgZ.el
(require 'orgZ)

;; ;;enabling company***********************************************
(add-hook 'after-init-hook 'global-company-mode)
;; ;; ;;makes completion start automatically rather than waiting for 3 chars / 0.5sec
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 1)
;; ;; ;;company quickhelp gives docstring info
(company-quickhelp-mode 1)
(setq company-quickhelp-delay nil)

;; ;;yasnippet setup************************************************
(require 'yasnippet)
(yas-global-mode 1)

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
(unless package-archive-contents 
  (package-refresh-contents))
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;;python setup*************************************************
 ;(eval-after-load "company"
 ;  '(add-to-list 'company-backends 'company-anaconda)); :with company-capf)))
 ;(add-hook 'python-mode-hook 'anaconda-mode)
 ;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;(add-hook 'python-mode-hook '(company-anaconda 'interactive))
;;(add-to-list 'company-backends 'company-jedi)
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(add-hook 'python-mode-hook 'linum-mode)
					;(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "python" python-shell-interpreter-args "-i")
(pyvenv-activate "~/virtualenv/pytorch")
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
(semantic-add-system-include "/usr/lib/gcc/x86_64-pc-linux-gnu/6.4.1/" 'c++-mode)
;(add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1")


;;visual editing**********************************************************************************************************
;;set transparency********************************************
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(95 . 85))
(add-to-list 'default-frame-alist '(alpha . (95 . 85)))


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(mode-line ((t (:background "#004276" :foreground "#8cccff" :box nil :height 0.9))))
;;  '(mode-line-inactive ((t (:background "#666666" :foreground "#f9f9f9" :box nil :height 0.9))))
;;  '(sml/global ((t (:foreground "gray50" :inverse-video nil :height 0.9 :width normal)))))


;;latex setup***********************************************************************************
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)
(require 'tex)
(require 'preview)
(TeX-global-PDF-mode t)
(require 'company-auctex)
(company-auctex-init)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(defun turn-on-outline-minor-mode ()
  ""
  (outline-minor-mode 1))

;; (defun activate-preview-mode ()
;;   (load "preview-latex.el" nil t t))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
;; (add-hook 'LaTeX-mode-hook 'activate-preview-mode)
;; (add-hook 'laTeX-mode-hook 'activate-preview-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(setq outline-minor-mode-prefix "\C-c \C-o")


;;lua setup************************************************************************
(require 'company-lua)
(add-to-list 'company-backends 'company-lua)


;;magit******************************************************************
(global-set-key (kbd "C-x g") 'magit-status)


;;projectile mode********************************************************
(projectile-mode)


;;ibuffer****************************************************************
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;hide/show
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
	      (hs-toggle-hiding)
	    (error t))
	  (hs-show-all))
    (toggle-selective-display column)))

(load-library "hideshow")
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)

(defun add-hs-hook (mode-list)
  "MODE-LIST:  list of modes that needs 'hs-minor-mode' hook."
  (dolist (mode mode-list)
    (add-hook mode 'hs-minor-mode)))

(add-hs-hook (list 'python-mode-hook 'lisp-mode-hook))

;;other stuff************************************************************
(defun copy-current-file-name ()
  "Copy the current buffers filename to the kill ring."
  (interactive)
  (kill-new (file-name-nondirectory (buffer-file-name (window-buffer (minibuffer-selected-window))))))

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


;;alert mode************************************************************
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(use-package dashboard
  :ensure t
  :config
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
  :bind ("C-c s" . switch-web-js2))

(use-package web-narrow-mode
  :hook 'web-mode)

(use-package js2-mode
  :mode "\\.js\\'"
  :bind ("C-c s" . switch-web-js2))
    
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))    

(use-package org-brain
  :init
  (setq org-brain-path "~/Documents/org/brain")
  :bind ("C-c v" . org-brain-visualize)
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 50)
  (defun org-brain-insert-resource-icon (link)
    "Insert an icon, based on content of org-mode LINK."
    (insert (format "%s "
                    (cond ((string-prefix-p "http" link)
                           (cond ((string-match "wikipedia\\.org" link)
                                  (all-the-icons-faicon "wikipedia-w"))
				 ((string-match "github\\.com" link)
                                  (all-the-icons-octicon "mark-github"))
				 ((string-match "vimeo\\.com" link)
                                  (all-the-icons-faicon "vimeo"))
				 ((string-match "youtube\\.com" link)
                                  (all-the-icons-faicon "youtube"))
				 (t
                                  (all-the-icons-faicon "globe"))))
                          ((string-prefix-p "brain:" link)
                           (all-the-icons-fileicon "brain"))
                          (t
                           (all-the-icons-icon-for-file link))))))
  (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)

  (defface aa2u-face '((t . nil))
    "Face for aa2u box drawing characters")
  (advice-add #'aa2u-1c :filter-return
              (lambda (str) (propertize str 'face 'aa2u-face)))
  (defun aa2u-org-brain-buffer ()
    (let ((inhibit-read-only t))
      (make-local-variable 'face-remapping-alist)
      (add-to-list 'face-remapping-alist
                   '(aa2u-face . org-brain-wires))
      (ignore-errors (aa2u (point-min) (point-max)))))
  (with-eval-after-load 'org-brain
    (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer)))


(use-package plantuml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path "~/.emacs.d/customFiles/plantuml.jar")
  (setq org-plantuml-jar-path "~/.emacs.d/customFiles/plantuml.jar"))
  ;; (setq plantuml-exec-mode "jar")
  ;; (plantuml-set-exec-mode "jar"))

;;Docker
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;;arxiv mode
(require 'arxiv-mode)

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
