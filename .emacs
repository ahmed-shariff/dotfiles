;;; package -- Summary

;;; Commentary:

;;; Code:

;; -*- emacs-lisp -*-
(require 'package)

;(unless package-archive-contents    ;; Refresh the packages descriptions
;  (package-refresh-contents))
(setq package-list '(;anaconda-mode
 			  company
 			  company-anaconda
 			  company-c-headers
 			  company-quickhelp
 			  dash
 			  epl
 			  f 			  
 			  let-alist
 			  macrostep
 			  pos-tip
 			  python-mode
 			  pythonic
 			  rich-minority
 			  s
 			  seq
			  flycheck
 			  slim-mode
 			  slime
 			  slime-company
 			  org
			  vlf
			  sr-speedbar
			  micgoline
			  yasnippet
			  elpy
			  ))     ;; List of packages to load



;;mepla setup****************************************************
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") '("org" . "http://orgmode.org/elpa/"));'("elpy" . "http://jorgenschaefer.github.io/packages/"))
;	     '("melpa" . "http://melpa.org/packages/")
;	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'load-path "~/.emacs.d/customFiles")
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/"))


(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(dolist (package package-list)
  (unless (package-installed-p package)  ;; Make sure the Org package is
    (package-install package)))           ;; installed, install it if not

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
 '(package-selected-packages
   (quote
    (magit company-lua stumpwm-mode all-the-icons-dired hledger-mode vlf elpy company-auctex auctex pdf-tools yasnippet company-jedi jedi sr-speedbar latex-preview-pane exec-path-from-shell smart-mode-line-powerline-theme slime-company slim-mode python-mode flycheck company-quickhelp company-c-headers company-anaconda)))
 '(prolog-system (quote swi))
 '(sml/mode-width 15)
 '(sml/shorten-modes t)
 '(sml/theme (quote dark)))

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



;;allout
(allout-mode)

;;syntax highlight
(global-font-lock-mode 1)

;;add proper word wrapping
(global-visual-line-mode t)

;;enable ido mode
(require 'ido)
(ido-mode t)


;;pdf
;(pdf-tools-install)

;;delete-selection-mode
(delete-selection-mode t)
(require 'vlf-setup)

;;speedbar settings
 (require 'sr-speedbar)
 (global-set-key (kbd "M-s M-s") 'sr-speedbar-toggle)

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
					;(require 'flyspell)

;;ispell
(setq ispell-program-name "aspell")
(require 'ispell)
(require 'flyspell)

(require 'micgoline)
(setq powerline-default-separator 'chamfer)

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
(elpy-enable)
(add-hook 'python-mode-hook 'linum-mode)
;(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "python" python-shell-interpreter-args "-i")

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
(semantic-add-system-include "/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/" 'c++-mode)
;(add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1")


;;visual editing**********************************************************************************************************
;;set transparency********************************************
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(90 . 60))
(add-to-list 'default-frame-alist '(alpha . (90 . 60)))


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
  (utline-minor-mode 1))

(defun activate-preview-mode ()
  (load "preview-latex.el" nil t t))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'activate-preview-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(setq outline-minor-mode-prefix "\C-c \C-o")


;;lua setup************************************************************************
(require 'company-lua)
(add-to-list 'company-backends 'company-lua)


;;magit******************************************************************
(global-set-key (kbd "C-x g") 'magit-status)


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


;;code to run at the end!************************************************

(defun company-mode/backend-with-yas (backend)
  ""
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;;yasnippets company conflict resolution
;(provide .emacs)
;;; .emacs ends here
