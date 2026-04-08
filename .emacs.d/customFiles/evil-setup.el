;;; evil-setup.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; The evil and related setup

;;; Code:
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
  (evil-global-set-key 'motion (kbd "C-S-o") 'evil-jump-forward)

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
  :defer 2
  :after evil
  :straight (evil-textobj-tree-sitter :type git :host github :repo "meain/evil-textobj-tree-sitter"
                                      :files (:defaults "queries" "treesit-queries"))
  :config
  (setf (alist-get 'csharp-mode evil-textobj-tree-sitter-major-mode-language-alist) "c-sharp"
        (alist-get 'csharp-ts-mode evil-textobj-tree-sitter-major-mode-language-alist) "c-sharp")

  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  ;; (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))

  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "comment.inner"))

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

(use-package evil-numbers
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "C-c +") #'evil-numbers/inc-at-pt)
  (evil-define-key 'normal 'global (kbd "C-c -") #'evil-numbers/dec-at-pt)
  (evil-define-key 'normal 'global (kbd "<kp-add>") #'evil-numbers/inc-at-pt)
  (evil-define-key 'normal 'global (kbd "<kp-substract>") #'evil-numbers/dec-at-pt))



(provide 'evil-setup)

;;; evil-setup.el ends here
