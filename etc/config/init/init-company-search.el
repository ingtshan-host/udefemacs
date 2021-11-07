;;; init-company-search.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

;; fix minibuffer issues
(leaf-unit fix-minibuffer
  
  (defun fm/stop-using-minibuffer ()
    "kill the minibuffer"
    (when (and (>= (recursion-depth) 1)
	           (active-minibuffer-window))
      (abort-recursive-edit)))
  ;; kill minibuffer while unfocus
  (add-hook 'mouse-leave-buffer-hook 'fm/stop-using-minibuffer)
  )

;; minibuffer improve
(leaf vertico
  :leaf-defer 0
  :bind (("C-c v" . vertico-repeat)
	     (:vertico-map
          ("M-RET" . minibuffer-force-complete-and-exit)))
  :custom
  (vertico-cycle . t)
  (vertico-resize . nil)
  :init (vertico-mode))

;; fonst of minibuffer
(custom-set-faces
 '(Info-quoted ((t (:family "Sarasa Mono SC Nerd"))))
 '(minibuffer-prompt ((t (:family "Sarasa Mono SC Nerd")))))

;; take over minibuffer
;; postframe setting
(leaf posframe)

;; posframes for vertico
(defface vertico-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the vertico-posframe's border."
  :group 'vertico-posframe)

(leaf vertico-posframe
  :ensure nil
  :straight (vertico-posframe :type git :host github
                              :repo "tumashu/vertico-posframe")
  :config
  (vertico-posframe-mode 1)
  :custom
  (vertico-posframe-poshandler
   .
   #'posframe-poshandler-p0.5p0-to-f0.5p1
   ;;#'posframe-poshandler-point-top-left-corner
   )
  (vertico-posframe-font .
                         "Sarasa Mono SC Nerd 15"
                         ;;"Inconsolata"
                         )
  (vertico-posframe-parameters
   .
   ;;nil
   ;;背景RGB (51,62,80)#BDC0C5，前景(189,192,197)#333E50
   '((foreground-color . "#BDC0C5")
     (background-color . "#333E50"))
   ))

(leaf which-key
  :config
  (which-key-mode))

(leaf which-key-posframe
  :custom
  (which-key-posframe-poshandler
   .
   ;;#'posframe-poshandler-frame-center
   #'posframe-poshandler-p0.5p0-to-f0.5p1
   )
  (which-key-posframe-parameters
   .
   '((background-color . "#333E50")))
  :config
  (which-key-posframe-mode))

;; Use the `orderless' completion style.
(leaf orderless
  :custom
  (completion-styles . '(orderless)))

;; detailed minibuffer
(leaf marginalia
  :bind (("M-A" . marginalia-cycle)
         (:minibuffer-local-map
          ("M-A" . marginalia-cycle)))
  :init (marginalia-mode))

;; asynchronous fuzzy finder 
(leaf affe
  :after (consult)
  :custom
  (affe-regexp-function . 'orderless-pattern-compiler)
  (affe-highlight-function . 'orderless--highlight)
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

;; powerful tool improving all kinds of minibuffer
;; you should learn basic usage first
;; https://github.com/minad/consult
(leaf consult
  :leaf-defer 0
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c H" . consult-history)
         ("C-c m" . consult-mode-command)
         ([remap bookmark-jump] . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ([remap recentf-open-files] . consult-recent-file)
         ;; C-x bindings (ctl-x-map)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ([remap abbrev-prefix-mark] . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ([remap yank-pop] . consult-yank-pop)
         ([remap apropos-command] . consult-apropos)
         ;; M-g bindings (goto-map)
         ([remap compile-goto-error] . consult-compile-error)
         ([remap flymake-show-diagnostics-buffer] . consult-flymake)
         ([remap goto-line] . consult-goto-line)             ;; orig. goto-line
         ([remap imenu] . consult-imenu)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("C-c ," . consult-imenu-multi)
	     ("H-g" . consult-imenu-multi)
         ("C-."   . xref-find-references)
         ;; M-s bindings (search-map)
         ([remap project-find-file] . consult-find)
         ([remap locate] . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ([remap ripgrep-regexp] . consult-ripgrep)
         ([remap isearch-forward] . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         (:isearch-mode-map
          ([remap isearch-edit-string] . consult-isearch)
          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
          ("M-s L" . consult-line-multi)))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  ;; :hook (completion-list-mode-hook . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; customize
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;; provides a sort of right-click contextual menu for Emacs
(leaf embark
  :preface
  (defun embark-dwim-other-window (&optional arg)
    (interactive "P")
    (split-window-right)
    (other-window 1)
    (embark-dwim arg))
  :bind
  (("C-c e" . embark-act)
   ("C-:" . embark-dwim-other-window)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   (:minibuffer-local-map
    ("C-c C-o" . embark-expoort)
    ("C-c C-c" . embark-collect-snapshot)))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(leaf embark-consult
  :after (embark consult)
  :leaf-defer nil
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;; use it where company does not have a default backend
(leaf corfu
  :hook
  (eshell-mode-hook . corfu-mode)
  (shell-mode-hook . corfu-mode)
  :bind (:corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous))
  :custom
  (corfu-cycle . t)
  (corfu-auto . nil)
  (corfu-echo-documentation . t))

;; `company'
(leaf company
  :diminish company-mode
  :hook
  ((after-init-hook . global-company-mode))
  :custom
  ((company-tooltip-align-annotations . t)
   (company-tooltip-limit . 12)
   (company-idle-delay . 0)
   (company-echo-delay . 0)
   (company-minimum-prefix-length . 3)
   (company-require-match . nil)
   (company-dabbrev-ignore-case . nil)
   (company-show-numbers . t)
   (company-dabbrev-downcase . nil)
   (company-global-modes
    . '(not
        erc-mode message-mode help-mode
        gud-mode eshell-mode shell-mode))
   (company-backends
    . '(company-capf company-files))
   (company-frontends
    . '(company-pseudo-tooltip-frontend
        company-echo-metadata-frontend)))
  :bind
  (("M-/" . company-yasnippet)
   (:company-active-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next)
    ("<tab>" . company-complete-common-or-cycle)
    ("<C-return>". company-complete-selection))
   (:company-search-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next))))

(leaf company-quickhelp
  :hook (global-company-mode-hook . company-quickhelp-mode)
  :custom ((company-quickhelp-delay . 0.5)))

;; (leaf consult-company)

;; A consulting-read interface for yasnippet.
;; (leaf consult-yasnippet)



(provide 'init-company-search)
;;; init-company-search.el ends here
