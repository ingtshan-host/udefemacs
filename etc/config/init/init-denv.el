;;; init-denv.el -*- lexical-binding: t; -*-

;;; Commentary:

;; my development tool environment

;;; Code:

;; vc

(leaf magit)

(leaf evil-magit
  :after magit evil
  :init
  (setq evil-magit-state 'normal)
  :config
  (evil-magit-init))

;; projectile
(leaf projectile
  :init
  (projectile-mode +1)
  :custom
  (projectile-indexing-method          . 'hybrid)
  (projectile-require-project-root     . 'prompt)
  :config
  ;; rely on ripgrep (you should install in your os)
  (defun denv/projectile-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep-command
           "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS")
          (project (projectile-acquire-root)))
      (consult-ripgrep project))))

(leaf consult-projectile
  :after consult projectile
  :straight (consult-projectile
             :type git :host gitlab
             :repo "OlMon/consult-projectile"
             :branch "master"))

(leaf-unit read-only-protect
  ;; Define a read-only directory class
  (dir-locals-set-class-variables
   'read-only
   '((nil . ((buffer-read-only . t)))))

  ;; Associate directories with the read-only class
  (dolist (dir-true-name
           (list
            ;; add protect dir here
            (expand-file-name "straight/repos" user-emacs-directory)
            ))    
    (dir-locals-set-directory-class
     dir-true-name 'read-only)))

;; shell
(leaf vterm
  :require cl-lib
  :config
  (setq vterm-shell "zsh")
  (add-hook 'vterm-set-title-functions 'denv/vterm--rename-buffer-as-title)

  ;;; Functions

  (defun denv/vterm-cd (dir)
    "cd to DIR."
    (when (derived-mode-p 'vterm-mode)
      (vterm-send-key "u" nil nil t)
      (execute-kbd-macro (format "cd %s
" dir))))

  (defun denv/vterm-current-dir-other-window()
    "use one vterm process and cd to current dir"
    (interactive)
    (let ((dir default-directory))
      (vterm-other-window)
      (denv/vterm-cd dir)))
  )

(provide 'init-denv)
;;; init-denv.el ends here
