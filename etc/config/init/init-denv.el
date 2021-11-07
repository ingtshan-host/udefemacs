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

(provide 'init-denv)
;;; init-denv.el ends here
