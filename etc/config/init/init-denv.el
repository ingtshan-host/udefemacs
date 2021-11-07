;;; init-denv.el -*- lexical-binding: t; -*-

;;; Commentary:

;; my development tool environment

;;; Code:

(leaf magit)

(leaf evil-magit
  :after magit evil
  :init
  (setq evil-magit-state 'normal)
  :config
  (evil-magit-init))

(provide 'init-denv)
;;; init-denv.el ends here
