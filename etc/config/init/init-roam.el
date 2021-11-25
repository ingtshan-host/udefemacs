;;; init-roam.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:
;;; load-org-roam.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;  roam

(leaf org-roam
  :diminish org-roam-mode
  
  :hook ((after-init-hook . org-roam-db-autosync-enable))
  
  :bind (("C-c rt" . org-roam-buffer-toggle)
         ("C-c rn" . org-roam-node-find)
         ("C-c ri" . org-roam-node-insert)
         ("C-c rc" . org-roam-capture)
         ("C-c rj" . org-roam-dailies-goto-today))
  
  :preface
  (setq
   org-roam-directory (file-truename "~/org/roam-v2")
   ;;(expand-file-name "roam-v2" =org-dir)
   org-roam-v2-ack t
   ;;fix some problem
   ;;org-roam-db-update-on-save nil
   )
  
  :config 
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  
  (unless (file-exists-p (expand-file-name "daily" org-roam-directory))
    (make-directory
     (expand-file-name "daily" org-roam-directory)))
  )

;; my org-roam search method
;; (require 'consult)
;; rely on ripgrep (you should install in your os)
(defun ns/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command
         "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

(defun ns/consult-ripgrep-files-with-matches (&optional dir initial)
  "Use consult-find style to return matches with \"rg --file-with-matches \". No live preview."
  (interactive "P")
  (let ((consult-find-command
         "rg --ignore-case --type org --files-with-matches . -e ARG OPTS"))
    (consult-find dir initial)))

(defun ns/org-roam-rg-file-search ()
  "Search org-roam directory using consult-find with \"rg --file-with-matches \". No live preview."
  (interactive)
  (ns/consult-ripgrep-files-with-matches org-roam-directory))

(global-set-key (kbd "C-c rr") #'ns/org-roam-rg-search)
(global-set-key (kbd "C-c rf") #'ns/org-roam-rg-file-search)

(provide 'init-roam)
;;; init-roam.el ends here
