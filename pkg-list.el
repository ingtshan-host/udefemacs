;;; pkg-list.el -*- lexical-binding: t; -*-

;;; Commentary:

;; my package list

;;; Code:

(defconst my-pkg-list
  '(
    ;; init-pkg
    ;; straight.el
    (leaf . nil)               ;包管理和配置
    (leaf-keywords . nil)
    (leaf-tree . nil)
    (leaf-convert . nil)
    (diminish . nil)         ;modeline 隐藏
    (no-littering . nil)       ;etc var

    ;; init-bas
    (aggressive-indent . nil)
    (yasnippet . nil)
    (yasnippet-snippets . nil)

    ;; init-editor
    (indent-guide . nil)

    ;; init-org
    (org . nil)    

    ;; init-nano
    (nano-emacs
     .
     (nano-emacs
      :type git :host github
      :repo "rougier/nano-emacs"))

    ;; init-fonts
    (evalign . nil)

    ;; init-company-search
    (vertico . nil)
    (posframe . nil)
    (vertico-posframe
     .
     (vertico-posframe
      :type git :host github
      :repo "tumashu/vertico-posframe"))
    (which-key . nil)
    (which-key-posframe . nil)
    (embark . nil)
    (consult . nil)
    (embark-consult . nil)
    (orderless . nil)
    (marginalia . nil)
    (affe . nil)
    (corfu . nil)
    (company . nil)
    (company-quickhelp . nil)

    ;; init-roam
    (org-roam . nil)

    ;; init-bujo
    
    ;; init-evil
    (general . nil)
    (evil . nil)
    
    ;; init-patch

    
    );; defconst ends here
  "all the third package I need here"
  )

(defun install-my-pkg()
  "install all my pkg"
  (prog1 "All done"
    ;;clone all
    (dolist (pkg my-pkg-list)
      (straight-use-package
       (if (cdr pkg) `(quote ,(cdr pkg)) (car pkg))
       nil t))
    ;; load to path
    ;; load init lib
    (add-subdirs-to-load-path
     (expand-file-name "straight/repos" user-emacs-directory))

    ;; build all
    (dolist (pkg my-pkg-list)
      ;; only clone
      (straight-use-package
       (if (cdr pkg) `(quote ,(cdr pkg)) (car pkg))))

    ;; leaf load
    ;; (dolist (pkg my-pkg-list)    
    ;;   (if (cdr pkg)
    ;;       (eval `(leaf ,(car pkg) :ensure nil :straight ,(cdr pkg)))
    ;;     (eval `(leaf ,(car pkg)))))
    );;manually do here
  )

;; to for add load-path recursion like -r
(unless (boundp 'add-subdirs-to-load-path)
  (defun add-subdirs-to-load-path(dir)
    "Recursive add directories to `load-path`."
    (let ((default-directory (file-name-as-directory dir)))
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))

;; load init lib
(add-subdirs-to-load-path
 (expand-file-name "etc/config" user-emacs-directory))

(require 'init-pkg)

(install-my-pkg)

;;(provide 'pkg-list)
;;; pkg-list.el ends here
