;;; init.el -*- lexical-binding: t -*-

;;; Commentary:

;; init sequence to launch my emacs

;;; Code:

(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))

;; -1 for disable
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; basic default
(setq
 make-backup-files nil         ; no file~
 auto-save-default nil         ; no #file#
 auto-save-list-file-prefix nil; no auto-save-list dir
 ;; because I only keep one emacs process on one work dir
 create-lockfiles nil)         ; no .#file (for file lock)

;; encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; to for add load-path recursion like -r
(unless (boundp 'add-subdirs-to-load-path)
  (defun add-subdirs-to-load-path(dir)
    "Recursive add directories to `load-path`."
    (let ((default-directory (file-name-as-directory dir)))
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))

;; stop emacs automatically editing .emacs
(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

;; load init lib
(add-subdirs-to-load-path
   (expand-file-name "etc/config" user-emacs-directory))

;;; init.el ends here
