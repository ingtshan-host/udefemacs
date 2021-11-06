;;; init.el -*- lexical-binding: t -*-

;;; Commentary:

;; init sequence to launch my emacs

;;; Code:

;; to for add load-path recursion like -r
(unless (boundp 'add-subdirs-to-load-path)
  (defun add-subdirs-to-load-path(dir)
    "Recursive add directories to `load-path`."
    (let ((default-directory (file-name-as-directory dir)))
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))

;; quick config
(defun config/init-file-other-window()
  "Open init.el buffer in other window"
  (interactive)
  (find-file-other-window
   (expand-file-name "init.el" user-emacs-directory)))

;; quick install
(defun config/straight-clone-and-build-my-pkg()
  "Dump Emacs."
  (interactive)
  (let ((buf "*my pkg install process*"))
	(make-process
	 :name "pkg-install"
	 :buffer buf
	 :command
	 (list "emacs" "--batch" "-q" "-l"
		   (expand-file-name "pkg-list.el" user-emacs-directory )
           "--eval" "`(progn (require 'pkg-list) (install-my-pkg))`"))
	(display-buffer buf)))

;; version check
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

;; stop emacs automatically editing .emacs
(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

;; load init lib
(add-subdirs-to-load-path
 (expand-file-name "etc/config" user-emacs-directory))

(require 'init-pkg)        ;pkg management
(require 'init-bas)        ;basic config
(require 'init-editor)     ;basic editor feature
(require 'init-org)        ;basic org-mode
;; UI setting
(require 'init-nano)
(require 'init-fonts)

(require 'init-company-search)

;; Noting sys
(require 'init-roam)
(require 'init-bujo)

(require 'init-evil)

;; final
;; (require 'init-patch)
;;; init.el ends here
