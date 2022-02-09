;;; init.el -*- lexical-binding: t -*-

;;; Commentary:

;; init sequence to launch my emacs

;;; Code:

;; quick config
(defun config/init-file-other-window()
  "Open init.el buffer in other window"
  (interactive)
  (find-file-other-window
   (expand-file-name "init.el" user-emacs-directory)))

;; tool
(defmacro locate-user-file-turename  (rpath)
  "Path translate"
  `(expand-file-name ,rpath user-emacs-directory))

;; to for add load-path recursion like -r
(unless (boundp 'add-subdirs-to-load-path)
  (defun add-subdirs-to-load-path(dir)
    "Recursive add directories to `load-path`."
    (let ((default-directory (file-name-as-directory dir)))
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))

;; stop emacs automatically editing .emacs
(setq custom-file (locate-user-file-turename  "var/custom.el"))

;; version check
(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))

;; encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(let (;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; Emacs配置文件内容写到下面.

  ;; very basic default

  ;; -1 for disable
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)

  (setq
   make-backup-files nil         ; no file~
   auto-save-default nil         ; no #file#
   auto-save-list-file-prefix nil; no auto-save-list dir
   ;; because I only keep one emacs process on one work dir
   create-lockfiles nil          ; no .#file (for file lock
   inhibit-startup-screen t
   )

  ;; for macOS
  ;; fresh shell env once for all
  (mapc
   (lambda (path)
     (add-to-list 'load-path path))
   (list ;; path need to load here
    (locate-user-file-turename  "etc/extend/exec-path-from-shell")
    (locate-user-file-turename  "etc/extend/cache-path-from-shell")
    ))
  (require 'cache-path-from-shell)

  ;; load init lib
  (add-subdirs-to-load-path
   (locate-user-file-turename  "etc/config"))

  ;; load extern lib
  ;; sometime you need compile mannualy
  ;; In Dired buffers M-x `dired-do-byte-compile'
  ;; or excute `make'
  (mapc
   (lambda (path)
     (add-to-list 'load-path path))
   (list ;; path need to load here
    (locate-user-file-turename "etc/extend/reveal-in-osx-finder")))

  (require 'init-pkg)        ;pkg management
  (require 'init-defmacro)   ;my defmacro
  (require 'init-bas)        ;basic config
  (require 'init-editor)     ;basic editor feature

  ;; UI setting
  (require 'init-nano)
  (require 'init-fonts)
  ;; (require 'init-layout)

  (require 'init-company-search)
  (require 'init-icon)       ;icon and emoj

  ;; Programming setting
  (require 'init-denv)       ;basic tool env

  ;; Noting sys
  (require 'init-org)        ;basic org-mode
  (require 'init-roam)
  (require 'init-bujo)
  (require 'init-timetask)

  (require 'init-evil)

  ;; final
  ;;(require 'init-patch)

  ;; config tool fun

  (defun config/straight-clone-and-build-my-pkg()
    "Dump Emacs."
    (interactive)
    (let ((buf "*my pkg install process*"))
	  (make-process
	   :name "pkg-install"
	   :buffer buf
	   :command
       ;; emacs --batch -q -l /Users/ingtshan/.emacs.d/pkg-install.el
	   (list "emacs" "--batch" "-q" "-l"
             (expand-file-name "pkg-install.el" user-emacs-directory))
       )
	  (display-buffer buf)))

  )

;;; init.el ends here
