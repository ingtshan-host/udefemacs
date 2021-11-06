;;; init-pkg.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; use straight.el and leaf.el as package manager

;;; Code:

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;         ("http" . "127.0.0.1:8889")
;;         ("https" . "127.0.0.1:8889")))

;; ;; setup package archives
(require 'package)
(eval-and-compile
  
  ;; package-archives

  ;; Ensure that, if we do need package.el
  ;; it is configured correctly. You really
  ;; shouldn't be using it
  ;; but it may be convenient for quick package testing.
  (setq package--init-file-ensured t
        package-enable-at-startup nil
        ;; package-user-dir doom-elpa-dir
        ;; package-gnupghome-dir (expand-file-name "gpg" doom-elpa-dir)
        ;; I omit Marmalade because its packages are manually submitted rather
        ;; than pulled, so packages are often out of date with upstream.
        ;; package-archives
        ;; (let ((proto "https"))
        ;;   `(("gnu"   . ,(concat proto "://elpa.gnu.org/packages/"))
        ;;     ("melpa" . ,(concat proto "://melpa.org/packages/"))
        ;;     ("org"   . ,(concat proto "://orgmode.org/elpa/"))))
        ;; emacs-china
        package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                           ("melpa" . "http://elpa.zilongshanren.com/melpa/"))
        )
  ;; (add-to-list 'package-archives
  ;;              `("elpa-mirror" .
  ;;                ,(expand-file-name "elpa-mirror/packages"
  ;;                                   user-emacs-directory)))
  
  ;; initialize packages
  ;; To avoid warnings in 27
  (unless (bound-and-true-p package--initialized)
    (package-initialize))

  ;; (when (not package-archive-contents)
  ;;   (package-refresh-contents))

  ;; initialize straight
  (defvar bootstrap-version)
  (setq straight-vc-git-default-clone-depth 1
	    straight-check-for-modifications '(find-when-checking)
	    straight-use-package-by-default t
	    straight-recipes-gnu-elpa-use-mirror t
        straight-recipes-emacsmirror-use-mirror t)

  ;; use github.com mirror from .cnpmjs.org
  (setq straight-vc-git-default-protocol 'https)
  (advice-add 'straight-vc-git--encode-url :around #'noalias-set-github-mirror)
  (defun noalias-set-github-mirror (oldfunc &rest args)
    (let ((url (apply oldfunc args)))
      (replace-regexp-in-string (rx (group "github.com"))
                                "github.com.cnpmjs.org" url nil nil 1)))
  
  ;; loading bootstrap file
  (let ((bootstrap-file
	     (expand-file-name
	      "straight/repos/straight.el/bootstrap.el"
	      user-emacs-directory))
	    (bootstrap-version 5))
    
    (unless (file-exists-p bootstrap-file)
      
      (with-current-buffer
          (url-retrieve-synchronously
           ;; "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	       "https://ghproxy.com/https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"  
           'silent 'inhibit-cookies)
	    
	    (save-excursion
          (goto-char (point-min))
          (while (search-forward "https://raw.githubusercontent.com" nil t)
            (replace-match "https://ghproxy.com/https://raw.githubusercontent.com")))
	    
        (goto-char (point-max))
	    (eval-print-last-sexp)))
    
    (load bootstrap-file nil 'nomessage)

    (require 'straight)

    (defun noalias-set-github-mirror (oldfunc &rest args)
      (let ((url (apply oldfunc args)))
        (replace-regexp-in-string (rx (group "github.com"))
                                  "github.com.cnpmjs.org" url nil nil 1)))
    (setq straight-vc-git-default-protocol 'https)

    (defvar my-core-package-sources
      '((org-elpa :local-repo nil)
        (melpa
         :type git :host github
         :repo "melpa/melpa"
         :no-build t)
        (gnu-elpa-mirror
         :type git :host github
         :repo "emacs-straight/gnu-elpa-mirror"
         :no-build t)
        (emacsmirror-mirror
         :type git :host github
         :repo "emacs-straight/emacsmirror-mirror"
         :no-build t))
      "A list of recipes for straight's recipe repos.")

    ;; (straight--reset-caches)
    (mapc #'straight-use-recipes my-core-package-sources)
    
    ;; make sure leaf is install
    (unless (package-installed-p 'leaf)
      ;; install by straight
	  (straight-use-package 'leaf)
	  (straight-use-package 'leaf-keywords)
      (straight-use-package 'diminish))
    (leaf-keywords-init)))

;; leaf.el setup
;; (leaf leaf-keywords
;; :config
;; optional packages if you want to use :hydra, :el-get, :blackout,,,
;; (leaf hydra :ensure t)
;; (leaf el-get :ensure t)
;; (leaf blackout :ensure t)
;; (leaf diminish :ensure t)

;; initialize leaf-keywords.el
;; (leaf-keywords-init))
;; now you can use leaf!

;; improve leaf

;; always ensure
(leaf leaf
  :ensure nil
  ;; feel free to change default here
  ;; :ensure nil to disable package.el
  :custom ((leaf-defaults . '(:ensure nil :straight t)))
  :bind (("H-f l" . leaf-find-with-unit))
  :config
  (defcustom leaf-find-unit-regexp ".*([[:space:]]*leaf-unit[[:space:]]+\\(%s\\)"
    "The regexp used by `leaf-find-with-unir' to search for a leaf block.
Note it must contain a `%s' at the place where `format'
should insert the leaf name."
    :type 'regexp
    :group 'leaf)
  (require 'find-func)
  (add-to-list
   'find-function-regexp-alist
   '(leaf-unit . leaf-find-unit-regexp))

  (defmacro leaf-unit (base &rest body)
    "do the sexp in body with leaf-bolck name base-unit
Generate code like (leaf base-name-unit :config body)"
    (declare (indent 1))
    (let ((base (intern (format "+unit-%s" `,base))))
      `(prog1 ',base
         (leaf-handler-leaf-path ,base)
         (leaf-handler-leaf-protect ,base ,@body))))

  (defun leaf-find-with-unit (truename)
    "Find the leaf block (and self make -unit) of NAME."
    (interactive
     (let ((candidates (delete-dups (mapcar #'car leaf--paths))))
       (if (not candidates)
           (error "Leaf has no definition informations")
         (list (completing-read "Find leaf: " (delete-dups (mapcar #'car leaf--paths)))))))
    (require 'find-func)
    (let* ((name (intern truename))
           (paths (mapcan (lambda (elm) (when (eq name (car elm)) (list (cdr elm)))) leaf--paths))
           (path (if (= (length paths) 1) (car paths) (completing-read "Select one: " paths)))
           (location nil))
      (setq location
            (if (string-match-p "^+unit-" truename)
                (find-function-search-for-symbol
                 (intern (substring truename +6))
                 'leaf-unit path)
              (find-function-search-for-symbol name 'leaf path)))
      (when location
        (prog1 (pop-to-buffer (car location))
          (when (cdr location)
            (goto-char (cdr location)))
          (run-hooks 'find-function-after-hook)))))
  )
;; dot operator means add iterm to list

;; Interactive side-bar feature for init.el using leaf.el.
;; usage M-x leaf-tree-mode
(leaf leaf-tree)

;; Convert from a plain Elisp to an expression using a leaf.
;; usage (leaf-convert elisp-code)
(leaf leaf-convert
  :config
  ;; improve leaf-convert
  (defun insert-leaf-convert-last-sexp ()
    "leaf-convert last sexp then inserat after"
    (interactive)
    (let ((sexp-str (thing-at-point 'sexp 'no-properties)))
      (insert "\n;;=>leaf-convert")
      (insert
       
       (replace-regexp-in-string
        (regexp-quote "\n(\n") "(" ;; no single (

        (replace-regexp-in-string
         (regexp-quote ":") "\n:" ;; break at :
         
         (replace-regexp-in-string
          (regexp-quote "(") "\n(" ;; break at (
          (format "%s"
                  (eval
                   (car
                    (read-from-string
                     (concat "(leaf-convert "
                             sexp-str
                             " )")))))
          nil 'literal)
         nil 'literal)
        nil 'literal)))))

;; manage pkg profile path
;; put configuration files in no-littering-etc-directory
;; (defaulting to "etc/" under user-emacs-directory, thus usually "~/.emacs.d/etc/")
;; persistent data files in no-littering-var-directory
;; (defaulting to "var/" under user-emacs-directory, thus usually "~/.emacs.d/var/")
(leaf no-littering
  :leaf-defer nil
  :custom
  ;; store these files in the var directory
  (auto-save-file-name-transforms
   . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


(leaf-unit my-pkg-list

  (defun pkg/my-pkg-list-other-window ()
    "Other window of pkg-list.el with match current buffer"
    (interactive)
    (let ((name (file-name-base (buffer-name))))
      (find-file-other-window
       (expand-file-name "pkg-list.el" user-emacs-directory))
      (goto-char (point-min))
      (re-search-forward (concat "[[:space:]]*;;[[:space:]]*" name))
      (match-string-no-properties 0)
      ))
  )

(provide 'init-pkg)
;;; init-pkg.el ends here
