;;; init-pkg.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; use straight.el and leaf.el as package manager

;;; Code:

;; optional proxy setup
(setq url-proxy-services
      '(("http" . "127.0.0.1:8889")
        ("https" . "127.0.0.1:8889")))
;; setup package archives
(require 'package)
(eval-and-compile
  ;; package-archives
  (customize-set-variable
   'package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu"   . "https://elpa.gnu.org/packages/")
	 ("org"   . "http://orgmode.org/elpa/")
	 ("melpa" . "https://melpa.org/packages/")))

  ;; (add-to-list 'package-archives
  ;;              `("elpa-mirror" .
  ;;                ,(expand-file-name "elpa-mirror/packages"
  ;;                                   user-emacs-directory)))
  
  ;; initialize packages
  (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
    (package-initialize))

  (when (not package-archive-contents)
    (package-refresh-contents))

  ;; initialize straight
  (defvar bootstrap-version)
  (setq straight-vc-git-default-clone-depth 1
	    straight-check-for-modifications '(find-when-checking)
	    straight-use-package-by-default t
	    straight-recipes-gnu-elpa-use-mirror t)
  
  ;; loading bootstrap file
  (let ((bootstrap-file
	     (expand-file-name
	      "straight/repos/straight.el/bootstrap.el"
	      user-emacs-directory))
	    (bootstrap-version 5))
    
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	    (goto-char (point-max))
	    (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)
    
    ;; make sure leaf is install
    (unless (package-installed-p 'leaf)
      (cond ((file-exists-p bootstrap-file) ;; install by straight
	         (straight-use-package 'leaf)
	         (straight-use-package 'leaf-keywords))
	        (t ;; install tradition way
	         (package-install 'leaf)
	         (package-install 'leaf-keywords))))))

;; leaf.el setup
(leaf leaf-keywords
  :config
  ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
  ;; (leaf hydra :ensure t)
  ;; (leaf el-get :ensure t)
  ;; (leaf blackout :ensure t)
  (leaf diminish :ensure t)

  ;; initialize leaf-keywords.el
  (leaf-keywords-init))
;; now you can use leaf!

;; improve leaf

;; always ensure
(leaf leaf
  ;; feel free to change default here
  :custom ((leaf-defaults . '(:ensure t :straight t)))
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
      nil 'literal))))
      )

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

(provide 'init-pkg)
;;; init-pkg.el ends here
