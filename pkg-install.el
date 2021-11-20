;;; pkg-install.el -*- lexical-binding: t; -*-

;;; Commentary:

;; install pkg

;;; Code:

(setq pkg--do-install t)

(load-file (expand-file-name "pkg-list.el" user-emacs-directory))

;; (provide 'pkg-install)
;;; pkg-install.el ends here
