;;; init-defmacro.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;; 函数执行期间 中文输入
(defmacro sis-protect-other (fun &rest argument)
  `(let ((r nil))
     (sis-global-respect-mode -1)
     (sis-set-other)
     (unwind-protect
         (setq r (funcall ,fun ,@argument)) ;; progn here
       (sis-set-english)
       (sis-global-respect-mode 1))
     r))

;;; Code:

(provide 'init-defmacro)
;;; init-defmacro.el ends here
