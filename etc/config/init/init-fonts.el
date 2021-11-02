;;; init-fonts.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(leaf-unit set-zh-fonts
  (let ((en-font "Fira Code")
        (zh-font "Glow Sans SC")
        (rescale 1.1))
    ;; alread done in nano
    ;; (set-face-attribute
    ;;  'default nil
    ;;  :font (font-spec  :family en-font
    ;;                    :weight 'normal
    ;;                    :slant 'normal
    ;;                    :size en-size
    ;;                    ))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family zh-font
                  :weight 'normal
                  :slant 'normal
                  ;; :size zh-size 在这里设置会影响缩放
                  )))
    (setq face-font-rescale-alist
          (list `(,en-font . 1) `(,zh-font . ,rescale)))))

(provide 'init-fonts)
;;; init-fonts.el ends here
