;;; init-timetask.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;; org-agenda

;; update agenda file dynamic

(defun task/project-p ()
  "Return non-nil if current buffer has any todo entry.
  TODO entries marked as done are ignored, meaning the this
  function returns nil if current buffer contains only completed
  tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun task/buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.
  If SEPARATORS is non-nil, it should be a regular expression
  matching text that separates, but is not part of, the substrings.
  If nil it defaults to `split-string-default-separators', normally
  \"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (task/buffer-prop-get name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))

(defun task/buffer-prop-remove (name)
  "Remove a buffer property called NAME."
  (org-with-point-at 1
    (when (re-search-forward
           (concat "\\(^#\\+" name ":.*\n?\\)")
           (point-max) t)
      (replace-match ""))))

(defun task/buffer-tags-get ()
  "Return filetags value in current buffer."
  (task/buffer-prop-get-list "filetags" "[ :]"))

(defun task/buffer-tags-set (&rest tags)
  "Set TAGS in current buffer.

  If filetags value is already set, replace it."
  (if tags
      (task/buffer-prop-set
       "filetags" (concat ":" (string-join tags ":") ":"))
    (task/buffer-prop-remove "filetags")))

(defun task/buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun task/buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in buffer file.
  If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

(defun task/project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             ;; (task/buffer-p)
             ;; Return non-nil if the currently visited buffer is a roam note.
             (and buffer-file-name
                  (string-prefix-p
                   (expand-file-name
                    (file-name-as-directory
                     org-roam-directory))
                   (file-name-directory buffer-file-name))))

    (save-excursion
      (goto-char (point-min))
      (let* ((tags (task/buffer-tags-get))
             (original-tags tags))
        (if (task/project-p)
            (setq tags (cons "project" tags))
          (setq tags (remove "project" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'task/buffer-tags-set tags))))))

(defun task/agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files
        ;; Return a list of note files containing 'project' tag.
        (seq-uniq
         (seq-map
          #'car
          (org-roam-db-query
           [:select
            [nodes:file]
            :from tags
            :left-join nodes
            :on (= tags:node-id nodes:id)
            :where (like tag (quote "%\"project\"%"))])))
        ))

(defun task/agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:
(setq org-agenda-prefix-format
      '((agenda . \" %(task/agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (task/buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))


(leaf-unit timetask

  ;; enable habit
  (with-eval-after-load 'org
	(require 'org-habit)
	(add-to-list 'org-modules 'org-habit t)
    (setq org-habit-show-only-for-today t
	      org-habit-show-done-always-green t
          org-habit-graph-column 55))

  (setq org-todo-keywords
        '((sequence
           ;; A task that needs doing & is ready to do
           " TODO(t)"
           " FIXME(f)"
           " BREAK(b)"
           " NEXT(n)"
           ;; A task that is in progress
           " INPROCESS(i)"
           ;; Something is holding up this task; or it is paused
           " WAITING(w@)"
           ;; An ongoing project that cannot be completed in one step
           " PROJ(P)"
           " NextMile(x)"
           "❤ Love(L)"
           ;; Task successfully completed
           "|"
           " Milestone(X)"
           ;; Task was cancelled, aborted or is no longer applicable
           " CANCELED(C@)"
           " Memo(M)"
           " NOTE(N)"
           " FIXED(F)"
           " DONE(d)"
           " REVIEW(R)")          
          (sequence
           "Gance(1)" ;;初见知识点
           "M deling(2)" ;;泛化理解
           "Ne k(3)";;瓶颈
           "Rep ting(4)";;重复训练
           "Ntting(5)";;知识网，二阶知识
           "|"
           " PP(A)";;运用
           "Recll(a)";;记忆，回忆
           "Knwledge(O)";;知识    
           )
          )) ; Task was completed
  

  ;; hid to :LOGBOOK:
  (setq org-log-into-drawer t)

  ;; update agenda list with roam note
  (add-hook 'find-file-hook #'task/project-update-tag)
  (add-hook 'before-save-hook #'task/project-update-tag)
  (advice-add 'org-agenda :before #'task/agenda-files-update)

  ;; %c   the category of the item, "Diary" for entries from the diary,
  ;; or as given by the CATEGORY keyword or derived from the file name
  ;; %e   the effort required by the item
  ;; %l   the level of the item (insert X space(s) if item is of level X)
  ;; %i   the icon category of the item, see ‘org-agenda-category-icon-alist’
  ;; %T   the last tag of the item (ignore inherited tags, which come first)
  ;; %t   the HH:MM time-of-day specification if one applies to the entry
  ;; %s   Scheduling/Deadline information, a short string
  ;; %b   show breadcrumbs, i.e., the names of the higher levels
  ;; %(expression) Eval EXPRESSION and replace the control string
  ;; by the result

  (setq org-agenda-prefix-format
        '((agenda . " %i %(task/agenda-category 12)%?-12t")
          (todo . " %i %(task/agenda-category 12) ")
          (tags . " %i %(task/agenda-category 12) ")
          (search . " %i %(task/agenda-category 12) ")))

  (setq org-agenda-hide-tags-regexp (regexp-opt '("project")))
  )

(leaf org-agenda :ensure nil :straight nil
  :bind ((org-agenda-mode-map
          ("j" . org-agenda-next-line)
          ("k" . org-agenda-previous-line)
          ("n" . org-roam-node-find))))

(leaf org-super-agenda
  :require org-habit
  :hook(after-init-hook . org-super-agenda-mode)
  :bind((org-super-agenda-header-map
         ("j" . org-agenda-next-line)
         ("k" . org-agenda-previous-line)))
  :config
  (setq
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-include-deadlines t
   org-agenda-include-diary nil
   org-agenda-block-separator nil
   org-agenda-compact-blocks t
   org-agenda-start-with-log-mode t
   )

  (setq org-agenda-custom-commands
        '(("u" "udef Agenda"
           ((agenda
             "" ((org-agenda-time-leading-zero t)
                 (org-agenda-timegrid-use-ampm nil)
                 (org-agenda-skip-timestamp-if-done t)
                 (org-agenda-skip-deadline-if-done t)
                 (org-agenda-start-day "+0d")
                 (org-agenda-span 1)
                 (org-agenda-overriding-header " Calendar")
                 (org-agenda-repeating-timestamp-show-all nil)
                 (org-agenda-remove-tags t)
                 ;;(org-habit-show-habits nil)
                 (org-agenda-prefix-format
                  "  %i %(task/agenda-category 12)%?-8t%s")
                 ;; (org-agenda-prefix-format "   %i %?-2 t%s")
                 ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                 ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                 ;; (org-agenda-todo-keyword-format " ☐ ")
                 (org-agenda-todo-keyword-format "  ")
                 (org-agenda-time)
                 (org-agenda-current-time-string "←  now")
                 (org-agenda-scheduled-leaders '("" ""))
                 (org-agenda-deadline-leaders
                  '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
                 (org-agenda-time-grid
                  '((daily today require-timed)
                    (600 800 1000 1200 1400 1600 1800 2000 2100 2200)
                    "------ "
                    ""))
                 (org-super-agenda-groups
                  '((:name "Today List"
                           :time-grid t
                           :todo t
                           :date today
                           :scheduled today
                           :order 1)
                    ))))
            (alltodo
             "" ((org-agenda-remove-tags t)
                 (org-agenda-overriding-header "")
                 (org-super-agenda-groups
                  '((:name "Working On"
                           :todo " INPROCESS"
                           :order 0)
                    (:name "Next to do"
                           :priority>= "B"
                           :todo " NEXT"
                           :order 1)
                    (:name "Millstone"
                           :todo " Millstone"
                           :order 2)
                    (:name "Due Today"
                           :deadline today
                           :order 3)
                    (:name "Due Soon"
                           :deadline future
                           :order 8)
                    (:name "Planning"
                           :and
                           (:todo " TODO"
                                  :not
                                  (:tag ("Chore" "Routine" "Daily")))
                           :order 21)
                    (:name "Overdue"
                           :deadline past
                           :order 20)
                    (:name "Issues"
                           :todo " FIXME"
                           :order 12)
                    (:name "Projects"
                           :todo " PROJ"
                           :order 14)
                    (:name "Waiting"
                           :todo " WAITING"
                           :order 18)
                    (:name "trivial"
                           :priority<= "C"
                           :todo ("SOMEDAY")
                           :order 90)
                    (:discard
                     (:tag
                      ("Chore" "Routine" "Daily")))))))))

          ("b" . "BOOK")

          ("bb" "Search tags in todo, note, and archives"
           search "+{:book\\|books:}")

          ("bd" "BOOK TODO List"
           search "+{^\\*+\\s-+\\( PROJ\\ INPROCESS\\| TODO\\| WAITING\\)\\s-} +{:book\\|books:}")

          ("d" "ALL DONE OF TASKS"
           search "+{^\\*+\\s-+\\( DONE\\| CANCELED\\)\\s-}")

          ("i" "ALL INPROCESS OF TASKS"
           search "+{^\\*+\\s-+\\( INPROCESS\\)\\s-}")

          ))
  ;; display on Emacs Start up
  ;; (add-hook 'emacs-startup-hook (lambda () (org-agenda nil "u")))
  )

(provide 'init-timetask)
;;; init-timetask.el ends here
