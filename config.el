;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'url-util)
(require 'json)

;;; === Identity ===

(setq user-full-name "bytenoob"
      user-mail-address "chouyc.adam@gmail.com")

;;; === UI ===

(setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16 :weight 'semi-light)
      doom-theme 'doom-one
      display-line-numbers-type t)

;;; === Paths ===

(setq org-directory "~/org/")
(setq org-current-file "~/org/gtd/current.org")
(setq org-archive-file "~/org/gtd/archive.org")

(setq blog-directory "~/github/bufrr.github.io/")
(setq blog-posts-directory (concat blog-directory "posts/"))
(setq blog-templates-directory (concat blog-directory "templates/"))

;;; === GTD helpers ===

(defvar my/gtd-required-headings '("Inbox" "Tasks" "Projects" "Work" "Part-Time" "Learning" "Ideas"))
(defvar my/gtd-review-days 7)
(defvar my/gtd-stale-days 14)
(defvar my/gtd-dashboard-html-file "~/org/gtd/dashboard.html")
(defvar my/gtd-action-server-host "127.0.0.1")
(defvar my/gtd-action-server-port 8765)
(defvar my/gtd-action-server-process nil)
(defvar my/gtd-title-acronyms
  '("ADL" "AI" "API" "AAVE" "BRC20" "BSC" "BTC" "CLI" "EVM" "ETH" "ETH2"
    "GPA" "GTD" "HTTP" "MEV" "P2P" "PR" "RPC" "SOL" "TODO" "UDP" "UI"
    "URL" "XDP" "ZKEVM"))

(defun my/gtd--ensure-org ()
  (require 'org)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAIT(w)" "|"
                    "DONE(d)" "CANCELLED(c)"))))

(defun my/gtd--timestamp ()
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun my/gtd-ensure-headings ()
  "Ensure `org-current-file' has the required top-level GTD headings."
  (interactive)
  (my/gtd--ensure-org)
  (unless (file-exists-p org-current-file)
    (make-directory (file-name-directory org-current-file) t)
    (with-temp-file org-current-file
      (insert "#+TITLE: Current Work\n")
      (insert "#+STARTUP: indent\n")
      (insert "#+STARTUP: show2levels\n\n")))
  (with-current-buffer (find-file-noselect org-current-file)
    (save-excursion
      (dolist (heading my/gtd-required-headings)
        (goto-char (point-min))
        (unless (re-search-forward
                 (format "^\\* %s[ \t]*$" (regexp-quote heading))
                 nil t)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n* " heading "\n"))))
    (save-buffer)))

(defun my/gtd-capture ()
  "Ensure GTD headings exist, then run `org-capture'."
  (interactive)
  (my/gtd-ensure-headings)
  (org-capture))

(defun my/gtd--known-acronym (word)
  (let ((upper (upcase word)))
    (when (member upper my/gtd-title-acronyms)
      upper)))

(defun my/gtd-slug-title (raw)
  "Convert RAW into a readable GTD title."
  (string-join
   (mapcar (lambda (word)
             (or (my/gtd--known-acronym word)
                 (capitalize word)))
           (split-string (string-trim raw) "[ \t]+" t))
   " "))

(defun my/gtd-normalize-title (title)
  "Normalize TITLE for task matching."
  (let ((text (downcase title)))
    (setq text (replace-regexp-in-string
                "\\b\\(todo\\|proj\\|done\\|cancelled\\)\\b" "" text))
    (setq text (replace-regexp-in-string ":[[:alnum:]_@#%:]+:" "" text))
    (string-trim (replace-regexp-in-string "[ \t\n]+" " " text))))

(defun my/gtd--title-words (title)
  (split-string (my/gtd-normalize-title title) "[^[:alnum:]]+" t))

(defun my/gtd--done-state-p (todo)
  (member todo (or org-done-keywords '("DONE" "CANCELLED"))))

(defun my/gtd--open-state-p (todo)
  (and todo (not (my/gtd--done-state-p todo))))

(defun my/gtd--match-score (query title)
  (let* ((normalized-query (my/gtd-normalize-title query))
         (normalized-title (my/gtd-normalize-title title))
         (query-words (my/gtd--title-words query))
         (title-words (my/gtd--title-words title)))
    (cond
     ((string-empty-p normalized-query) nil)
     ((string= normalized-query normalized-title) 100)
     ((and (>= (length normalized-query) 8)
           (string-match-p (regexp-quote normalized-query) normalized-title))
      85)
     ((and (>= (length normalized-title) 8)
           (string-match-p (regexp-quote normalized-title) normalized-query))
      80)
     ((and (>= (length query-words) 2)
           (cl-every (lambda (word) (member word title-words)) query-words))
      75)
     (t nil))))

(defun my/gtd--collect-tasks (&optional include-done)
  (my/gtd--ensure-org)
  (my/gtd-ensure-headings)
  (with-current-buffer (find-file-noselect org-current-file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (tasks)
          (while (re-search-forward org-heading-regexp nil t)
            (let* ((components (org-heading-components))
                   (todo (nth 2 components))
                   (title (nth 4 components)))
              (when (and todo
                         (or include-done
                             (my/gtd--open-state-p todo)))
                (push (list :marker (copy-marker (line-beginning-position))
                            :todo todo
                            :title title)
                      tasks))))
          (nreverse tasks))))))

(defun my/gtd-find-task-matches (query &optional include-done)
  "Return GTD task matches for QUERY."
  (let (matches)
    (dolist (task (my/gtd--collect-tasks include-done))
      (when-let ((score (my/gtd--match-score query (plist-get task :title))))
        (push (plist-put task :score score) matches)))
    (cl-sort matches #'> :key (lambda (task) (plist-get task :score)))))

(defun my/gtd--insert-heading-line (todo title tags)
  (let ((start (point))
        (tag-text (when tags
                    (concat " :" (string-join tags ":") ":"))))
    (insert (format "** %s%s%s\n"
                    (if todo (concat todo " ") "")
                    title
                    (or tag-text "")))
    (when tags
      (save-excursion
        (goto-char start)
        (org-align-tags)))))

(defun my/gtd-insert-entry (section todo title tags effort &optional children)
  "Insert a GTD entry into SECTION."
  (my/gtd--ensure-org)
  (my/gtd-ensure-headings)
  (with-current-buffer (find-file-noselect org-current-file)
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward
               (format "^\\* %s[ \t]*$" (regexp-quote section))
               nil t)
        (user-error "Missing GTD section: %s" section))
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (my/gtd--insert-heading-line todo title tags)
      (insert "   :PROPERTIES:\n")
      (when effort
        (insert (format "   :Effort: %s\n" effort)))
      (insert (format "   :Created: %s\n" (my/gtd--timestamp)))
      (insert "   :Source: codex\n")
      (insert "   :END:\n")
      (dolist (child children)
        (insert (format "*** TODO %s\n" child))))
    (save-buffer))
  title)

(defun my/gtd--text-has-any (text words)
  (cl-some (lambda (word)
             (string-match-p
              (concat "\\b" (regexp-quote word) "\\b")
              text))
           words))

(defun my/gtd--project-children (raw)
  (let ((lower (downcase raw)))
    (cond
     ((string-match-p "\\b\\(presentation\\|slides\\)\\b" lower)
      '("Collect source material" "Draft outline" "Review final version"))
     ((string-match-p "\\b\\(doc\\|document\\|guide\\)\\b" lower)
      '("Collect context" "Draft document" "Review final version"))
     (t '("Clarify outcome" "Plan next action" "Review result")))))

(defun my/gtd-classify-task (raw)
  "Classify RAW task text into a GTD insertion plist."
  (let* ((lower (downcase raw))
         (idea (my/gtd--text-has-any lower '("idea" "maybe" "someday" "write about")))
         (learning (my/gtd--text-has-any lower '("learn" "study" "read" "course" "leetcode")))
         (shallow (my/gtd--text-has-any lower '("send" "reply" "update" "schedule" "email")))
         (work (my/gtd--text-has-any
                lower
                '("api" "code" "review" "doc" "design" "incident" "gateway" "tx"
                  "repo" "hyperliquid" "bsc" "evm" "sol" "eth" "btc")))
         (project (my/gtd--text-has-any
                   lower
                   '("prepare" "presentation" "project" "release" "build" "launch"))))
    (cond
     (idea (list :section "Ideas" :todo nil :tags '("idea") :effort nil))
     (learning (list :section "Learning" :todo "TODO" :tags '("deep" "learning") :effort "1:00"))
     (project (list :section (if work "Work" "Projects")
                    :todo "PROJ"
                    :tags (if work '("deep" "work") '("deep"))
                    :effort "2:00"
                    :children (my/gtd--project-children raw)))
     (work (list :section "Work" :todo "TODO" :tags '("deep" "work") :effort "1:00"))
     (shallow (list :section "Tasks" :todo "TODO" :tags '("shallow") :effort "0:30"))
     (t (list :section "Tasks" :todo "TODO" :tags nil :effort "0:30")))))

(defun my/gtd--exact-match (query include-done)
  (cl-find-if (lambda (task)
                (= (plist-get task :score) 100))
              (my/gtd-find-task-matches query include-done)))

(defun my/gtd-add-task (raw &optional force)
  "Add RAW to the GTD system unless it is an open duplicate."
  (interactive "sTask: ")
  (let* ((classification (my/gtd-classify-task raw))
         (title (my/gtd-slug-title raw))
         (todo (plist-get classification :todo))
         (open-match (my/gtd--exact-match title nil))
         (done-match (my/gtd--exact-match title t)))
    (cond
     (open-match
      (message "Task already exists: %s" (plist-get open-match :title))
      (plist-get open-match :title))
     ((and done-match
           (my/gtd--done-state-p (plist-get done-match :todo))
           (not force))
      (if (and (called-interactively-p 'interactive)
               (yes-or-no-p "A completed task matches. Add a new task? "))
          (my/gtd-insert-entry
           (plist-get classification :section)
           todo
           (if (string= todo "PROJ") (concat title " [0%]") title)
           (plist-get classification :tags)
           (plist-get classification :effort)
           (plist-get classification :children))
        (message "Skipped completed duplicate: %s" (plist-get done-match :title))
        (plist-get done-match :title)))
     (t
      (when (string= todo "PROJ")
        (setq title (concat title " [0%]")))
      (my/gtd-insert-entry
       (plist-get classification :section)
       todo
       title
       (plist-get classification :tags)
       (plist-get classification :effort)
       (plist-get classification :children))))))

(defun my/gtd-add-task-batch (raw)
  "Add one or more GTD tasks from RAW."
  (interactive "sTasks: ")
  (let* ((parts (split-string raw "[\n;]+" t "[ \t]+"))
         (tasks (cl-mapcan
                 (lambda (part)
                   (if (and (string-match-p "," part)
                            (cl-every (lambda (piece)
                                        (>= (length (split-string piece "[ \t]+" t)) 2))
                                      (split-string part "," t "[ \t]+")))
                       (split-string part "," t "[ \t]+")
                     (list part)))
                 parts)))
    (mapcar #'my/gtd-add-task tasks)))

(defun my/gtd-add-deep-work-task (raw)
  "Add RAW as a deep work task."
  (interactive "sDeep work task: ")
  (my/gtd-insert-entry "Work" "TODO" (my/gtd-slug-title raw) '("deep" "work") "1:00"))

(defun my/gtd-add-shallow-task (raw)
  "Add RAW as a shallow task."
  (interactive "sShallow task: ")
  (my/gtd-insert-entry "Tasks" "TODO" (my/gtd-slug-title raw) '("shallow") "0:30"))

(defun my/gtd-add-learning-task (raw)
  "Add RAW as a learning task."
  (interactive "sLearning task: ")
  (my/gtd-insert-entry "Learning" "TODO" (my/gtd-slug-title raw) '("deep" "learning") "1:00"))

(defun my/gtd-add-work-task (raw)
  "Add RAW as a work task."
  (interactive "sWork task: ")
  (my/gtd-insert-entry "Work" "TODO" (my/gtd-slug-title raw) '("work") "0:30"))

(defun my/gtd--completion-query (raw)
  (string-trim
   (replace-regexp-in-string
    "\\`\\(?:i[ \t]+finished\\|finished\\|done\\|completed\\|mark[ \t]+done\\|mark[ \t]+as[ \t]+done\\)[ \t:,-]*"
    ""
    (string-trim raw))))

(defun my/gtd--ensure-closed ()
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (unless (re-search-forward "^[ \t]*CLOSED:" end t)
        (org-back-to-heading t)
        (end-of-line)
        (insert (format "\n   CLOSED: %s" (my/gtd--timestamp)))))))

(defun my/gtd--complete-match (task)
  (with-current-buffer (find-file-noselect org-current-file)
    (goto-char (plist-get task :marker))
    (org-todo "DONE")
    (my/gtd--ensure-closed)
    (when (fboundp 'my/org-safe-update-checkboxes)
      (my/org-safe-update-checkboxes))
    (save-buffer))
  (plist-get task :title))

(defun my/gtd-complete-task (raw)
  "Mark the GTD task matching RAW as DONE."
  (interactive "sFinished task: ")
  (let* ((query (my/gtd--completion-query raw))
         (matches (my/gtd-find-task-matches query nil))
         (best (car matches))
         (second (cadr matches)))
    (cond
     ((null best)
      (user-error "No open task matches: %s" query))
     ((and second
           (= (plist-get best :score) (plist-get second :score)))
      (let* ((choice (completing-read
                      "Complete task: "
                      (mapcar (lambda (task) (plist-get task :title)) matches)
                      nil t))
             (task (cl-find-if (lambda (candidate)
                                 (string= choice (plist-get candidate :title)))
                               matches)))
        (message "Marked done: %s" (my/gtd--complete-match task))))
     ((>= (plist-get best :score) 75)
      (message "Marked done: %s" (my/gtd--complete-match best)))
     (t
      (user-error "No strong task match for: %s" query)))))

(defun my/gtd--set-task-todo (task todo)
  (with-current-buffer (find-file-noselect org-current-file)
    (goto-char (plist-get task :marker))
    (org-todo todo)
    (save-buffer))
  (plist-get task :title))

(defun my/gtd-mark-next-task (raw)
  "Mark the open GTD task matching RAW as NEXT."
  (interactive "sNext task: ")
  (let* ((query (string-trim raw))
         (matches (my/gtd-find-task-matches query nil))
         (best (car matches))
         (second (cadr matches)))
    (cond
     ((null best)
      (user-error "No open task matches: %s" query))
     ((and second
           (= (plist-get best :score) (plist-get second :score)))
      (let* ((choice (completing-read
                      "Mark NEXT: "
                      (mapcar (lambda (task) (plist-get task :title)) matches)
                      nil t))
             (task (cl-find-if (lambda (candidate)
                                 (string= choice (plist-get candidate :title)))
                               matches)))
        (message "Marked NEXT: %s" (my/gtd--set-task-todo task "NEXT"))))
     ((>= (plist-get best :score) 75)
      (message "Marked NEXT: %s" (my/gtd--set-task-todo best "NEXT")))
     (t
      (user-error "No strong task match for: %s" query)))))

(defun my/gtd-dashboard ()
  "Open the GTD dashboard agenda."
  (interactive)
  (org-agenda nil "g"))

(defun my/gtd--existing-files ()
  (cl-remove-if-not
   #'file-exists-p
   (mapcar #'expand-file-name (list org-current-file org-archive-file))))

(defun my/gtd--timestamp-to-time (timestamp)
  (when (and timestamp (string-match-p "\\S-" timestamp))
    (ignore-errors (org-time-string-to-time timestamp))))

(defun my/gtd--entry-timestamp (label)
  (org-entry-get (point) label))

(defun my/gtd--top-heading ()
  (save-excursion
    (while (and (> (or (org-current-level) 1) 1)
                (org-up-heading-safe)))
    (nth 4 (org-heading-components))))

(defun my/gtd--clean-outline-title (title)
  "Return a compact display version of an Org outline TITLE."
  (let ((text (or title "")))
    (setq text (replace-regexp-in-string
                "\\`[[:space:]]*\\(?:TODO\\|NEXT\\|PROJ\\|WAIT\\|DONE\\|CANCELLED\\)[[:space:]]+"
                ""
                text))
    (setq text (replace-regexp-in-string
                "\\`[[:space:]]*\\[#.\\][[:space:]]*"
                ""
                text))
    (setq text (replace-regexp-in-string
                "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]"
                ""
                text))
    (string-trim (replace-regexp-in-string "[ \t\n]+" " " text))))

(defun my/gtd--subtask-stats ()
  "Return descendant TODO statistics for the Org subtree at point."
  (save-excursion
    (org-back-to-heading t)
    (let ((level (org-current-level))
          (end (save-excursion (org-end-of-subtree t t) (point)))
          (total 0)
          (done 0)
          (open 0)
          (next 0)
          (wait 0)
          (projects 0))
      (forward-line 1)
      (while (re-search-forward org-heading-regexp end t)
        (let ((child-level (org-current-level))
              (todo (nth 2 (org-heading-components))))
          (when (and (> child-level level) todo)
            (cl-incf total)
            (if (my/gtd--done-state-p todo)
                (cl-incf done)
              (cl-incf open))
            (when (string= todo "NEXT")
              (cl-incf next))
            (when (string= todo "WAIT")
              (cl-incf wait))
            (when (string= todo "PROJ")
              (cl-incf projects)))))
      (list :total total
            :done done
            :open open
            :next next
            :wait wait
            :projects projects
            :percent (if (> total 0)
                         (/ (* done 100) total)
                       0)))))

(defun my/gtd--child-task-preview (&optional limit)
  "Return visible descendant TODO headings for the Org subtree at point."
  (save-excursion
    (org-back-to-heading t)
    (let ((root-level (org-current-level))
          (end (save-excursion (org-end-of-subtree t t) (point)))
          (limit (or limit 8))
          children)
      (forward-line 1)
      (while (and (< (length children) limit)
                  (re-search-forward org-heading-regexp end t))
        (let* ((components (org-heading-components))
               (child-level (nth 0 components))
               (todo (nth 2 components))
               (title (nth 4 components)))
          (when (and (> child-level root-level) todo)
            (push (list :depth (- child-level root-level)
                        :todo todo
                        :title (my/gtd--clean-outline-title title))
                  children))))
      (nreverse children))))

(defun my/gtd--collect-entries (&optional files)
  (my/gtd--ensure-org)
  (let (entries)
    (dolist (file (or files (my/gtd--existing-files)))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (re-search-forward org-heading-regexp nil t)
                (let* ((components (org-heading-components))
                       (level (nth 0 components))
                       (todo (nth 2 components))
                       (priority (nth 3 components))
                       (title (nth 4 components))
                       (tags (org-get-tags))
                       (effort (org-entry-get (point) "Effort"))
                       (created (or (org-entry-get (point) "Created")
                                    (org-entry-get (point) "CREATED")))
                       (scheduled (org-entry-get (point) "SCHEDULED"))
                       (closed (my/gtd--entry-timestamp "CLOSED"))
                       (outline-path (mapcar #'my/gtd--clean-outline-title
                                             (org-get-outline-path t)))
                       (parent-path (butlast outline-path))
                       (parent-title (car (last parent-path)))
                       (subtask-stats (my/gtd--subtask-stats))
                       (child-preview (my/gtd--child-task-preview))
                       (parent-stats
                        (save-excursion
                          (when (org-up-heading-safe)
                            (my/gtd--subtask-stats)))))
                  (push (list :file (expand-file-name file)
                              :marker (copy-marker (line-beginning-position))
                              :line (line-number-at-pos)
                              :level level
                              :section (my/gtd--top-heading)
                              :todo todo
                              :priority priority
                              :title title
                              :outline-path outline-path
                              :parent-path parent-path
                              :parent-title parent-title
                              :tags tags
                              :effort effort
                              :created created
                              :created-time (my/gtd--timestamp-to-time created)
                              :scheduled scheduled
                              :scheduled-time (my/gtd--timestamp-to-time scheduled)
                              :closed closed
                              :closed-time (my/gtd--timestamp-to-time closed)
                              :subtask-total (plist-get subtask-stats :total)
                              :subtask-done (plist-get subtask-stats :done)
                              :subtask-open (plist-get subtask-stats :open)
                              :subtask-next (plist-get subtask-stats :next)
                              :subtask-wait (plist-get subtask-stats :wait)
                              :subtask-projects (plist-get subtask-stats :projects)
                              :subtask-percent (plist-get subtask-stats :percent)
                              :child-preview child-preview
                              :parent-subtask-total (plist-get parent-stats :total)
                              :parent-subtask-done (plist-get parent-stats :done)
                              :parent-subtask-open (plist-get parent-stats :open)
                              :parent-subtask-next (plist-get parent-stats :next)
                              :parent-subtask-wait (plist-get parent-stats :wait)
                              :parent-subtask-projects (plist-get parent-stats :projects)
                              :parent-subtask-percent (plist-get parent-stats :percent))
                        entries))))))))
    (nreverse entries)))

(defun my/gtd--entry-has-open-child-p (entry &optional states)
  (with-current-buffer (find-file-noselect (plist-get entry :file))
    (save-excursion
      (goto-char (plist-get entry :marker))
      (let ((level (org-current-level))
            (end (save-excursion (org-end-of-subtree t t) (point)))
            found)
        (forward-line 1)
        (while (and (not found)
                    (re-search-forward org-heading-regexp end t))
          (let ((child-level (org-current-level))
                (todo (nth 2 (org-heading-components))))
            (when (and (> child-level level)
                       todo
                       (if states
                           (member todo states)
                         (my/gtd--open-state-p todo)))
              (setq found t))))
        found))))

(defun my/gtd--entry-open-p (entry)
  (let ((todo (plist-get entry :todo)))
    (and todo (my/gtd--open-state-p todo))))

(defun my/gtd--entry-link-description (title)
  (let ((text (replace-regexp-in-string "[\n\r\t ]+" " " title)))
    (setq text (replace-regexp-in-string "\\[" "(" text))
    (replace-regexp-in-string "\\]" ")" text)))

(defun my/gtd--entry-link (entry)
  (format "[[file:%s::%d][%s]]"
          (plist-get entry :file)
          (plist-get entry :line)
          (my/gtd--entry-link-description (plist-get entry :title))))

(defun my/gtd--entry-tags-text (entry)
  (when-let ((tags (plist-get entry :tags)))
    (unless (null tags)
      (concat ":" (string-join tags ":") ":"))))

(defun my/gtd--entry-extra-text (entry &optional show-closed)
  (string-join
   (delq nil
         (list (when-let ((tags (my/gtd--entry-tags-text entry)))
                 tags)
               (when-let ((effort (plist-get entry :effort)))
                 (format "Effort %s" effort))
               (when show-closed
                 (when-let ((closed (plist-get entry :closed)))
                   (format "Closed %s" closed)))))
   "  "))

(defun my/gtd--priority-rank (entry)
  (pcase (plist-get entry :priority)
    (?A 0)
    (?B 1)
    (?C 2)
    (_ 3)))

(defun my/gtd--effort-minutes (entry)
  (let ((effort (plist-get entry :effort)))
    (if (and effort
             (string-match "\\`\\([0-9]+\\):\\([0-9][0-9]\\)\\'" effort))
        (+ (* 60 (string-to-number (match-string 1 effort)))
           (string-to-number (match-string 2 effort)))
      9999)))

(defun my/gtd--sort-actions (entries)
  (cl-sort
   (copy-sequence entries)
   (lambda (a b)
     (let ((priority-a (my/gtd--priority-rank a))
           (priority-b (my/gtd--priority-rank b))
           (effort-a (my/gtd--effort-minutes a))
           (effort-b (my/gtd--effort-minutes b)))
       (cond
        ((/= priority-a priority-b) (< priority-a priority-b))
        ((/= effort-a effort-b) (< effort-a effort-b))
        (t (string< (plist-get a :title)
                    (plist-get b :title))))))))

(defun my/gtd--sort-closed (entries)
  (cl-sort
   (copy-sequence entries)
   (lambda (a b)
     (time-less-p (plist-get b :closed-time)
                  (plist-get a :closed-time)))))

(defun my/gtd--insert-entry-list (entries empty-text &optional show-closed)
  (if entries
      (dolist (entry entries)
        (let ((extra (my/gtd--entry-extra-text entry show-closed)))
          (insert (format "- %s %s%s\n"
                          (or (plist-get entry :todo) "-")
                          (my/gtd--entry-link entry)
                          (if (string-empty-p extra)
                              ""
                            (concat "  " extra))))))
    (insert (format "- %s\n" empty-text))))

(defun my/gtd--current-action-groups ()
  (let* ((current-file (expand-file-name org-current-file))
         (entries (my/gtd--collect-entries (list current-file)))
         (explicit-next
          (cl-remove-if-not
           (lambda (entry) (string= (plist-get entry :todo) "NEXT"))
           entries))
         (candidate-next
          (cl-remove-if-not
           (lambda (entry)
             (and (string= (plist-get entry :todo) "TODO")
                  (not (my/gtd--entry-has-open-child-p entry))))
           entries))
         (waiting
          (cl-remove-if-not
           (lambda (entry) (string= (plist-get entry :todo) "WAIT"))
           entries))
         (projects-missing-next
          (cl-remove-if-not
           (lambda (entry)
             (and (string= (plist-get entry :todo) "PROJ")
                  (not (my/gtd--entry-has-open-child-p entry '("NEXT")))))
           entries))
         (inbox
          (cl-remove-if-not
           (lambda (entry)
             (and (> (plist-get entry :level) 1)
                  (string= (plist-get entry :section) "Inbox")))
           entries))
         (scheduled
          (cl-remove-if-not
           (lambda (entry)
             (and (my/gtd--entry-open-p entry)
                  (plist-get entry :scheduled)))
           entries))
         (someday
          (cl-remove-if-not
           (lambda (entry)
             (and (> (plist-get entry :level) 1)
                  (string= (plist-get entry :section) "Ideas")))
           entries))
         (stale-cutoff (time-subtract (current-time)
                                      (days-to-time my/gtd-stale-days)))
         (stale
          (cl-remove-if-not
           (lambda (entry)
             (and (member (plist-get entry :todo) '("TODO" "NEXT"))
                  (plist-get entry :created-time)
                  (time-less-p (plist-get entry :created-time) stale-cutoff)))
           entries)))
    (list :next (my/gtd--sort-actions explicit-next)
          :candidates (my/gtd--sort-actions candidate-next)
          :waiting (my/gtd--sort-actions waiting)
          :projects-missing-next (my/gtd--sort-actions projects-missing-next)
          :inbox inbox
          :scheduled (my/gtd--sort-actions scheduled)
          :someday someday
          :stale (my/gtd--sort-actions stale))))

(defun my/gtd--insert-next-action-sections (groups)
  (insert "* NEXT Actions\n")
  (my/gtd--insert-entry-list
   (plist-get groups :next)
   "No explicit NEXT actions yet. Mark one with SPC g N.")
  (insert "\n* Candidate Next Actions\n")
  (my/gtd--insert-entry-list
   (plist-get groups :candidates)
   "No leaf TODO tasks found.")
  (insert "\n* Projects Missing NEXT\n")
  (my/gtd--insert-entry-list
   (plist-get groups :projects-missing-next)
   "Every open project has a NEXT action.")
  (insert "\n* Waiting\n")
  (my/gtd--insert-entry-list
   (plist-get groups :waiting)
   "Nothing is waiting. Nice."))

(defun my/gtd-next-actions ()
  "Show a compact GTD cockpit for choosing what to do next."
  (interactive)
  (my/gtd-ensure-headings)
  (let ((groups (my/gtd--current-action-groups)))
    (with-current-buffer (get-buffer-create "*GTD Next Actions*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+TITLE: GTD Next Actions\n")
        (insert (format "#+DATE: %s\n\n" (my/gtd--timestamp)))
        (my/gtd--insert-next-action-sections groups)
        (goto-char (point-min))
        (org-mode)
        (setq buffer-read-only t))
      (pop-to-buffer (current-buffer)))))

(defun my/gtd-weekly-review (&optional days)
  "Show completed work and review prompts for the last DAYS."
  (interactive
   (list (if current-prefix-arg
             (read-number "Review days: " my/gtd-review-days)
           my/gtd-review-days)))
  (my/gtd-ensure-headings)
  (let* ((days (or days my/gtd-review-days))
         (cutoff (time-subtract (current-time) (days-to-time days)))
         (all-entries (my/gtd--collect-entries))
         (completed
          (my/gtd--sort-closed
           (cl-remove-if-not
            (lambda (entry)
              (and (my/gtd--done-state-p (plist-get entry :todo))
                   (plist-get entry :closed-time)
                   (not (time-less-p (plist-get entry :closed-time) cutoff))))
            all-entries)))
         (groups (my/gtd--current-action-groups)))
    (with-current-buffer (get-buffer-create "*GTD Weekly Review*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+TITLE: GTD Weekly Review\n")
        (insert (format "#+DATE: %s\n" (my/gtd--timestamp)))
        (insert (format "#+OPTIONS: toc:nil\n\n"))
        (insert (format "* Completed In Last %d Days\n" days))
        (my/gtd--insert-entry-list completed "No completed tasks found in this window." t)
        (insert "\n")
        (my/gtd--insert-next-action-sections groups)
        (insert "\n* Inbox\n")
        (my/gtd--insert-entry-list
         (plist-get groups :inbox)
         "Inbox is empty.")
        (insert "\n* Stale Open Tasks\n")
        (my/gtd--insert-entry-list
         (plist-get groups :stale)
         (format "No TODO/NEXT tasks older than %d days." my/gtd-stale-days))
        (goto-char (point-min))
        (org-mode)
        (setq buffer-read-only t))
      (pop-to-buffer (current-buffer)))))

(defun my/gtd--completed-since (days)
  (let ((cutoff (time-subtract (current-time) (days-to-time days))))
    (my/gtd--sort-closed
     (cl-remove-if-not
      (lambda (entry)
        (and (my/gtd--done-state-p (plist-get entry :todo))
             (plist-get entry :closed-time)
             (not (time-less-p (plist-get entry :closed-time) cutoff))))
      (my/gtd--collect-entries)))))

(defun my/gtd--same-file-p (a b)
  (and a b
       (string= (file-truename (expand-file-name a))
                (file-truename (expand-file-name b)))))

(defun my/gtd--allowed-dashboard-file-p (file)
  (let ((expanded (and file (expand-file-name file))))
    (and expanded
         (file-exists-p expanded)
         (cl-some (lambda (allowed)
                    (my/gtd--same-file-p expanded allowed))
                  (my/gtd--existing-files)))))

(defun my/gtd--current-dashboard-file-p (file)
  (my/gtd--same-file-p file org-current-file))

(defun my/gtd--goto-entry-in-buffer (line title)
  "Move point to the Org heading near LINE with TITLE."
  (let ((line-number (max 1 (string-to-number (format "%s" (or line "1")))))
        (target-title (or title "")))
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (org-at-heading-p)
      (ignore-errors (org-back-to-heading t)))
    (unless (and (org-at-heading-p)
                 (or (string-empty-p target-title)
                     (string= target-title (nth 4 (org-heading-components)))))
      (goto-char (point-min))
      (let (found)
        (while (and (not found)
                    (re-search-forward org-heading-regexp nil t))
          (when (string= target-title (nth 4 (org-heading-components)))
            (setq found t)
            (beginning-of-line)))
        (unless found
          (user-error "Cannot find GTD entry: %s" target-title))))
    (org-back-to-heading t)))

(defun my/gtd--with-dashboard-entry (file line title fn &optional allow-archive)
  "Run FN at the dashboard entry identified by FILE, LINE, and TITLE."
  (let ((expanded (expand-file-name (or file ""))))
    (unless (my/gtd--allowed-dashboard-file-p expanded)
      (user-error "Refusing to open non-GTD file: %s" expanded))
    (unless (or allow-archive
                (my/gtd--current-dashboard-file-p expanded))
      (user-error "Refusing to mutate archived GTD file: %s" expanded))
    (with-current-buffer (find-file-noselect expanded)
      (my/gtd--ensure-org)
      (save-excursion
        (save-restriction
          (widen)
          (my/gtd--goto-entry-in-buffer line title)
          (funcall fn))))))

(defun my/gtd--dashboard-action-state (action)
  (pcase action
    ("todo" "TODO")
    ("next" "NEXT")
    ("wait" "WAIT")
    ("done" "DONE")
    (_ nil)))

(defun my/gtd--set-dashboard-entry-state (file line title action)
  "Set a dashboard entry TODO state and refresh the HTML dashboard."
  (let ((state (my/gtd--dashboard-action-state action)))
    (unless state
      (user-error "Unknown GTD action: %s" action))
    (my/gtd--with-dashboard-entry
     file line title
     (lambda ()
       (let ((org-inhibit-logging t)
             (org-log-done nil)
             (org-todo-log-states nil))
         (org-todo state))
       (when (string= state "DONE")
         (my/gtd--ensure-closed))
       (when (fboundp 'my/org-safe-update-checkboxes)
         (my/org-safe-update-checkboxes))
       (save-buffer)))
    (my/gtd-export-dashboard-html my/gtd-review-days)
    (format "Marked %s: %s" state title)))

(defun my/gtd--dashboard-area-section (area)
  (pcase area
    ("work" "Work")
    ("parttime" "Part-Time")
    ("learn" "Learning")
    (_ "Tasks")))

(defun my/gtd--dashboard-area-tags (area)
  (pcase area
    ("work" '("work"))
    ("parttime" '("parttime"))
    ("learn" '("learning"))
    (_ nil)))

(defun my/gtd--dashboard-area-effort (area)
  (pcase area
    ((or "work" "parttime" "learn") "1:00")
    (_ "0:30")))

(defun my/gtd--add-dashboard-task (area title)
  "Add TITLE to the GTD section represented by AREA and refresh the dashboard."
  (let ((clean-title (string-trim (or title ""))))
    (when (string-empty-p clean-title)
      (user-error "Task title is required"))
    (my/gtd-insert-entry
     (my/gtd--dashboard-area-section area)
     "TODO"
     (my/gtd-slug-title clean-title)
     (my/gtd--dashboard-area-tags area)
     (my/gtd--dashboard-area-effort area))
    (my/gtd-export-dashboard-html my/gtd-review-days)
    (format "Added task: %s" clean-title)))

(defun my/gtd--open-dashboard-entry (file line title)
  "Open the dashboard entry identified by FILE, LINE, and TITLE in Emacs."
  (let ((expanded (expand-file-name (or file ""))))
    (unless (my/gtd--allowed-dashboard-file-p expanded)
      (user-error "Refusing to open non-GTD file: %s" expanded))
    (let ((buffer (find-file-noselect expanded)))
      (pop-to-buffer buffer)
      (save-restriction
        (widen)
        (my/gtd--goto-entry-in-buffer line title)
        (org-show-context)
        (ignore-errors (org-reveal))
        (recenter)))
    (format "Opened: %s" title)))

(defun my/gtd--handle-dashboard-action (params)
  "Handle dashboard action PARAMS from the local action server."
  (let ((action (plist-get params :action))
        (file (plist-get params :file))
        (line (plist-get params :line))
        (title (plist-get params :title))
        (area (plist-get params :area)))
    (pcase action
      ("refresh"
       (my/gtd-export-dashboard-html my/gtd-review-days)
       "Dashboard refreshed")
      ("open"
       (my/gtd--open-dashboard-entry file line title))
      ("add"
       (my/gtd--add-dashboard-task area title))
      ((or "todo" "next" "wait" "done")
       (my/gtd--set-dashboard-entry-state file line title action))
      (_
       (user-error "Unknown GTD action: %s" action)))))

(defun my/gtd--decode-query-value (value)
  (url-unhex-string
   (replace-regexp-in-string "\\+" "%20" (or value ""))))

(defun my/gtd--parse-query-string (query)
  "Parse a small URL QUERY string into a plist."
  (let (params)
    (dolist (pair (split-string (or query "") "&" t))
      (let* ((parts (split-string pair "="))
             (key (car parts))
             (value (string-join (cdr parts) "=")))
        (unless (string-empty-p (or key ""))
          (setq params
                (plist-put params
                           (intern (concat ":" (my/gtd--decode-query-value key)))
                           (my/gtd--decode-query-value value))))))
    params))

(defun my/gtd--action-server-send (process status body)
  "Send HTTP STATUS and JSON BODY to PROCESS."
  (let* ((payload (encode-coding-string (json-encode body) 'utf-8))
         (headers (concat
                   (format "HTTP/1.1 %s\r\n" status)
                   "Content-Type: application/json; charset=utf-8\r\n"
                   "Access-Control-Allow-Origin: *\r\n"
                   "Access-Control-Allow-Methods: GET, OPTIONS\r\n"
                   "Cache-Control: no-store\r\n"
                   (format "Content-Length: %d\r\n" (string-bytes payload))
                   "Connection: close\r\n\r\n")))
    (process-send-string process headers)
    (process-send-string process payload)
    (delete-process process)))

(defun my/gtd--action-server-handle-request (process request)
  "Handle one HTTP REQUEST for PROCESS."
  (if (not (string-match "\\`\\([A-Z]+\\) \\([^ ]+\\) HTTP/" request))
      (my/gtd--action-server-send
       process "400 Bad Request"
       '(:ok nil :message "Malformed request"))
    (let ((method (match-string 1 request))
          (target (match-string 2 request)))
      (cond
       ((string= method "OPTIONS")
        (my/gtd--action-server-send process "204 No Content" '(:ok t)))
       ((not (string= method "GET"))
        (my/gtd--action-server-send
         process "405 Method Not Allowed"
         '(:ok nil :message "Only GET is supported")))
       (t
        (let* ((query-start (string-match-p "\\?" target))
               (path (url-unhex-string
                      (if query-start
                          (substring target 0 query-start)
                        target)))
               (query (when query-start
                        (substring target (1+ query-start)))))
          (if (not (string= path "/gtd-action"))
              (my/gtd--action-server-send
               process "404 Not Found"
               '(:ok nil :message "Unknown GTD endpoint"))
            (condition-case err
                (let ((message (my/gtd--handle-dashboard-action
                                (my/gtd--parse-query-string query))))
                  (my/gtd--action-server-send
                   process "200 OK"
                   (list :ok t
                         :message message
                         :dashboard (expand-file-name my/gtd-dashboard-html-file))))
              (error
               (my/gtd--action-server-send
                process "400 Bad Request"
                (list :ok nil :message (error-message-string err))))))))))))

(defun my/gtd--action-server-filter (process chunk)
  "Accumulate HTTP CHUNK from PROCESS and dispatch complete requests."
  (let ((request (concat (or (process-get process :request) "") chunk)))
    (process-put process :request request)
    (when (string-match-p "\r?\n\r?\n" request)
      (my/gtd--action-server-handle-request process request))))

(defun my/gtd-action-server-start ()
  "Start the local GTD dashboard action server."
  (interactive)
  (unless (process-live-p my/gtd-action-server-process)
    (setq my/gtd-action-server-process
          (make-network-process
           :name "gtd-action-server"
           :server t
           :host my/gtd-action-server-host
           :service my/gtd-action-server-port
           :noquery t
           :filter #'my/gtd--action-server-filter))
    (message "GTD action server listening on http://%s:%d"
             my/gtd-action-server-host
             my/gtd-action-server-port))
  my/gtd-action-server-process)

(defun my/gtd-action-server-stop ()
  "Stop the local GTD dashboard action server."
  (interactive)
  (when (process-live-p my/gtd-action-server-process)
    (delete-process my/gtd-action-server-process)
    (setq my/gtd-action-server-process nil)
    (message "GTD action server stopped")))

(defun my/gtd--html-escape (text)
  (let ((text (or text "")))
    (setq text (replace-regexp-in-string "&" "&amp;" text t t))
    (setq text (replace-regexp-in-string "<" "&lt;" text t t))
    (setq text (replace-regexp-in-string ">" "&gt;" text t t))
    (setq text (replace-regexp-in-string "\"" "&quot;" text t t))
    (replace-regexp-in-string "'" "&#39;" text t t)))

(defun my/gtd--dashboard-url-param (key value)
  (format "%s=%s"
          (url-hexify-string key)
          (url-hexify-string (format "%s" (or value "")))))

(defun my/gtd--dashboard-action-url (action &optional entry)
  (let ((params (list (cons "action" action))))
    (when entry
      (setq params
            (append params
                    (list (cons "file" (plist-get entry :file))
                          (cons "line" (plist-get entry :line))
                          (cons "title" (plist-get entry :title))))))
    (format "http://%s:%d/gtd-action?%s"
            my/gtd-action-server-host
            my/gtd-action-server-port
            (mapconcat
             (lambda (param)
               (my/gtd--dashboard-url-param (car param) (cdr param)))
             params
             "&"))))

(defun my/gtd--dashboard-action-endpoint ()
  (format "http://%s:%d/gtd-action"
          my/gtd-action-server-host
          my/gtd-action-server-port))

(defun my/gtd--html-state-class (entry)
  (downcase (or (plist-get entry :todo) "note")))

(defun my/gtd--entry-area (entry)
  (let ((section (plist-get entry :section))
        (tags (plist-get entry :tags)))
    (cond
     ((or (string= section "Work")
          (member "work" tags))
      "work")
     ((or (string= section "Part-Time")
          (member "parttime" tags)
          (member "part-time" tags))
      "parttime")
     ((or (string= section "Learning")
          (member "learning" tags))
      "learn")
     (t "other"))))

(defun my/gtd--entry-area-label (entry)
  (pcase (my/gtd--entry-area entry)
    ("work" "Work")
    ("parttime" "Part-Time")
    ("learn" "Learn")
    (_ "Other")))

(defun my/gtd--html-action-link (entry action label &optional reload primary)
  (format
   "<a class=\"task-action%s\" href=\"%s\" data-gtd-command=\"%s\" data-gtd-reload=\"%s\">%s</a>"
   (if primary " primary" "")
   (my/gtd--html-escape (my/gtd--dashboard-action-url action entry))
   (my/gtd--html-escape action)
   (if reload "true" "false")
   (my/gtd--html-escape label)))

(defun my/gtd--html-actions (entry &optional omit-open)
  (let* ((todo (plist-get entry :todo))
         (mutable (and todo
                       (my/gtd--current-dashboard-file-p
                        (plist-get entry :file))))
         (links nil))
    (when mutable
      (setq links
            (pcase todo
              ("TODO"
               (list (my/gtd--html-action-link entry "next" "Start" t t)
                     (my/gtd--html-action-link entry "done" "Done" t)
                     (my/gtd--html-action-link entry "wait" "Wait" t)))
              ("NEXT"
               (list (my/gtd--html-action-link entry "done" "Done" t t)
                     (my/gtd--html-action-link entry "wait" "Wait" t)
                     (my/gtd--html-action-link entry "todo" "Todo" t)))
              ("WAIT"
               (list (my/gtd--html-action-link entry "next" "Start" t t)
                     (my/gtd--html-action-link entry "done" "Done" t)))
              (_ nil))))
    (unless omit-open
      (setq links
            (append links
                    (list (my/gtd--html-action-link entry "open" "Open" nil)))))
    (if links
        (format "<div class=\"item-actions\">%s</div>"
                (string-join links ""))
      "")))

(defun my/gtd--html-outline (entry)
  (let ((path (cl-remove-if #'string-empty-p
                            (or (plist-get entry :parent-path) nil))))
    (if path
        (format
         "<div class=\"outline-path\" aria-label=\"Org path\">%s</div>"
         (mapconcat
          (lambda (part)
            (format "<span>%s</span>" (my/gtd--html-escape part)))
          path
          "<span class=\"outline-sep\">/</span>"))
      "")))

(defun my/gtd--html-progress (entry)
  (let* ((subtask-total (or (plist-get entry :subtask-total) 0))
         (parent-total (or (plist-get entry :parent-subtask-total) 0))
         (parent-label (if (<= (length (or (plist-get entry :parent-path) nil)) 1)
                           "Section process"
                         "Parent process"))
         (progress
          (cond
           ((> subtask-total 0)
            (list "Subtasks"
                  subtask-total
                  (or (plist-get entry :subtask-done) 0)
                  (or (plist-get entry :subtask-open) 0)
                  (or (plist-get entry :subtask-next) 0)
                  (or (plist-get entry :subtask-wait) 0)
                  (or (plist-get entry :subtask-projects) 0)
                  (or (plist-get entry :subtask-percent) 0)))
           ((> parent-total 0)
            (list parent-label
                  parent-total
                  (or (plist-get entry :parent-subtask-done) 0)
                  (or (plist-get entry :parent-subtask-open) 0)
                  (or (plist-get entry :parent-subtask-next) 0)
                  (or (plist-get entry :parent-subtask-wait) 0)
                  (or (plist-get entry :parent-subtask-projects) 0)
                  (or (plist-get entry :parent-subtask-percent) 0))))))
    (if progress
        (let ((label (nth 0 progress))
              (total (nth 1 progress))
              (done (nth 2 progress))
              (open (nth 3 progress))
              (next-count (nth 4 progress))
              (wait-count (nth 5 progress))
              (project-count (nth 6 progress))
              (percent (nth 7 progress)))
          (format
           "<div class=\"process\"><div class=\"process-head\"><span>%s</span><strong>%d/%d done</strong></div><div class=\"progress-track\" aria-label=\"%s %d percent\"><span style=\"width:%d%%\"></span></div><div class=\"process-badges\">%s</div></div>"
           (my/gtd--html-escape label)
           done
           total
           (my/gtd--html-escape label)
           percent
           percent
           (string-join
            (delq nil
                  (list
                   (format "<span>%d open</span>" open)
                   (when (> next-count 0)
                     (format "<span>%d next</span>" next-count))
                   (when (> wait-count 0)
                     (format "<span>%d wait</span>" wait-count))
                   (when (> project-count 0)
                     (format "<span>%d project%s</span>"
                             project-count
                             (if (= project-count 1) "" "s")))))
            "")))
      "")))

(defun my/gtd--html-child-task (child)
  (let* ((todo (or (plist-get child :todo) "NOTE"))
         (class (downcase todo))
         (depth (min 4 (max 1 (or (plist-get child :depth) 1)))))
    (format
     "<div class=\"subtask depth-%d state-%s\"><span class=\"state\">%s</span><span>%s</span></div>"
     depth
     (my/gtd--html-escape class)
     (my/gtd--html-escape todo)
     (my/gtd--html-escape (plist-get child :title)))))

(defun my/gtd--html-child-tasks (entry &optional open)
  (let ((children (plist-get entry :child-preview))
        (total (or (plist-get entry :subtask-total) 0)))
    (if children
        (format
         "<details class=\"subtasks\"%s><summary><span>Subtasks</span><strong>%d total</strong></summary><div class=\"subtask-list\">%s%s</div></details>"
         (if open " open" "")
         total
         (mapconcat #'my/gtd--html-child-task children "")
         (if (> total (length children))
             (format "<div class=\"subtask-more\">%d more in org</div>"
                     (- total (length children)))
           ""))
      "")))

(defun my/gtd--html-child-search (entry)
  (mapconcat
   (lambda (child)
     (format "%s %s"
             (or (plist-get child :todo) "")
             (or (plist-get child :title) "")))
   (or (plist-get entry :child-preview) nil)
   " "))

(defun my/gtd--html-entry-search (entry)
  (downcase
   (string-join
    (delq nil
          (append (list (plist-get entry :todo)
                        (plist-get entry :title)
                        (string-join (or (plist-get entry :outline-path) nil) " ")
                        (plist-get entry :parent-title)
                        (plist-get entry :section)
                        (my/gtd--entry-area-label entry)
                        (my/gtd--html-child-search entry)
                        (plist-get entry :effort)
                        (format "%s/%s done %s/%s parent"
                                (or (plist-get entry :subtask-done) 0)
                                (or (plist-get entry :subtask-total) 0)
                                (or (plist-get entry :parent-subtask-done) 0)
                                (or (plist-get entry :parent-subtask-total) 0))
                        (plist-get entry :closed))
                  (plist-get entry :tags)))
    " ")))

(defun my/gtd--html-meta (entry &optional show-closed)
  (string-join
   (delq nil
         (list
          (when-let ((path (plist-get entry :parent-path)))
            (unless (null path)
              (format "<span>%s</span>"
                      (my/gtd--html-escape (string-join path " / ")))))
          (when-let ((priority (plist-get entry :priority)))
            (format "<span>Priority %c</span>" priority))
          (when-let ((effort (plist-get entry :effort)))
            (format "<span>Effort %s</span>" (my/gtd--html-escape effort)))
          (when-let ((scheduled (plist-get entry :scheduled)))
            (format "<span>Scheduled %s</span>"
                    (my/gtd--html-escape scheduled)))
          (when (> (or (plist-get entry :subtask-total) 0) 0)
            (format "<span>%d/%d done</span>"
                    (or (plist-get entry :subtask-done) 0)
                    (or (plist-get entry :subtask-total) 0)))
          (when show-closed
            (when-let ((closed (plist-get entry :closed)))
              (format "<span>Closed %s</span>" (my/gtd--html-escape closed))))))
   ""))

(defun my/gtd--html-meta-row (entry &optional show-closed)
  (let ((meta (my/gtd--html-meta entry show-closed)))
    (if (string-empty-p meta)
        ""
      (format "<div class=\"meta\">%s</div>" meta))))

(defun my/gtd--html-entry (entry &optional show-closed)
  (let* ((todo (or (plist-get entry :todo) "NOTE"))
         (class (my/gtd--html-state-class entry))
         (area (my/gtd--entry-area entry))
         (title (my/gtd--html-escape (plist-get entry :title)))
         (progress (my/gtd--html-progress entry))
         (children (my/gtd--html-child-tasks entry))
         (meta (my/gtd--html-meta-row entry show-closed))
         (actions (my/gtd--html-actions entry))
         (search (my/gtd--html-escape (my/gtd--html-entry-search entry))))
    (format
     "<article class=\"item task-row state-%s area-%s\" data-state=\"%s\" data-area=\"%s\" data-search=\"%s\"><span class=\"row-grip\">::</span><span class=\"row-check\" aria-hidden=\"true\"></span><span class=\"row-star\" aria-hidden=\"true\">*</span><div class=\"task-body\"><div class=\"item-main\"><span class=\"state\">%s</span><h3>%s</h3></div>%s%s%s</div>%s</article>\n"
     class
     area
     class
     area
     search
     (my/gtd--html-escape todo)
     title
     meta
     progress
     children
     actions)))

(defun my/gtd--html-section (id title entries empty-text &optional show-closed)
  (format
   "<section id=\"%s\" class=\"task-section\" data-panel><div class=\"section-title\"><h2>%s</h2><span data-count>%d</span></div><div class=\"items\">%s</div></section>\n"
   (my/gtd--html-escape id)
   (my/gtd--html-escape title)
   (length entries)
   (if entries
       (mapconcat (lambda (entry) (my/gtd--html-entry entry show-closed)) entries "")
     (format "<div class=\"empty\">%s</div>" (my/gtd--html-escape empty-text)))))

(defun my/gtd--html-empty-card (title body)
  (format
   "<div class=\"empty-card\"><h2>%s</h2><p>%s</p></div>\n"
   (my/gtd--html-escape title)
   body))

(defun my/gtd--html-entry-block (section-id section-title entries empty-title
                                            empty-body &optional show-closed)
  (if entries
      (my/gtd--html-section section-id section-title entries "" show-closed)
    (my/gtd--html-empty-card empty-title empty-body)))

(defun my/gtd--html-view (id title subtitle entries section-title empty-title
                             empty-body &optional show-closed default-area chips)
  (concat
   "<section id=\"view-" (my/gtd--html-escape id)
   "\" class=\"view-screen\" data-view=\"" (my/gtd--html-escape id)
   "\"><div class=\"view-header\"><h1>" (my/gtd--html-escape title)
   "</h1><p>" subtitle "</p>"
   (or chips "")
   "</div>\n"
   (my/gtd--html-quick-add default-area)
   (my/gtd--html-entry-block
    (concat id "-list")
    section-title
    entries
    empty-title
    empty-body
    show-closed)
   "<div class=\"no-results\" hidden>No matching tasks.</div></section>\n"))

(defun my/gtd--html-area-view (area title entries)
  (my/gtd--html-view
   (concat "project-" area)
   title
   "Project actions"
   entries
   "Next Up"
   (format "%s list is empty" title)
   "Press <kbd>n</kbd> to create a new action."
   nil
   area))

(defun my/gtd--html-next-chips ()
  (concat
   "<div class=\"chip-row\" aria-label=\"Area filters\">"
   (my/gtd--html-area-button "All" "all" t)
   (my/gtd--html-area-button "Work" "work" nil)
   (my/gtd--html-area-button "Part-Time" "parttime" nil)
   (my/gtd--html-area-button "Learn" "learn" nil)
   (my/gtd--html-area-button "Other" "other" nil)
   "</div>"))

(defun my/gtd--html-metric (label value target)
  (format
   "<a class=\"stat\" href=\"#%s\"><span>%s</span><strong>%s</strong></a>\n"
   (my/gtd--html-escape target)
   (my/gtd--html-escape label)
   (my/gtd--html-escape (format "%s" value))))

(defun my/gtd--html-count (count)
  (if (and count (> count 0))
      (format "<strong class=\"rail-count\">%d</strong>" count)
    "<strong class=\"rail-count\"></strong>"))

(defun my/gtd--html-nav-link (label target icon count &optional active)
  (format
   "<a class=\"rail-link%s\" href=\"#%s\" data-nav-target=\"%s\"><span class=\"rail-icon icon-%s\" aria-hidden=\"true\"></span><span class=\"rail-label\">%s</span>%s</a>\n"
   (if active " active" "")
   (my/gtd--html-escape target)
   (my/gtd--html-escape target)
   (my/gtd--html-escape icon)
   (my/gtd--html-escape label)
   (my/gtd--html-count count)))

(defun my/gtd--html-area-nav-link (label area count)
  (format
   "<a class=\"rail-link project-link\" href=\"#project-%s\" data-nav-target=\"project-%s\" data-area-filter=\"%s\"><span class=\"rail-icon icon-project\" aria-hidden=\"true\"></span><span class=\"rail-label\">%s</span>%s</a>\n"
   (my/gtd--html-escape area)
   (my/gtd--html-escape area)
   (my/gtd--html-escape area)
   (my/gtd--html-escape label)
   (my/gtd--html-count count)))

(defun my/gtd--html-filter-button (label state active)
  (format
   "<button class=\"filter%s\" type=\"button\" data-state-filter=\"%s\">%s</button>"
   (if active " active" "")
   (my/gtd--html-escape state)
   (my/gtd--html-escape label)))

(defun my/gtd--html-area-button (label area active)
  (format
   "<button class=\"filter area-filter%s\" type=\"button\" data-area-filter=\"%s\">%s</button>"
   (if active " active" "")
   (my/gtd--html-escape area)
   (my/gtd--html-escape label)))

(defun my/gtd--entries-in-area (entries area)
  (cl-remove-if-not
   (lambda (entry)
     (string= (my/gtd--entry-area entry) area))
   entries))

(defun my/gtd--html-area-card (label area count detail)
  (format
   "<a class=\"area-card area-%s\" href=\"#%s\"><span>%s</span><strong>%d</strong><em>%s</em></a>\n"
   (my/gtd--html-escape area)
   (my/gtd--html-escape area)
   (my/gtd--html-escape label)
   count
   (my/gtd--html-escape detail)))

(defun my/gtd--html-review-stat (label count target)
  (format
   "<a class=\"review-stat\" href=\"#%s\"><span>%s</span><strong>%d</strong></a>\n"
   (my/gtd--html-escape target)
   (my/gtd--html-escape label)
   count))

(defun my/gtd--html-option (value label selected)
  (format "<option value=\"%s\"%s>%s</option>"
          (my/gtd--html-escape value)
          (if (string= value selected) " selected" "")
          (my/gtd--html-escape label)))

(defun my/gtd--html-quick-add (&optional default-area)
  (let ((default-area (or default-area "other")))
    (format
     "<form class=\"rapid-entry\" data-gtd-add-form data-endpoint=\"%s\"><input name=\"title\" type=\"text\" autocomplete=\"off\" placeholder=\"Rapid Entry  -  type here and hit enter / or esc\"><select name=\"area\" aria-label=\"Area\">%s</select><button type=\"submit\">Add</button><p data-add-status hidden></p></form>\n"
     (my/gtd--html-escape (my/gtd--dashboard-action-endpoint))
     (concat
      (my/gtd--html-option "work" "Work" default-area)
      (my/gtd--html-option "parttime" "Part-Time" default-area)
      (my/gtd--html-option "learn" "Learn" default-area)
      (my/gtd--html-option "other" "Other" default-area)))))

(defun my/gtd--html-rail (work-count parttime-count learn-count other-count
                                     explicit-next-count waiting-count stale-count
                                     inbox-count scheduled-count someday-count
                                     updated)
  (concat
   "<aside class=\"nav-rail\" aria-label=\"GTD sections\"><a class=\"brand\" href=\"#next\" data-nav-target=\"next\">ORG GTD</a><nav class=\"rail-nav\">"
   (my/gtd--html-nav-link "Inbox" "inbox" "inbox" inbox-count)
   (my/gtd--html-nav-link "Focus" "focus" "focus" nil)
   (my/gtd--html-nav-link "Next" "next" "next" (+ work-count parttime-count learn-count other-count) t)
   (my/gtd--html-nav-link "Later" "stale" "later" stale-count)
   (my/gtd--html-nav-link "Scheduled" "scheduled" "scheduled" scheduled-count)
   (my/gtd--html-nav-link "Someday" "someday" "someday" someday-count)
   (my/gtd--html-nav-link "Waiting" "waiting" "waiting" waiting-count)
   "<p class=\"rail-heading\">Projects</p>"
   (my/gtd--html-area-nav-link "Work" "work" work-count)
   (my/gtd--html-area-nav-link "Part-Time" "parttime" parttime-count)
   (my/gtd--html-area-nav-link "Learning" "learn" learn-count)
   (my/gtd--html-area-nav-link "Other" "other" other-count)
   "<p class=\"rail-heading\">Tags</p>"
   "</nav><div class=\"rail-footer\"><a href=\"#completed\" data-nav-target=\"completed\">Logbook</a><span>"
   (my/gtd--html-escape updated)
   "</span></div></aside>\n"))

(defun my/gtd--html-lane (id title entries empty-text)
  (format
   "<section id=\"%s\" class=\"task-section lane area-%s\" data-panel><div class=\"section-title\"><h2>%s</h2><span data-count>%d</span></div><div class=\"items\">%s</div></section>\n"
   (my/gtd--html-escape id)
   (my/gtd--html-escape id)
   (my/gtd--html-escape title)
   (length entries)
   (if entries
       (mapconcat (lambda (entry) (my/gtd--html-entry entry)) entries "")
     (format "<div class=\"empty\">%s</div>" (my/gtd--html-escape empty-text)))))

(defun my/gtd--html-focus (entry source-label)
  (if entry
      (format
       "<section class=\"focus panel area-%s\"><div class=\"focus-copy\"><p class=\"kicker\">%s</p>%s<h2>%s</h2>%s%s<div class=\"focus-actions\">%s</div></div><a class=\"source-link\" href=\"%s\" data-gtd-command=\"open\" data-gtd-reload=\"false\">Open</a></section>\n"
       (my/gtd--html-escape (my/gtd--entry-area entry))
       (my/gtd--html-escape source-label)
       (my/gtd--html-outline entry)
       (my/gtd--html-escape (plist-get entry :title))
       (my/gtd--html-progress entry)
       (my/gtd--html-meta-row entry)
       (my/gtd--html-actions entry t)
       (my/gtd--html-escape (my/gtd--dashboard-action-url "open" entry)))
    "<section class=\"focus panel\"><div class=\"focus-copy\"><p class=\"kicker\">Start here</p><h2>No action selected</h2><div class=\"meta\"><span>Mark an item NEXT to make this area decisive.</span></div></div></section>\n"))

(defun my/gtd--dashboard-css ()
  ":root{--bg:#eef2f4;--surface:#fff;--surface-soft:#f7f9fa;--ink:#17201f;--muted:#60706b;--line:#d8e0e3;--work:#23689b;--parttime:#a65d19;--learn:#13745f;--other:#66569a;--done:#1c7a5a;--wait:#7b5fa8;--danger:#a1453f;--shadow:0 18px 40px rgba(29,39,42,.08)}
*{box-sizing:border-box}[hidden]{display:none!important}html,body{max-width:100%;overflow-x:hidden}body{margin:0;background:var(--bg);color:var(--ink);font-family:system-ui,-apple-system,BlinkMacSystemFont,sans-serif;line-height:1.45}a{color:inherit}.page{max-width:1540px;margin:0 auto;padding:28px clamp(18px,4vw,52px) 56px}.app-header{display:flex;align-items:flex-end;justify-content:space-between;gap:24px;margin-bottom:18px}.eyebrow,.kicker{margin:0 0 5px;color:var(--learn);font-size:12px;font-weight:850;text-transform:uppercase;letter-spacing:0}h1{margin:0;font-size:clamp(32px,4vw,52px);line-height:1.02;letter-spacing:0}h2,h3,p{letter-spacing:0}.updated{color:var(--muted);font-size:14px;white-space:nowrap}.panel{background:rgba(255,255,255,.96);border:1px solid var(--line);border-radius:8px;box-shadow:var(--shadow);overflow:hidden}.summary-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px;margin-bottom:18px}.area-card{--accent:var(--other);display:grid;gap:8px;min-height:116px;padding:15px 16px;border:1px solid var(--line);border-top:4px solid var(--accent);border-radius:8px;background:#fff;text-decoration:none;box-shadow:var(--shadow)}.area-card:hover{border-color:color-mix(in srgb,var(--accent) 42%,var(--line))}.area-card span{color:var(--muted);font-size:13px;font-weight:750}.area-card strong{font-size:34px;line-height:1}.area-card em{color:var(--muted);font-size:12px;font-style:normal}.area-work{--accent:var(--work)}.area-parttime{--accent:var(--parttime)}.area-learn{--accent:var(--learn)}.area-other{--accent:var(--other)}.layout{display:grid;grid-template-columns:minmax(0,1fr) 380px;gap:18px;align-items:start}.workbench{display:grid;gap:16px;min-width:0}.review-column{position:sticky;top:18px;display:grid;gap:14px;min-width:0}.focus{--accent:var(--work);display:grid;grid-template-columns:minmax(0,1fr) auto;gap:18px;align-items:center;padding:20px 22px;border-left:4px solid var(--accent);background:linear-gradient(135deg,#fff 0,#f6f9fa 100%)}.focus.area-work{--accent:var(--work)}.focus.area-parttime{--accent:var(--parttime)}.focus.area-learn{--accent:var(--learn)}.focus.area-other{--accent:var(--other)}.focus h2{margin:0;font-size:clamp(22px,3vw,34px);line-height:1.12;overflow-wrap:anywhere}.source-link{display:inline-flex;align-items:center;justify-content:center;min-height:38px;padding:0 14px;border:1px solid var(--line);border-radius:7px;background:#fff;color:var(--work);font-size:14px;font-weight:800;text-decoration:none}.controlbar{display:grid;grid-template-columns:minmax(260px,1fr) auto;gap:14px;align-items:center;padding:12px}.search-wrap{display:flex;align-items:center;gap:10px;min-width:0}.search-wrap label{color:var(--muted);font-size:13px;font-weight:800}input[type=search]{width:100%;min-height:40px;border:1px solid var(--line);border-radius:7px;background:#fff;padding:0 12px;color:var(--ink);font:inherit}input[type=search]:focus{outline:2px solid rgba(35,104,155,.18);border-color:var(--work)}.filter-groups{display:grid;gap:8px;min-width:0}.filters{display:flex;flex-wrap:wrap;justify-content:flex-end;gap:8px;min-width:0}.filter{min-height:34px;border:1px solid var(--line);border-radius:7px;background:#fff;color:var(--muted);padding:0 12px;font:inherit;font-size:13px;font-weight:800;cursor:pointer}.filter.active{background:#1e2a2b;color:#fff;border-color:#1e2a2b}.area-filter.active{background:var(--learn);border-color:var(--learn)}.action-lanes{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px;align-items:start}.lane{--accent:var(--other)}.lane.area-work{--accent:var(--work)}.lane.area-parttime{--accent:var(--parttime)}.lane.area-learn{--accent:var(--learn)}.lane.area-other{--accent:var(--other)}.lane-head,.section-title{display:flex;align-items:center;justify-content:space-between;gap:14px;padding:14px 16px;border-bottom:1px solid var(--line);background:var(--surface-soft)}.lane-head{border-left:4px solid var(--accent);padding-left:12px}.lane-head p{margin:0 0 2px;color:var(--accent);font-size:11px;font-weight:850;text-transform:uppercase;letter-spacing:0}.lane-head h2,.section-title h2{margin:0;font-size:16px}.lane-head span,.section-title span{display:inline-flex;align-items:center;justify-content:center;min-width:28px;height:24px;border-radius:7px;background:#e8eef0;color:var(--muted);font-size:13px;font-weight:850}.items{display:grid}.item{padding:14px 16px;border-bottom:1px solid var(--line);background:#fff}.item:last-child{border-bottom:0}.item-main{display:flex;align-items:flex-start;gap:10px}.state{display:inline-flex;align-items:center;min-height:22px;padding:0 8px;border-radius:7px;font-size:12px;font-weight:850;background:#edf1ec;color:var(--muted);line-height:1}.state-next .state{background:#dff1ea;color:var(--learn)}.state-todo .state{background:#e7eef8;color:var(--work)}.state-proj .state{background:#f7ead8;color:var(--parttime)}.state-wait .state{background:#ece7f5;color:var(--wait)}.state-done .state{background:#e4f0e8;color:var(--done)}.state-cancelled .state{background:#f3e1df;color:var(--danger)}.item h3{margin:1px 0 0;font-size:15px;font-weight:750;line-height:1.34;overflow-wrap:anywhere}.meta{display:flex;flex-wrap:wrap;gap:8px 12px;margin-top:9px;color:var(--muted);font-size:12px}.meta a{color:var(--work);text-decoration:none;font-weight:700}.meta a:hover{text-decoration:underline}.empty,.no-results{padding:18px 16px;color:var(--muted);font-size:14px}.review-summary{padding:14px 16px}.review-summary h2{margin:0 0 12px;font-size:16px}.review-stat-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:8px}.review-stat{display:flex;align-items:center;justify-content:space-between;gap:10px;min-height:42px;padding:0 10px;border:1px solid var(--line);border-radius:7px;background:#fff;text-decoration:none}.review-stat span{color:var(--muted);font-size:12px;font-weight:750}.review-stat strong{font-size:18px}.section-panel .items{max-height:420px;overflow:auto}.foot{margin:18px 0 0;color:var(--muted);font-size:12px;overflow-wrap:anywhere}@media(max-width:1180px){.summary-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.layout{grid-template-columns:1fr}.review-column{position:static;grid-template-columns:repeat(2,minmax(0,1fr))}.review-summary{grid-column:1/-1}}@media(max-width:760px){.page{padding:22px 14px 42px}.app-header{display:block}.updated{margin-top:10px;white-space:normal}.summary-grid{grid-template-columns:repeat(2,minmax(0,1fr));gap:10px}.area-card{min-height:98px;padding:13px}.area-card strong{font-size:28px}.focus{grid-template-columns:1fr;padding:18px}.source-link{width:100%}.controlbar{display:block;width:100%;max-width:100%}.search-wrap{display:grid;width:100%;min-width:0}.filter-groups{margin-top:12px;width:100%}.filters{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));justify-content:stretch;gap:7px}.filter{width:100%;min-width:0;padding:0 8px;font-size:12px}.action-lanes,.review-column{grid-template-columns:1fr}.review-stat-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.item-main{display:grid;gap:7px}.state{width:max-content}}")

(defun my/gtd--dashboard-dark-css ()
  ":root{color-scheme:dark;--bg:#0a0a0a;--surface:#11100f;--surface-soft:#161512;--ink:#f5f1e8;--muted:#a8a097;--line:#2b2925;--work:#d4a27f;--parttime:#e08d86;--learn:#8cc49a;--other:#aba5bb;--done:#b9d7bd;--wait:#c7b8e8;--danger:#e08d86;--shadow:none}
body{background:var(--bg);color:var(--ink);font-family:ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,sans-serif}.simple-page{max-width:1120px;margin:0 auto;padding:28px clamp(16px,4vw,36px) 48px}.app-header{align-items:center;margin-bottom:16px;padding-bottom:16px;border-bottom:1px solid var(--line)}.eyebrow,.kicker{color:#d4a27f}.updated,.search-wrap label,.meta,.empty,.no-results,.review-stat span{color:var(--muted)}h1{font-family:Georgia,\"Times New Roman\",serif;font-size:clamp(34px,6vw,58px);font-weight:600;color:#f7f3ea}.panel{background:#11100f;border-color:var(--line);box-shadow:none}.top-grid{display:grid;grid-template-columns:minmax(0,1fr) 320px;gap:14px;align-items:start}.simple-section{display:grid;gap:12px;margin-top:18px}.simple-section>h2{margin:0;font-size:18px}.focus{background:#11100f;border-color:var(--line);border-left-color:var(--accent);padding:18px 20px}.focus h2{font-family:Georgia,\"Times New Roman\",serif;font-size:clamp(24px,4vw,38px);font-weight:600;color:#f7f3ea}.source-link,input[type=search],.filter,.review-stat,select,.quick-add input{background:#0b0b0a;border-color:#34312d;color:#f5f1e8}.source-link{color:#f0c7a9}.source-link:hover,.filter:hover,.review-stat:hover{background:#171614;border-color:#45413c}input[type=search]::placeholder,.quick-add input::placeholder{color:#706962}input[type=search]:focus,.quick-add input:focus,select:focus{outline:2px solid rgba(212,162,127,.24);border-color:#d4a27f}.quick-add{padding:14px}.quick-add label{color:#d4a27f;font-size:12px;font-weight:850;text-transform:uppercase}.quick-add-row{display:grid;grid-template-columns:minmax(0,1fr) 120px auto;gap:8px;margin-top:8px}.quick-add input,.quick-add select{width:100%;min-height:38px;border:1px solid #34312d;border-radius:8px;padding:0 10px;font:inherit}.quick-add button,.header-action{min-height:34px;border:1px solid #d4a27f;border-radius:8px;background:#d4a27f;color:#090909;padding:0 14px;font:inherit;font-size:13px;font-weight:850;text-decoration:none;cursor:pointer}.quick-add p{margin:6px 0 0;color:var(--muted);font-size:12px}.quick-add p.error{color:var(--danger)}.controlbar{display:grid;grid-template-columns:minmax(180px,1fr) auto;gap:12px;padding:12px}.filters{display:flex;flex-wrap:wrap;gap:7px}.filter{min-height:32px;border-radius:8px}.filter.active{background:#f5f1e8;color:#090909;border-color:#f5f1e8}.area-filter.active{background:#d4a27f;color:#090909;border-color:#d4a27f}.action-lanes{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px}.lane{--accent:var(--other)}.lane.area-work{--accent:var(--work)}.lane.area-parttime{--accent:var(--parttime)}.lane.area-learn{--accent:var(--learn)}.lane.area-other{--accent:var(--other)}.lane-head,.section-title{display:flex;align-items:center;justify-content:space-between;gap:12px;padding:12px 14px;border-bottom:1px solid var(--line);background:#151411}.lane-head{border-left:3px solid var(--accent)}.lane-head h2,.section-title h2{margin:0;font-size:16px}.lane-head span,.section-title span{display:inline-flex;align-items:center;justify-content:center;min-width:26px;height:24px;border-radius:8px;background:#24211d;color:#d8d2c9;font-size:12px;font-weight:850}.item{background:#11100f;border-bottom-color:#272520;padding:13px 14px}.item:hover{background:#151411}.item h3{color:#f5f1e8;font-size:15px}.state{background:#24211d;color:#d8d2c9}.state-todo .state{background:rgba(212,162,127,.16);color:#f0c7a9}.state-next .state{background:rgba(140,196,154,.16);color:#bfe4c8}.state-proj .state{background:rgba(224,141,134,.15);color:#f0b0aa}.state-wait .state{background:rgba(199,184,232,.15);color:#d7c9fb}.state-done .state{background:rgba(185,215,189,.14);color:#cce7cf}.review-stack{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;margin-top:18px}.review-stack .section-panel .items{max-height:360px;overflow:auto}.review-stat-grid{grid-template-columns:repeat(3,minmax(0,1fr))}@media(max-width:900px){.top-grid,.action-lanes,.review-stack{grid-template-columns:1fr}.controlbar{grid-template-columns:1fr}.quick-add-row{grid-template-columns:1fr}.updated{white-space:normal}.app-header{display:block}.header-action{display:inline-flex;align-items:center;margin-top:10px}.filters{display:grid;grid-template-columns:repeat(2,minmax(0,1fr))}.filter{width:100%}}")

(defun my/gtd--dashboard-org-css ()
  ".updated{display:flex;align-items:center;gap:10px}.outline-path{display:flex;flex-wrap:wrap;align-items:center;gap:5px;margin:0 0 8px;color:var(--muted);font-size:12px;line-height:1.35;min-width:0}.outline-path span{min-width:0;overflow-wrap:anywhere}.outline-sep{color:#6f685f}.process{display:grid;gap:6px;margin-top:10px}.process-head{display:flex;align-items:center;justify-content:space-between;gap:10px;color:var(--muted);font-size:12px}.process-head span{font-weight:750}.process-head strong{color:var(--ink);font-size:12px}.progress-track{height:7px;border:1px solid var(--line);border-radius:999px;background:#1c1a17;overflow:hidden}.progress-track span{display:block;height:100%;max-width:100%;border-radius:999px;background:linear-gradient(90deg,var(--accent),#f0c7a9)}.process-badges{display:flex;flex-wrap:wrap;gap:6px}.process-badges span{display:inline-flex;align-items:center;min-height:20px;padding:0 7px;border:1px solid var(--line);border-radius:7px;background:#151412;color:#d8d2c9;font-size:12px}.subtasks{margin-top:11px;border:1px solid var(--line);border-radius:8px;background:#0b0b0a;overflow:hidden}.subtasks summary{display:flex;align-items:center;justify-content:space-between;gap:10px;min-height:32px;padding:0 10px;color:#d8d2c9;font-size:12px;font-weight:800;cursor:pointer}.subtasks summary::marker{color:#d4a27f}.subtask-list{display:grid;border-top:1px solid var(--line)}.subtask{display:grid;grid-template-columns:auto minmax(0,1fr);align-items:start;gap:8px;padding:8px 10px;border-top:1px solid #201e1a;font-size:12px}.subtask:first-child{border-top:0}.subtask.depth-2{padding-left:22px}.subtask.depth-3{padding-left:34px}.subtask.depth-4{padding-left:46px}.subtask span:last-child{overflow-wrap:anywhere;color:#d8d2c9}.subtask-more{padding:8px 10px;color:var(--muted);font-size:12px}.quick-add{display:grid;gap:8px;padding:12px}.quick-add label{color:#d4a27f;font-size:12px;font-weight:850;text-transform:uppercase}.quick-add-row{display:grid;grid-template-columns:minmax(180px,1fr) 132px auto;gap:8px}.quick-add input,.quick-add select{width:100%;min-height:38px;border:1px solid #34312d;border-radius:8px;padding:0 10px;font:inherit}.quick-add button{min-height:38px;border:1px solid #d4a27f;border-radius:8px;background:#d4a27f;color:#090909;padding:0 14px;font:inherit;font-size:13px;font-weight:850;cursor:pointer}.quick-add button:disabled{opacity:.72;cursor:default}.quick-add p{margin:0;color:var(--muted);font-size:12px}.quick-add p.error{color:var(--danger)}.item-actions{display:flex;flex-wrap:wrap;gap:7px;margin-top:11px}.focus-actions .item-actions{margin-top:12px}.task-action,.header-action{display:inline-flex;align-items:center;justify-content:center;min-height:28px;padding:0 10px;border:1px solid #34312d;border-radius:7px;background:#0b0b0a;color:#f5f1e8;font-size:12px;font-weight:850;text-decoration:none}.task-action:hover,.header-action:hover{border-color:#d4a27f;color:#f0c7a9}.task-action.primary,.header-action{background:#d4a27f;border-color:#d4a27f;color:#090909}.task-action.busy,.header-action.busy{opacity:.72;pointer-events:none}.task-action.error,.header-action.error{border-color:var(--danger);color:var(--danger)}.focus .process,.focus .subtasks{max-width:760px}@media(max-width:760px){.updated{display:block}.header-action{margin-top:8px}.process-head{align-items:flex-start}.process-badges span{font-size:11px}.quick-add-row{grid-template-columns:1fr}.item-actions{display:grid;grid-template-columns:repeat(2,minmax(0,1fr))}.task-action{width:100%}}")

(defun my/gtd--dashboard-app-css ()
  ":root{color-scheme:dark;--app-bg:#1f2023;--side:#242529;--side-active:#303136;--row:#242528;--row-alt:#222326;--text:#d7d8dc;--text-strong:#f0f0f2;--muted:#9b9ca3;--faint:#686a72;--line:#303238;--blue:#4b8de8;--yellow:#f3cf4f;--green:#8ecf9b;--pink:#d99ab6}
body{margin:0;background:var(--app-bg);color:var(--text);font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,sans-serif;font-size:15px;line-height:1.45}.gtd-app{display:grid;grid-template-columns:268px minmax(0,1fr);min-height:100vh}.nav-rail{position:sticky;top:0;height:100vh;display:flex;flex-direction:column;background:var(--side);border-right:1px solid #202124}.brand{display:block;padding:17px 18px 20px;color:#d5d6db;text-decoration:none;font-size:20px;letter-spacing:7px;line-height:1;font-weight:500}.rail-nav{display:grid;gap:3px;padding:0 6px}.rail-nav p{margin:24px 12px 12px;color:#bbbcc2;font-size:15px}.rail-link{position:relative;display:grid;grid-template-columns:minmax(0,1fr) auto;align-items:center;min-height:36px;padding:0 14px;border-radius:6px;color:#c7c8ce;text-decoration:none}.rail-link:hover,.rail-link.selected{background:#2b2c31;color:#eeeef1}.rail-link.active{background:var(--side-active);color:#eeeef1}.rail-link.active:before{content:\"\";position:absolute;left:-6px;top:0;bottom:0;width:3px;background:var(--blue);border-radius:0 2px 2px 0}.rail-link span{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.rail-link strong{color:#9fa0a7;font-size:13px;font-weight:700}.rail-footer{margin-top:auto;display:grid;gap:10px;padding:18px;border-top:1px solid #2d2e33;color:#9fa0a7}.rail-footer a{color:#d7d8dc;text-decoration:none}.rail-footer span{font-size:12px;color:#888990}.gtd-main{min-width:0}.topbar{height:52px;display:flex;align-items:center;gap:26px;padding:0 22px}.new-item{display:inline-flex;align-items:center;gap:9px;border:0;background:transparent;color:#d7d8dc;font:inherit;font-weight:700;cursor:pointer}.top-search{display:flex;align-items:center;gap:8px;min-width:280px;color:#d7d8dc}.sr-only{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border:0}.search-icon{position:relative;width:16px;height:16px;flex:0 0 16px;border:2px solid #c6c7cc;border-radius:50%}.search-icon:after{content:\"\";position:absolute;width:7px;height:2px;right:-6px;bottom:-3px;background:#c6c7cc;transform:rotate(45deg);border-radius:2px}.top-search input{width:100%;min-height:36px;border:0;background:transparent;color:#ececef;font:inherit;outline:0}.top-search input::placeholder{color:#b9bac0}.view{max-width:980px;margin:48px 0 80px;padding:0 42px}.view-header h1{margin:0;color:#d9dade;font-size:34px;font-weight:800;letter-spacing:0}.view-header p{margin:2px 0 22px;color:#acaeb5;font-size:16px}.chip-row,.filters{display:flex;flex-wrap:wrap;gap:8px}.filter{min-height:26px;border:0;border-radius:999px;background:#2b2c31;color:#aeb0b7;padding:0 13px;font:inherit;font-size:13px;font-weight:700;cursor:pointer}.filter.active{background:#5a5b61;color:#fff}.rapid-entry{display:grid;grid-template-columns:minmax(0,560px) 118px auto;align-items:center;gap:8px;margin:64px 0 48px}.rapid-entry input{height:38px;border:0;border-bottom:1px solid #5a5b61;background:transparent;color:#e5e5e8;font:inherit;outline:0}.rapid-entry input::placeholder{color:#73747b}.rapid-entry select{height:32px;border:0;border-radius:6px;background:#2b2c31;color:#c8c9cf;padding:0 8px;font:inherit}.rapid-entry button{height:32px;border:0;border-radius:6px;background:#4b8de8;color:#fff;padding:0 12px;font:inherit;font-weight:800;cursor:pointer}.rapid-entry p{grid-column:1/-1;margin:0;color:#9fa0a7;font-size:12px}.task-section{margin-top:30px}.section-title{display:flex;align-items:center;gap:10px;margin:0 0 12px}.section-title h2{margin:0;color:#d7d8dc;font-size:18px;font-weight:800}.section-title span{margin-left:auto;color:#a0a1a8;font-size:13px}.items{background:var(--row);border-radius:4px;overflow:hidden}.task-row{display:grid;grid-template-columns:20px 28px 28px minmax(0,1fr) auto;gap:8px;align-items:start;min-height:42px;padding:10px 14px;border-bottom:1px solid #292a2f;background:var(--row)}.task-row:last-child{border-bottom:0}.task-row:nth-child(even){background:var(--row-alt)}.task-row:hover{background:#292a2f}.row-grip{color:#6d6f76;font-weight:900;letter-spacing:-2px}.row-check{width:18px;height:18px;margin-top:2px;border:2px solid #575961;border-radius:4px}.row-star{margin-top:-1px;color:#5f6168;font-size:23px;line-height:1}.state-next .row-star{color:var(--yellow)}.task-body{min-width:0}.item-main{display:flex;align-items:baseline;gap:9px}.item-main h3{margin:0;color:#e4e5e8;font-size:16px;font-weight:600;line-height:1.35;overflow-wrap:anywhere}.state{display:inline-flex;align-items:center;min-height:20px;padding:0 7px;border-radius:5px;background:#3b3c42;color:#bfc0c7;font-size:11px;font-weight:800}.state-next .state{background:rgba(75,141,232,.18);color:#8cb9ff}.state-todo .state{background:rgba(215,216,220,.12);color:#cfd0d5}.state-proj .state{background:rgba(217,154,182,.18);color:#e9b4ca}.state-wait .state{background:rgba(243,207,79,.16);color:#f1d879}.state-done .state{background:rgba(142,207,155,.16);color:#b8e7c1}.meta{display:flex;flex-wrap:wrap;gap:6px 12px;margin-top:6px;color:#a5a6ad;font-size:12px}.meta span:first-child{color:#9a9ba2}.process{max-width:560px;margin-top:8px}.process-head{display:flex;justify-content:space-between;color:#a8a9af;font-size:12px;font-weight:700}.progress-track{height:5px;margin-top:5px;border-radius:999px;background:#33343a;overflow:hidden}.progress-track span{display:block;height:100%;background:#777982}.process-badges{display:flex;gap:6px;margin-top:6px}.process-badges span{padding:0 7px;border-radius:5px;background:#2d2e33;color:#b8b9bf;font-size:12px}.subtasks{margin-top:9px;border:1px solid #36373d;border-radius:6px;background:#232429}.subtasks summary{display:flex;justify-content:space-between;padding:7px 10px;color:#c9cad0;font-size:13px;cursor:pointer}.subtask{display:grid;grid-template-columns:auto minmax(0,1fr);gap:8px;padding:8px 10px;border-top:1px solid #303137;font-size:13px}.item-actions{display:flex;gap:6px;opacity:.24;transition:opacity .12s}.task-row:hover .item-actions{opacity:1}.task-action{display:inline-flex;align-items:center;justify-content:center;min-height:26px;border:1px solid #3a3b42;border-radius:5px;background:#242529;color:#caccd2;padding:0 8px;font-size:12px;font-weight:800;text-decoration:none}.task-action.primary{background:#e0aa86;border-color:#e0aa86;color:#202124}.task-action.error{border-color:#d66;color:#f99}.empty,.no-results{padding:14px;color:#9ea0a7;background:#242528}.secondary-sections{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:30px;margin-top:42px}.secondary-sections .task-section{margin-top:0}.secondary-sections .items{max-height:360px;overflow:auto}@media(max-width:900px){.gtd-app{grid-template-columns:1fr}.nav-rail{position:static;height:auto}.brand{letter-spacing:5px}.rail-nav{grid-template-columns:repeat(2,minmax(0,1fr));padding-bottom:12px}.rail-nav p{grid-column:1/-1}.view{margin-top:28px;padding:0 16px}.topbar{padding:0 16px;gap:12px}.top-search{min-width:0;flex:1}.rapid-entry{grid-template-columns:1fr;margin:36px 0}.secondary-sections{grid-template-columns:1fr}.task-row{grid-template-columns:18px 24px 24px minmax(0,1fr)}.item-actions{grid-column:4;opacity:1;flex-wrap:wrap}}")

(defun my/gtd--dashboard-nirvana-css ()
  ":root{color-scheme:dark;--app-bg:#1f2021;--side:#25262a;--side-dark:#222326;--active:#303136;--row:#242528;--row-alt:#222326;--text:#d6d7dc;--strong:#ececf0;--muted:#aaaab2;--dim:#767780;--line:#45464d;--soft-line:#2b2c31;--blue:#4b8de8;--yellow:#ffd84d;--later:#7fb2ff;--scheduled:#a9d8bd;--someday:#d8a4bd;--waiting:#d8c99e}
body{margin:0;background:var(--app-bg);color:var(--text);font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,sans-serif;font-size:19px;line-height:1.45;letter-spacing:0}.gtd-app{display:grid;grid-template-columns:330px minmax(0,1fr);min-height:100vh;background:var(--app-bg)}.nav-rail{position:sticky;top:0;height:100vh;background:var(--side);border-right:1px solid #202125;display:flex;flex-direction:column;overflow:auto}.brand{display:block;padding:24px 18px 34px;color:#d7d8dd;text-decoration:none;font-size:24px;font-weight:500;letter-spacing:10px;line-height:1}.rail-nav{display:grid;gap:6px;padding:0 0 18px}.rail-heading{margin:28px 22px 8px;color:#d1d2d7;font-size:20px;font-weight:500}.rail-link{position:relative;display:grid;grid-template-columns:32px minmax(0,1fr) 36px;align-items:center;min-height:44px;margin:0 6px;padding:0 18px;border-radius:6px;color:#d2d3d8;text-decoration:none;font-size:20px}.rail-link:hover,.rail-link.active{background:var(--active);color:#f0f0f2}.rail-link.active:before{content:\"\";position:absolute;left:-6px;top:0;bottom:0;width:4px;background:#2f7dd6;border-radius:0 2px 2px 0}.rail-label{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.rail-count{justify-self:end;color:#9a9ba3;font-size:18px;font-weight:700}.rail-icon{position:relative;width:22px;height:22px;color:#cfd0d5}.rail-icon:before,.rail-icon:after{box-sizing:border-box}.icon-inbox{border:2px solid currentColor;border-radius:3px}.icon-inbox:after{content:\"\";position:absolute;left:4px;right:4px;bottom:4px;height:3px;border:2px solid currentColor;border-top:0;border-radius:0 0 3px 3px}.icon-focus:before{content:\"\\2605\";position:absolute;left:-2px;top:-8px;color:var(--yellow);font-size:31px;line-height:1}.icon-next:before{content:\"\\00BB\";position:absolute;left:-2px;top:-9px;color:#85b6ff;font-size:34px;font-weight:900}.icon-later{width:24px;height:16px;margin-left:0;border:2px solid var(--later);border-left-width:3px;border-radius:3px}.icon-later:after{content:\"\";position:absolute;right:-6px;top:4px;width:8px;height:8px;border-top:2px solid var(--later);border-right:2px solid var(--later);transform:rotate(45deg)}.icon-scheduled{border:2px solid var(--scheduled);border-radius:3px}.icon-scheduled:before{content:\"\";position:absolute;left:3px;right:3px;top:5px;border-top:2px solid var(--scheduled)}.icon-scheduled:after{content:\"\";position:absolute;left:5px;top:-5px;width:3px;height:7px;background:var(--scheduled);box-shadow:9px 0 0 var(--scheduled);border-radius:2px}.icon-someday:before{content:\"\";position:absolute;left:1px;top:8px;width:21px;height:2px;background:var(--someday);transform:rotate(-42deg);border-radius:2px}.icon-someday:after{content:\"\";position:absolute;left:2px;top:3px;width:16px;height:16px;border:2px solid var(--someday);border-right-color:transparent;border-bottom-color:transparent;border-radius:50%;transform:rotate(-20deg)}.icon-waiting:before{content:\"\";position:absolute;left:1px;right:1px;top:4px;height:12px;border:2px solid var(--waiting);border-radius:2px 2px 6px 6px}.icon-waiting:after{content:\"\";position:absolute;left:0;right:0;bottom:1px;border-top:2px solid var(--waiting)}.icon-project:before{content:\"\";position:absolute;left:8px;top:9px;width:6px;height:6px;border:2px solid currentColor;border-radius:50%}.rail-footer{margin-top:auto;display:grid;gap:8px;padding:18px 22px;border-top:1px solid #2d2e33;color:#92939a}.rail-footer a{color:#cfd0d5;text-decoration:none;font-size:17px}.rail-footer span{font-size:12px}.gtd-main{min-width:0}.topbar{height:58px;display:flex;align-items:center;gap:34px;padding:0 40px}.new-item{display:inline-flex;align-items:center;gap:10px;border:0;background:transparent;color:#d7d8dc;font:inherit;font-size:19px;font-weight:800;cursor:pointer}.plus-icon{display:inline-flex;align-items:center;justify-content:center;width:21px;height:21px;border-radius:50%;background:#d7d8dc;color:#222326;font-size:20px;font-weight:900;line-height:1}.caret{width:0;height:0;border-left:6px solid transparent;border-right:6px solid transparent;border-top:6px solid #b8b9bf;margin-left:4px}.top-search{display:flex;align-items:center;gap:12px;min-width:260px;color:#d8d9de;font-weight:800}.search-icon{position:relative;width:20px;height:20px;flex:0 0 20px;border:3px solid #cfd0d5;border-radius:50%}.search-icon:after{content:\"\";position:absolute;width:10px;height:3px;right:-8px;bottom:-5px;background:#cfd0d5;transform:rotate(45deg);border-radius:3px}.top-search input{width:100%;min-height:36px;border:0;background:transparent;color:#ececef;font:inherit;font-size:19px;font-weight:800;outline:0}.top-search input::placeholder{color:#d5d6dc}.view-shell{max-width:1040px;margin:80px 0 100px;padding:0 58px}.view-screen{display:block}.view-screen[hidden]{display:none!important}.view-header h1{margin:0;color:#d9dade;font-size:42px;font-weight:800;line-height:1.05;letter-spacing:0}.view-header p{margin:6px 0 0;color:#b7b8bf;font-size:20px}.chip-row{display:flex;flex-wrap:wrap;gap:8px;margin-top:20px}.filter{min-height:26px;border:0;border-radius:999px;background:#2b2c31;color:#afb0b8;padding:0 13px;font:inherit;font-size:14px;font-weight:800;cursor:pointer}.filter.active{background:#5a5b61;color:#fff}.rapid-entry{display:grid;grid-template-columns:minmax(0,620px) 128px auto;align-items:center;gap:8px;margin:76px 0 74px}.rapid-entry input{height:42px;border:0;border-bottom:2px solid #55565d;background:transparent;color:#e5e5e8;font:inherit;font-size:20px;outline:0}.rapid-entry input::placeholder{color:#696a72}.rapid-entry select{height:34px;border:0;border-radius:6px;background:#2b2c31;color:#c8c9cf;padding:0 8px;font:inherit;font-size:14px}.rapid-entry button{height:34px;border:0;border-radius:6px;background:#4b8de8;color:#fff;padding:0 12px;font:inherit;font-size:14px;font-weight:800;cursor:pointer}.rapid-entry p{grid-column:1/-1;margin:0;color:#9fa0a7;font-size:13px}.empty-card{width:min(720px,100%);margin:0 0 0 0;padding:44px 48px;border:2px solid var(--line);border-radius:7px;background:transparent;color:#cfd0d5}.empty-card h2{margin:0 0 22px;color:#d6d7dc;font-size:23px;font-weight:800}.empty-card p{margin:0;color:#c6c7cd;font-size:20px;line-height:1.55}.empty-card kbd{display:inline-flex;align-items:center;justify-content:center;min-width:38px;height:34px;margin:0 7px;border:2px solid #63646c;border-radius:7px;background:#222326;color:#dadbe0;font:inherit;font-size:18px}.task-section{margin-top:0}.section-title{display:flex;align-items:center;gap:10px;margin:0 0 12px;padding:0;border:0;background:transparent}.section-title h2{margin:0;color:#d7d8dc;font-size:20px;font-weight:800}.section-title span{margin-left:auto;color:#9fa0a8;font-size:14px}.items{background:var(--row);border-radius:4px;overflow:hidden}.task-row{display:grid;grid-template-columns:20px 28px 28px minmax(0,1fr) auto;gap:8px;align-items:start;min-height:46px;padding:10px 14px;border-bottom:1px solid #292a2f;background:var(--row)}.task-row:nth-child(even){background:var(--row-alt)}.task-row:hover{background:#292a2f}.row-grip{color:#6d6f76;font-size:18px;font-weight:900;letter-spacing:-2px}.row-check{width:19px;height:19px;margin-top:2px;border:2px solid #575961;border-radius:4px}.row-star{margin-top:-2px;color:#5f6168;font-size:26px;line-height:1}.state-next .row-star{color:var(--yellow)}.item-main{display:flex;align-items:baseline;gap:9px}.item-main h3{margin:0;color:#e4e5e8;font-size:20px;font-weight:600;line-height:1.35;overflow-wrap:anywhere}.state{display:inline-flex;align-items:center;min-height:20px;padding:0 7px;border-radius:5px;background:#3b3c42;color:#bfc0c7;font-size:11px;font-weight:800}.meta{display:flex;flex-wrap:wrap;gap:6px 12px;margin-top:6px;color:#a5a6ad;font-size:13px}.process{max-width:560px;margin-top:9px}.process-head{display:flex;justify-content:space-between;color:#a8a9af;font-size:13px;font-weight:700}.progress-track{height:5px;margin-top:5px;border-radius:999px;background:#33343a;overflow:hidden}.progress-track span{display:block;height:100%;background:#777982}.process-badges{display:flex;gap:6px;margin-top:6px}.process-badges span{padding:0 7px;border-radius:5px;background:#2d2e33;color:#b8b9bf;font-size:12px}.subtasks{margin-top:10px;border-left:4px solid #55565d;border-top:0;border-right:0;border-bottom:0;border-radius:0;background:transparent}.subtasks summary{padding:7px 12px;color:#c9cad0;font-size:15px;cursor:pointer}.subtask{display:grid;grid-template-columns:auto minmax(0,1fr);gap:8px;padding:7px 12px;border-top:0;font-size:15px}.item-actions{display:flex;gap:6px;opacity:.12;transition:opacity .12s}.task-row:hover .item-actions{opacity:1}.task-action{display:inline-flex;align-items:center;justify-content:center;min-height:26px;border:1px solid #3a3b42;border-radius:5px;background:#242529;color:#caccd2;padding:0 8px;font-size:12px;font-weight:800;text-decoration:none}.task-action.primary{background:#e0aa86;border-color:#e0aa86;color:#202124}.no-results{margin-top:16px;padding:14px;color:#9ea0a7;background:#242528;border-radius:4px}@media(max-width:900px){.gtd-app{grid-template-columns:1fr}.nav-rail{position:static;height:auto}.brand{letter-spacing:7px}.rail-nav{grid-template-columns:1fr}.view-shell{margin:36px 0 72px;padding:0 18px}.topbar{padding:0 18px;gap:16px}.top-search{min-width:0;flex:1}.rapid-entry{grid-template-columns:1fr;margin:40px 0 42px}.task-row{grid-template-columns:18px 24px 24px minmax(0,1fr)}.item-actions{grid-column:4;opacity:1;flex-wrap:wrap}.empty-card{padding:30px 28px}}")

(defun my/gtd--dashboard-polish-css ()
  ".view-header h1{font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,sans-serif}.rapid-entry select,.rapid-entry button{opacity:0;pointer-events:none;transition:opacity .12s}.rapid-entry:focus-within select,.rapid-entry:focus-within button{opacity:1;pointer-events:auto}.item-actions{opacity:0}.task-row:hover .item-actions,.task-row:focus-within .item-actions{opacity:1}@media(min-width:761px) and (max-width:900px){.gtd-app{grid-template-columns:330px minmax(0,1fr)}.nav-rail{position:sticky;top:0;height:100vh}.rail-nav{grid-template-columns:1fr}.view-shell{margin:80px 0 100px;padding:0 58px}.topbar{padding:0 40px;gap:34px}.rapid-entry{grid-template-columns:minmax(0,620px) 128px auto;margin:76px 0 74px}.rapid-entry select,.rapid-entry button{opacity:0;pointer-events:none}.rapid-entry:focus-within select,.rapid-entry:focus-within button{opacity:1;pointer-events:auto}.item-actions{grid-column:auto;opacity:0}.task-row:hover .item-actions,.task-row:focus-within .item-actions{opacity:1}.empty-card{padding:44px 48px}}@media(max-width:760px){.gtd-app{grid-template-columns:1fr}.nav-rail{position:static;height:auto}.brand{letter-spacing:7px}.rail-nav{grid-template-columns:1fr}.view-shell{margin:36px 0 72px;padding:0 18px}.topbar{padding:0 18px;gap:16px}.top-search{min-width:0;flex:1}.rapid-entry{grid-template-columns:1fr;margin:40px 0 42px}.rapid-entry select,.rapid-entry button{opacity:1;pointer-events:auto}.task-row{grid-template-columns:18px 24px 24px minmax(0,1fr)}.item-actions{grid-column:4;opacity:1;flex-wrap:wrap}.empty-card{padding:30px 28px}}")

(defun my/gtd--dashboard-js ()
  "(function(){
var search=document.getElementById(\"task-search\");
var areaFilters=[].slice.call(document.querySelectorAll(\"[data-area-filter]\"));
var navLinks=[].slice.call(document.querySelectorAll(\"[data-nav-target]\"));
var currentView=\"next\";
var activeArea=\"all\";
function activeScreen(){
  return document.querySelector(\"[data-view='\"+currentView+\"']\");
}
function setView(view,replace){
  if(!document.querySelector(\"[data-view='\"+view+\"']\")){view=\"next\";}
  currentView=view;
  activeArea=\"all\";
  document.querySelectorAll(\"[data-view]\").forEach(function(screen){
    screen.hidden=screen.dataset.view!==view;
  });
  navLinks.forEach(function(link){
    link.classList.toggle(\"active\",link.dataset.navTarget===view);
  });
  areaFilters.forEach(function(button){
    button.classList.toggle(\"active\",button.dataset.areaFilter===\"all\");
  });
  if(replace){history.replaceState(null,\"\",\"#\"+view);}
  else{history.pushState(null,\"\",\"#\"+view);}
  apply();
}
function apply(){
  var screen=activeScreen();
  if(!screen){return;}
  var q=(search&&search.value||\"\").trim().toLowerCase();
  var shown=0;
  screen.querySelectorAll(\".item\").forEach(function(item){
    var area=item.dataset.area||\"\";
    var text=item.dataset.search||\"\";
    var useArea=currentView===\"next\"&&activeArea!==\"all\";
    var okText=!q||text.indexOf(q)!==-1;
    var okArea=!useArea||area===activeArea;
    var show=okText&&okArea;
    item.hidden=!show;
    if(show){shown+=1;}
  });
  screen.querySelectorAll(\"[data-panel]\").forEach(function(panel){
    var count=panel.querySelector(\"[data-count]\");
    if(count){count.textContent=panel.querySelectorAll(\".item:not([hidden])\").length;}
  });
  screen.querySelectorAll(\".no-results\").forEach(function(node){
    node.hidden=!q||shown>0;
  });
}
function wireNav(){
  navLinks.forEach(function(link){
    link.addEventListener(\"click\",function(event){
      event.preventDefault();
      setView(link.dataset.navTarget||\"next\");
    });
  });
  window.addEventListener(\"popstate\",function(){
    setView((location.hash||\"#next\").slice(1),true);
  });
}
function wireAreaFilters(){
  areaFilters.forEach(function(button){
    button.addEventListener(\"click\",function(){
      activeArea=button.dataset.areaFilter||\"all\";
      areaFilters.forEach(function(other){
        other.classList.toggle(\"active\",other===button);
      });
      apply();
    });
  });
}
function wireCommands(){
  document.querySelectorAll(\"[data-gtd-command]\").forEach(function(link){
    link.addEventListener(\"click\",function(event){
      event.preventDefault();
      if(link.classList.contains(\"busy\")){return;}
      var original=link.dataset.originalLabel||link.textContent;
      link.dataset.originalLabel=original;
      link.classList.remove(\"error\");
      link.classList.add(\"busy\");
      link.textContent=link.dataset.gtdReload===\"true\"?\"Sent\":\"Opening\";
      fetch(link.href,{method:\"GET\",cache:\"no-store\"})
        .then(function(response){
          return response.json().then(function(data){
            if(!response.ok||!data.ok){throw new Error(data.message||\"Action failed\");}
            return data;
          });
        })
        .then(function(){
          if(link.dataset.gtdReload===\"true\"){
            window.setTimeout(function(){window.location.reload();},350);
          }else{
            link.textContent=\"Opened\";
            window.setTimeout(function(){
              link.classList.remove(\"busy\");
              link.textContent=original;
            },900);
          }
        })
        .catch(function(error){
          link.classList.add(\"error\");
          link.classList.remove(\"busy\");
          link.textContent=\"Failed\";
          link.title=error.message;
          window.setTimeout(function(){link.textContent=original;},1800);
        });
    });
  });
}
function focusRapid(){
  var screen=activeScreen();
  var title=screen&&screen.querySelector(\"[data-gtd-add-form] [name=title]\");
  if(title){title.focus();}
}
function wireAddForms(){
  var focusButton=document.querySelector(\"[data-focus-rapid]\");
  if(focusButton){focusButton.addEventListener(\"click\",focusRapid);}
  document.addEventListener(\"keydown\",function(event){
    var tag=(event.target&&event.target.tagName||\"\").toLowerCase();
    if(event.key===\"n\"&&tag!==\"input\"&&tag!==\"textarea\"&&tag!==\"select\"){
      event.preventDefault();
      focusRapid();
    }
  });
  document.querySelectorAll(\"[data-gtd-add-form]\").forEach(function(form){
    var title=form.querySelector(\"[name=title]\");
    var status=form.querySelector(\"[data-add-status]\");
    if(title){
      title.addEventListener(\"keydown\",function(event){
        if(event.key===\"Escape\"){title.value=\"\";title.blur();}
      });
    }
    form.addEventListener(\"submit\",function(event){
      event.preventDefault();
      var value=(title&&title.value||\"\").trim();
      if(!value){if(title){title.focus();}return;}
      var button=form.querySelector(\"button\");
      var params=new URLSearchParams(new FormData(form));
      params.set(\"action\",\"add\");
      if(button){button.disabled=true;button.textContent=\"Adding\";}
      if(status){status.hidden=false;status.textContent=\"Saving\";status.className=\"\";}
      fetch(form.dataset.endpoint+\"?\"+params.toString(),{method:\"GET\",cache:\"no-store\"})
        .then(function(response){
          return response.json().then(function(data){
            if(!response.ok||!data.ok){throw new Error(data.message||\"Add failed\");}
            return data;
          });
        })
        .then(function(){
          if(status){status.textContent=\"Saved\";}
          window.setTimeout(function(){window.location.reload();},350);
        })
        .catch(function(error){
          if(button){button.disabled=false;button.textContent=\"Add\";}
          if(status){status.textContent=error.message;status.className=\"error\";}
        });
    });
  });
}
if(search){search.addEventListener(\"input\",apply);}
wireNav();
wireAreaFilters();
wireCommands();
wireAddForms();
setView((location.hash||\"#next\").slice(1),true);
})();")

(defun my/gtd--dashboard-html (days)
  (let* ((groups (my/gtd--current-action-groups))
         (completed (my/gtd--completed-since days))
         (updated (format-time-string "%Y-%m-%d %a %H:%M"))
         (next (plist-get groups :next))
         (candidates (plist-get groups :candidates))
         (projects-missing-next (plist-get groups :projects-missing-next))
         (waiting (plist-get groups :waiting))
         (inbox (plist-get groups :inbox))
         (scheduled (plist-get groups :scheduled))
         (someday (plist-get groups :someday))
         (stale (plist-get groups :stale))
         (actions (my/gtd--sort-actions (append next candidates)))
         (work-actions (my/gtd--entries-in-area actions "work"))
         (parttime-actions (my/gtd--entries-in-area actions "parttime"))
         (learn-actions (my/gtd--entries-in-area actions "learn"))
         (other-actions (my/gtd--entries-in-area actions "other"))
         (all-actions (my/gtd--sort-actions actions)))
    (concat
     "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
     "<title>GTD Workbench</title>\n"
     "<style>\n"
     (my/gtd--dashboard-css)
     "\n"
     (my/gtd--dashboard-dark-css)
     "\n"
     (my/gtd--dashboard-org-css)
     "\n"
     (my/gtd--dashboard-app-css)
     "\n"
     (my/gtd--dashboard-nirvana-css)
     "\n"
     (my/gtd--dashboard-polish-css)
     "\n</style>\n</head>\n<body>\n<div class=\"gtd-app\">\n"
     (my/gtd--html-rail
      (length work-actions)
      (length parttime-actions)
      (length learn-actions)
      (length other-actions)
      (length next)
      (length waiting)
      (length stale)
      (length inbox)
      (length scheduled)
      (length someday)
      updated)
     "<main id=\"top\" class=\"gtd-main\"><header class=\"topbar\"><button class=\"new-item\" type=\"button\" data-focus-rapid><span class=\"plus-icon\" aria-hidden=\"true\">+</span><span>New Item</span><span class=\"caret\" aria-hidden=\"true\"></span></button><label class=\"top-search\" for=\"task-search\"><span class=\"search-icon\" aria-hidden=\"true\"></span><span class=\"sr-only\">Search</span><input id=\"task-search\" type=\"search\" placeholder=\"Search\"></label></header><section class=\"view-shell\">\n"
     (my/gtd--html-view
      "inbox"
      "Inbox"
      "New / unprocessed to-do's"
      inbox
      "Inbox"
      "Your Inbox is empty"
      "Press <kbd>n</kbd> to create a new action."
      nil
      "other")
     (my/gtd--html-view
      "focus"
      "Focus"
      "Today"
      next
      "Focus"
      "Focus list is empty"
      "Press <kbd>n</kbd> to create a new action."
      nil
      "other")
     (my/gtd--html-view
      "next"
      "Next"
      "To-do's for anytime"
      all-actions
      "Actions"
      "Next list is empty"
      "Press <kbd>n</kbd> to create a new action."
      nil
      "other"
      (my/gtd--html-next-chips))
     (my/gtd--html-view
      "stale"
      "Later"
      "To-do's for later"
      stale
      "Later"
      "Later list is empty"
      "Press <kbd>n</kbd> to create a new action."
      nil
      "other")
     (my/gtd--html-view
      "scheduled"
      "Scheduled"
      "... for a future date"
      scheduled
      "Scheduled"
      "You have nothing Scheduled"
      "Press <kbd>n</kbd> to create a new Scheduled action."
      nil
      "other")
     (my/gtd--html-view
      "someday"
      "Someday"
      "Maybe"
      someday
      "Someday"
      "Someday list is empty"
      "Press <kbd>n</kbd> to capture a maybe item."
      nil
      "other")
     (my/gtd--html-view
      "waiting"
      "Waiting"
      "... for someone / on hold"
      waiting
      "Waiting"
      "You are not currently Waiting on anyone"
      "Press <kbd>n</kbd> to create a new action."
      nil
      "other")
     (my/gtd--html-area-view "work" "Work" work-actions)
     (my/gtd--html-area-view "parttime" "Part-Time" parttime-actions)
     (my/gtd--html-area-view "learn" "Learning" learn-actions)
     (my/gtd--html-area-view "other" "Other" other-actions)
     (my/gtd--html-view
      "completed"
      "Logbook"
      (format "Done in the last %d days" days)
      completed
      (format "Logbook %dd" days)
      "No completed tasks found"
      "Completed actions will appear here after you mark them done."
      t
      "other")
     "</section></main></div><script>\n"
     (my/gtd--dashboard-js)
     "\n"
     "</script>\n</body>\n</html>\n")))

(defun my/gtd-export-dashboard-html (&optional days)
  "Write the local GTD HTML dashboard and return its path."
  (interactive
   (list (if current-prefix-arg
             (read-number "Dashboard days: " my/gtd-review-days)
           my/gtd-review-days)))
  (my/gtd-ensure-headings)
  (unless noninteractive
    (condition-case err
        (my/gtd-action-server-start)
      (error (message "GTD action server unavailable: %s"
                      (error-message-string err)))))
  (let ((file (expand-file-name my/gtd-dashboard-html-file))
        (days (or days my/gtd-review-days)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (my/gtd--dashboard-html days)))
    (message "Wrote GTD dashboard: %s" file)
    file))

(defun my/gtd-open-dashboard-html (&optional days)
  "Refresh and open the local GTD HTML dashboard."
  (interactive
   (list (if current-prefix-arg
             (read-number "Dashboard days: " my/gtd-review-days)
           my/gtd-review-days)))
  (browse-url-of-file (my/gtd-export-dashboard-html days)))

(defun my/gtd-cleanup-current ()
  "Refresh GTD headings and statistics without archiving."
  (interactive)
  (my/gtd-ensure-headings)
  (with-current-buffer (find-file-noselect org-current-file)
    (when (fboundp 'my/org-safe-update-checkboxes)
      (my/org-safe-update-checkboxes))
    (save-buffer))
  (message "GTD cleanup complete"))

(defun my/gtd-archive-done-tasks ()
  "Archive DONE and CANCELLED entries from the current GTD file after confirmation."
  (interactive)
  (my/gtd--ensure-org)
  (my/gtd-ensure-headings)
  (with-current-buffer (find-file-noselect org-current-file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (markers)
          (while (re-search-forward "^\\*+ \\(DONE\\|CANCELLED\\) " nil t)
            (push (copy-marker (line-beginning-position)) markers))
          (cond
           ((null markers)
            (message "No completed GTD entries to archive"))
           ((yes-or-no-p (format "Archive %d completed GTD entries? " (length markers)))
            (dolist (marker markers)
              (goto-char marker)
              (org-archive-subtree))
            (save-buffer)
            (message "Archived %d completed GTD entries" (length markers)))))))))

(defvar my/gtd-web-app-directory
  (expand-file-name "gtd-web"
                    (file-name-directory
                     (or load-file-name
                         (buffer-file-name)
                         default-directory))))
(defvar my/gtd-web-host "127.0.0.1")
(defvar my/gtd-web-port 8787)
(defvar my/gtd-web-process nil)

(defun my/gtd-web-url ()
  (format "http://%s:%d" my/gtd-web-host my/gtd-web-port))

(defun my/gtd-web-start ()
  "Start the local JavaScript GTD web app."
  (interactive)
  (unless (and my/gtd-web-process
               (process-live-p my/gtd-web-process))
    (let ((node (or (executable-find "node")
                    (user-error "Node.js is required for the GTD web app")))
          (default-directory my/gtd-web-app-directory)
          (process-environment
           (append
            (list
             (format "GTD_HOST=%s" my/gtd-web-host)
             (format "GTD_PORT=%d" my/gtd-web-port)
             (format "GTD_CURRENT_FILE=%s" (expand-file-name org-current-file))
             (format "GTD_ARCHIVE_FILE=%s" (expand-file-name org-archive-file))
             (format "GTD_REVIEW_DAYS=%d" my/gtd-review-days)
             (format "GTD_STALE_DAYS=%d" my/gtd-stale-days))
            process-environment)))
      (unless (file-exists-p (expand-file-name "server.mjs" my/gtd-web-app-directory))
        (user-error "Missing GTD web app server: %s" my/gtd-web-app-directory))
      (setq my/gtd-web-process
            (start-process "gtd-web" "*GTD Web*" node "server.mjs"))
      (set-process-query-on-exit-flag my/gtd-web-process nil)))
  (message "GTD web app running at %s" (my/gtd-web-url))
  my/gtd-web-process)

(defun my/gtd-web-open ()
  "Start and open the local JavaScript GTD web app."
  (interactive)
  (my/gtd-web-start)
  (browse-url (my/gtd-web-url)))

(defun my/gtd-web-stop ()
  "Stop the local JavaScript GTD web app."
  (interactive)
  (when (and my/gtd-web-process
             (process-live-p my/gtd-web-process))
    (delete-process my/gtd-web-process)
    (setq my/gtd-web-process nil)
    (message "GTD web app stopped")))

;;; === macOS ===

(when (file-directory-p "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))

;;; === Blog helpers ===

(defun blog/new-post (title)
  "Create a new blog post with TITLE."
  (interactive "sPost title: ")
  (let ((filename (concat blog-posts-directory
                          (format-time-string "%Y-%m-%d-")
                          (replace-regexp-in-string "[^a-zA-Z0-9]+" "-"
                                                    (downcase title))
                          ".org")))
    (find-file filename)
    (insert-file-contents (concat blog-templates-directory "post-template.org"))
    (goto-char (point-min))
    (search-forward "#+TITLE:")
    (insert (concat " " title))
    (search-forward "#+DATE:")
    (insert (concat " " (format-time-string "<%Y-%m-%d %a>")))
    (search-forward "#+AUTHOR:")
    (insert (concat " " (or user-full-name "Your Name")))
    (search-forward "#+LASTMOD:")
    (insert (concat " " (format-time-string "<%Y-%m-%d %a>")))
    (goto-char (point-min))
    (search-forward "#+FILETAGS:")
    (insert " ")
    (end-of-line)))

(defun blog/publish ()
  "Publish the blog using build.sh."
  (interactive)
  (let ((default-directory blog-directory))
    (compile "./build.sh")))

(defun blog/update-lastmod ()
  "Update the #+LASTMOD: date in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+LASTMOD:.*$" nil t)
      (replace-match (format "#+LASTMOD: %s" (format-time-string "<%Y-%m-%d %a>"))))))

(add-hook 'before-save-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-prefix-p blog-posts-directory (buffer-file-name))
                       (string-suffix-p ".org" (buffer-file-name)))
              (blog/update-lastmod))))

;;; === Keybindings ===

(map! :leader
      ;; Blog
      :desc "New blog post" "B n" #'blog/new-post
      :desc "Publish blog" "B p" #'blog/publish

      ;; GTD
      :desc "Capture" "g c" #'my/gtd-capture
      :desc "Agenda" "g a" #'org-agenda
      :desc "Archive done" "g A" #'org-archive-subtree
      :desc "View current work" "g w" (lambda () (interactive) (find-file org-current-file))
      :desc "View archive" "g v" (lambda () (interactive) (find-file org-archive-file))
      :desc "GTD dashboard" "g G" #'my/gtd-dashboard
      :desc "Open GTD web app" "g h" #'my/gtd-web-open
      :desc "Stop GTD web app" "g q" #'my/gtd-web-stop
      :desc "Export HTML dashboard" "g H" #'my/gtd-export-dashboard-html
      :desc "Next actions" "g n" #'my/gtd-next-actions
      :desc "Mark NEXT" "g N" #'my/gtd-mark-next-task
      :desc "Weekly review" "g r" #'my/gtd-weekly-review
      :desc "Add task" "g t" #'my/gtd-add-task
      :desc "Add deep work task" "g d" #'my/gtd-add-deep-work-task
      :desc "Add shallow task" "g s" #'my/gtd-add-shallow-task
      :desc "Add learning task" "g l" #'my/gtd-add-learning-task
      :desc "Complete task" "g x" #'my/gtd-complete-task
      :desc "Cleanup GTD" "g C" #'my/gtd-cleanup-current
      :desc "Archive completed GTD" "g X" #'my/gtd-archive-done-tasks
      :desc "Insert checkbox" "g b" (lambda () (interactive)
                                      (end-of-line)
                                      (newline-and-indent)
                                      (insert "- [ ] "))
      :desc "Update statistics" "g u" #'my/force-org-checkbox-statistics-update

      ;; Knowledge Base
      :desc "Browse wiki" "k k" (lambda () (interactive) (dired org-roam-directory))
      :desc "New raw file" "k n" (lambda () (interactive)
                                   (find-file (read-file-name "New raw file: "
                                                              (concat blog-directory "kb/raw/"))))
      :desc "Roam find node" "k f" #'org-roam-node-find
      :desc "Roam graph" "k g" #'org-roam-ui-open)

;;; === Markdown preview ===

(after! markdown-mode
  (defun my/markdown-pandoc-new-syntax-highlighting-p ()
    "Return non-nil when installed pandoc expects `--syntax-highlighting'."
    (when-let* ((version-line (ignore-errors (car (process-lines "pandoc" "--version"))))
                (version (and (string-match "\\([0-9]+\\(?:\\.[0-9]+\\)+\\)" version-line)
                              (match-string 1 version-line))))
      (version<= "3.0" version)))

  (defun my/markdown-pandoc-input-format ()
    "Return the best pandoc input format for the current Markdown buffer."
    (if (derived-mode-p 'gfm-mode)
        "gfm+tex_math_dollars"
      "markdown+tex_math_dollars"))

  (advice-add '+markdown-compile-pandoc :override
              (lambda (beg end output-buffer)
                (when (executable-find "pandoc")
                  (apply #'call-process-region
                         beg end "pandoc" nil output-buffer nil
                         (append (list "-f" (my/markdown-pandoc-input-format)
                                       "-t" "html5"
                                       "--standalone"
                                       "--metadata=title:Markdown Preview"
                                       "--mathjax")
                                 (if (my/markdown-pandoc-new-syntax-highlighting-p)
                                     '("--syntax-highlighting=pygments")
                                   '("--highlight-style=pygments")))))))

  (require 'shr)

  (defvar-local my/markdown-preview-buffer nil)
  (defvar-local my/markdown-preview-source-buffer nil)

  (defvar my/markdown-rendered-preview-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map special-mode-map)
      (define-key map (kbd "g") #'my/markdown-preview-refresh)
      map))

  (define-derived-mode my/markdown-rendered-preview-mode special-mode "Markdown Preview"
    "Major mode for rendered Markdown previews inside Emacs.")

  (defun my/markdown-preview--window-width (preview-buffer)
    (let ((window (get-buffer-window preview-buffer)))
      (max 72 (min 100 (if window (- (window-body-width window) 4) 90)))))

  (defun my/markdown-preview--position-ratio (position)
    (if (<= (buffer-size) 1) 0.0
      (/ (float (- position (point-min)))
         (float (max 1 (- (point-max) (point-min)))))))

  (defun my/markdown-preview--ratio-position (ratio)
    (+ (point-min)
       (round (* ratio (max 1 (- (point-max) (point-min)))))))

  (defun my/markdown-preview--capture-window-state (preview-buffer)
    (with-current-buffer preview-buffer
      (mapcar (lambda (window)
                (list window
                      (my/markdown-preview--position-ratio (window-point window))
                      (my/markdown-preview--position-ratio (window-start window))))
              (get-buffer-window-list preview-buffer nil t))))

  (defun my/markdown-preview--restore-window-state (window-state)
    (dolist (entry window-state)
      (pcase-let ((`(,window ,point-ratio ,start-ratio) entry))
        (when (window-live-p window)
          (with-selected-window window
            (set-window-point window
                              (my/markdown-preview--ratio-position point-ratio))
            (set-window-start window
                              (my/markdown-preview--ratio-position start-ratio)
                              t))))))

  (defun my/markdown-preview--html (source-buffer)
    (with-current-buffer source-buffer
      (let ((output-buffer (generate-new-buffer " *markdown-preview-html*")))
        (unwind-protect
            (progn
              (+markdown-compile (point-min) (point-max) output-buffer)
              (with-current-buffer output-buffer (buffer-string)))
          (kill-buffer output-buffer)))))

  (defun my/markdown-preview--render (source-buffer preview-buffer)
    (let ((html (my/markdown-preview--html source-buffer)))
      (with-current-buffer preview-buffer
        (let ((inhibit-read-only t))
          (my/markdown-rendered-preview-mode)
          (setq-local my/markdown-preview-source-buffer source-buffer)
          (setq-local cursor-type nil)
          (setq-local truncate-lines nil)
          (setq-local mode-line-format nil)
          (setq-local header-line-format "Markdown preview  g refresh  q quit")
          (setq-local default-directory
                      (with-current-buffer source-buffer default-directory))
          (erase-buffer)
          (insert html)
          (let ((dom (libxml-parse-html-region (point-min) (point-max)))
                (shr-use-fonts nil)
                (shr-use-colors nil)
                (shr-inhibit-images nil)
                (shr-max-image-proportion 0.5)
                (shr-width (my/markdown-preview--window-width preview-buffer)))
            (erase-buffer)
            (shr-insert-document dom))
          (visual-line-mode 1)
          (goto-char (point-min))))))

  (defun my/markdown-preview-refresh ()
    "Refresh the rendered Markdown preview."
    (interactive)
    (let* ((source-buffer
            (cond ((derived-mode-p 'markdown-mode 'gfm-mode) (current-buffer))
                  ((buffer-live-p my/markdown-preview-source-buffer)
                   my/markdown-preview-source-buffer)
                  (t (user-error "Not in a Markdown buffer or preview buffer"))))
           (preview-buffer
            (or (buffer-local-value 'my/markdown-preview-buffer source-buffer)
                (get-buffer-create
                 (format "*markdown-preview: %s*" (buffer-name source-buffer)))))
           (window-state
            (when (buffer-live-p preview-buffer)
              (my/markdown-preview--capture-window-state preview-buffer))))
      (with-current-buffer source-buffer
        (setq-local my/markdown-preview-buffer preview-buffer))
      (my/markdown-preview--render source-buffer preview-buffer)
      (with-current-buffer preview-buffer
        (when window-state
          (my/markdown-preview--restore-window-state window-state)))
      preview-buffer))

  (defun my/markdown-preview-in-emacs ()
    "Open a rendered Markdown preview in another Emacs window."
    (interactive)
    (display-buffer (my/markdown-preview-refresh)))

  (defun my/markdown-preview-refresh-maybe ()
    (when (and (derived-mode-p 'markdown-mode 'gfm-mode)
               (buffer-live-p my/markdown-preview-buffer))
      (my/markdown-preview-refresh)))

  (defun my/markdown-preview-cleanup ()
    (when (buffer-live-p my/markdown-preview-buffer)
      (kill-buffer my/markdown-preview-buffer))
    (setq my/markdown-preview-buffer nil))

  (setq markdown-asymmetric-header t
        markdown-enable-math t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-use-electric-backquote t)

  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

  (dolist (hook '(markdown-mode-hook gfm-mode-hook))
    (add-hook hook #'visual-line-mode)
    (add-hook hook (lambda ()
                     (add-hook 'after-save-hook #'my/markdown-preview-refresh-maybe nil t)
                     (add-hook 'kill-buffer-hook #'my/markdown-preview-cleanup nil t))))

  (map! :map markdown-mode-map
        :localleader
        :desc "Preview in Emacs" "p" #'my/markdown-preview-in-emacs
        :desc "Refresh preview" "r" #'my/markdown-preview-refresh
        :desc "Preview in browser" "P" #'markdown-preview)
  (map! :map gfm-mode-map
        :localleader
        :desc "Preview in Emacs" "p" #'my/markdown-preview-in-emacs
        :desc "Refresh preview" "r" #'my/markdown-preview-refresh
        :desc "Preview in browser" "P" #'markdown-preview))

;;; === Org / GTD ===

(after! org
  (require 'ox-publish)

  ;; GTD todo states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAIT(w@/!)" "|"
                    "DONE(d)" "CANCELLED(c)")))
  (setq org-agenda-files (list org-current-file))
  (setq org-archive-location (concat org-archive-file "::* Archived %s"))
  (setq org-log-done 'time)
  (setq org-hierarchical-todo-statistics nil)
  (setq org-refile-targets '((org-current-file :maxlevel . 3)))

  ;; Effort estimates
  (setq org-global-properties
        '(("Effort_ALL" . "0:15 0:30 1:00 1:30 2:00 3:00 4:00")))
  (setq org-columns-default-format "%40ITEM(Task) %TODO %3PRIORITY %17Effort(Effort){:} %TAGS")

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Deep work" tags-todo "+deep"
           ((org-agenda-overriding-header "Deep Work Queue")
            (org-agenda-sorting-strategy '(priority-down effort-up))))
          ("S" "Shallow work" tags-todo "+shallow"
           ((org-agenda-overriding-header "Shallow Work Queue")
            (org-agenda-sorting-strategy '(effort-up))))
          ("n" "Next actions" todo "NEXT"
           ((org-agenda-overriding-header "Next Actions")
            (org-agenda-sorting-strategy '(priority-down effort-up))))
          ("w" "Work overview" ((tags-todo "+deep"
                                 ((org-agenda-overriding-header "Deep Work")))
                                (tags-todo "+shallow"
                                 ((org-agenda-overriding-header "Shallow Work")))
                                (tags-todo "-deep-shallow"
                                 ((org-agenda-overriding-header "Other Tasks")))))
          ("g" "GTD dashboard" ((todo "NEXT"
                                 ((org-agenda-overriding-header "Next Actions")
                                  (org-agenda-sorting-strategy '(priority-down effort-up))))
                                (tags-todo "+deep"
                                 ((org-agenda-overriding-header "Deep Work")
                                  (org-agenda-sorting-strategy '(priority-down effort-up))))
                                (tags-todo "+shallow"
                                 ((org-agenda-overriding-header "Shallow Work")
                                  (org-agenda-sorting-strategy '(effort-up))))
                                (todo "PROJ"
                                 ((org-agenda-overriding-header "Projects")))
                                (todo "WAIT"
                                 ((org-agenda-overriding-header "Waiting")))
                                (todo "TODO"
                                 ((org-agenda-overriding-header "All Open Tasks")))))))

  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-current-file "Tasks")
           "** TODO %?\n   :PROPERTIES:\n   :Effort: 0:30\n   :Created: %U\n   :Source: manual\n   :END:"
           :empty-lines 1)
          ("n" "Next action" entry (file+headline org-current-file "Tasks")
           "** NEXT %?\n   :PROPERTIES:\n   :Effort: 0:30\n   :Created: %U\n   :Source: manual\n   :END:"
           :empty-lines 1)
          ("d" "Deep work" entry (file+headline org-current-file "Tasks")
           "** TODO %? :deep:\n   :PROPERTIES:\n   :Effort: 2:00\n   :Created: %U\n   :Source: manual\n   :END:"
           :empty-lines 1)
          ("s" "Shallow task" entry (file+headline org-current-file "Tasks")
           "** TODO %? :shallow:\n   :PROPERTIES:\n   :Effort: 0:30\n   :Created: %U\n   :Source: manual\n   :END:"
           :empty-lines 1)
          ("p" "Project" entry (file+headline org-current-file "Projects")
           "** PROJ %? [0%]\n   :PROPERTIES:\n   :Effort: 2:00\n   :Created: %U\n   :Source: manual\n   :END:\n*** TODO "
           :empty-lines 1)
          ("i" "Idea" entry (file+headline org-current-file "Ideas")
           "** %?\n   :PROPERTIES:\n   :Created: %U\n   :Source: manual\n   :END:"
           :empty-lines 1)))

  (my/gtd-ensure-headings)

  ;; Auto-complete parent TODO when all children are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-todo-log-states)
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

  ;; --- Checkbox / statistics automation ---

  (add-hook 'org-checkbox-statistics-hook 'my/org-safe-update-checkboxes)

  (defun my/org-update-todo-checkbox ()
    "Update TODO state based on checkbox completion."
    (when (eq major-mode 'org-mode)
      (save-excursion
        (condition-case nil
            (progn
              (org-back-to-heading t)
              (let ((beg (point))
                    (end (save-excursion (org-end-of-subtree t) (point)))
                    (todo-state (org-get-todo-state)))
                (when todo-state
                  (goto-char beg)
                  (let ((total 0) (done 0))
                    (while (re-search-forward "^[ \t]*[-+*][ \t]+\\[\\([X ]\\)\\]" end t)
                      (setq total (1+ total))
                      (when (string= (match-string 1) "X")
                        (setq done (1+ done))))
                    (goto-char beg)
                    (cond
                     ((and (> total 0) (= done total) (not (string= todo-state "DONE")))
                      (org-todo "DONE"))
                     ((and (> total 0) (< done total) (member todo-state org-done-keywords))
                      (org-todo "TODO")))))))
          (error nil)))))

  (defun my/org-safe-update-checkboxes ()
    "Safely update all checkbox statistics without freezing."
    (when (eq major-mode 'org-mode)
      (condition-case err
          (progn
            (org-update-checkbox-count 'all)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward "^\\*+ \\(TODO\\|NEXT\\|PROJ\\|WAIT\\|DONE\\|CANCELLED\\)" nil t)
                (save-excursion
                  (my/org-update-todo-checkbox))))
            (my/org-update-parent-todo-statistics)
            (my/org-update-parent-todo-states))
        (error (message "Error updating checkboxes: %s" err)))))

  (defun my/org-update-parent-todo-statistics ()
    "Update parent TODO statistics to include checkbox percentages."
    (condition-case _
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\(\\*+\\) \\(TODO\\|NEXT\\|PROJ\\|WAIT\\) .+\\[\\([0-9]+\\)%\\]" nil t)
            (let* ((level (length (match-string 1)))
                   (cookie-start (match-beginning 3))
                   (cookie-end (match-end 3))
                   (total 0.0)
                   (done 0.0))
              (when (and cookie-start cookie-end)
                (save-excursion
                  (org-back-to-heading t)
                  (when (save-excursion (ignore-errors (org-goto-first-child)))
                    (org-goto-first-child)
                    (let ((continue t))
                      (while continue
                        (when (and (= (org-current-level) (1+ level))
                                   (org-get-todo-state))
                          (let* ((child-state (org-get-todo-state))
                                 (line-end (line-end-position))
                                 (checkbox-match (save-excursion
                                                   (beginning-of-line)
                                                   (re-search-forward "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" line-end t))))
                            (setq total (1+ total))
                            (cond
                             (checkbox-match
                              (let ((checked (string-to-number (match-string 1)))
                                    (total-boxes (string-to-number (match-string 2))))
                                (when (> total-boxes 0)
                                  (setq done (+ done (/ (float checked) total-boxes))))))
                             ((member child-state org-done-keywords)
                              (setq done (1+ done))))))
                        (setq continue (ignore-errors (org-goto-sibling)))))
                    (when (> total 0)
                      (let ((pct (round (* 100 (/ done total)))))
                        (save-excursion
                          (goto-char cookie-start)
                          (delete-region cookie-start cookie-end)
                          (insert (number-to-string pct)))))))))))
      (error nil)))

  (defun my/org-update-parent-todo-states ()
    "Update parent TODO/PROJ states based on their children's completion."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) \\(TODO\\|NEXT\\|PROJ\\|WAIT\\|DONE\\|CANCELLED\\) .+\\[\\([0-9]+\\)%\\]" nil t)
        (let* ((level (length (match-string 1)))
               (current-state (match-string 2))
               (percentage (string-to-number (match-string 3))))
          (save-excursion
            (org-back-to-heading t)
            (when (save-excursion (ignore-errors (org-goto-first-child)))
              (cond
               ((and (= percentage 100) (not (string= current-state "DONE")))
                (org-todo "DONE"))
               ((and (< percentage 100) (string= current-state "DONE"))
                (if (save-excursion
                      (org-goto-first-child)
                      (let ((has-subtasks nil))
                        (while (and (not has-subtasks)
                                    (= (org-current-level) (1+ level)))
                          (when (org-get-todo-state)
                            (setq has-subtasks t))
                          (unless (org-goto-sibling)
                            (goto-char (point-max))))
                        has-subtasks))
                    (org-todo "PROJ")
                  (org-todo "TODO"))))))))))

  (advice-add 'org-toggle-checkbox :after
              (lambda (&rest _)
                (run-at-time 0.1 nil #'my/org-safe-update-checkboxes)))

  (advice-add 'org-ctrl-c-ctrl-c :after
              (lambda (&rest _)
                (when (org-at-item-checkbox-p)
                  (run-at-time 0.1 nil #'my/org-safe-update-checkboxes))))

  (defun my/force-org-checkbox-statistics-update ()
    "Force update of all checkbox statistics."
    (interactive)
    (my/org-safe-update-checkboxes)
    (message "Checkbox statistics updated"))

  ) ;; end (after! org)

;;; === Org-roam ===

(after! org-roam
  (setq org-roam-directory "~/github/bufrr.github.io/kb/wiki/"
        org-roam-db-location "~/github/bufrr.github.io/.org-roam.db"))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))
