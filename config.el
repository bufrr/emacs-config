;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "bytenoob"
      user-mail-address "chouyc.adam@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; GTD Configuration
(setq org-gtd-directory "~/org/gtd/")
(setq org-inbox-file (concat org-gtd-directory "inbox.org"))
(setq org-gtd-file (concat org-gtd-directory "gtd.org"))
(setq org-calendar-file (concat org-gtd-directory "calendar.org"))

;; Blog configuration
(setq blog-directory "~/blog/")
(setq blog-posts-directory (concat blog-directory "posts/"))
(setq blog-publish-directory (concat blog-directory "public/"))
(setq blog-static-directory (concat blog-directory "static/"))
(setq blog-templates-directory (concat blog-directory "templates/"))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Blog helper functions (defined at top level so they're always available)
(defun blog/new-post (title)
  "Create a new blog post with TITLE."
  (interactive "sPost title: ")
  (let* ((filename (concat blog-posts-directory
                           (format-time-string "%Y-%m-%d-")
                           (replace-regexp-in-string "[^a-zA-Z0-9]+" "-"
                                                     (downcase title))
                           ".org"))
         (buffer (find-file filename)))
    (insert-file-contents (concat blog-templates-directory "post-template.org"))
    (goto-char (point-min))
    (search-forward "#+TITLE: ")
    (insert title)
    (search-forward "#+DATE: ")
    (insert (format-time-string "<%Y-%m-%d %a>"))
    (search-forward "#+AUTHOR: ")
    (insert (or user-full-name "Your Name"))
    (search-forward "#+LASTMOD: ")
    (insert (format-time-string "<%Y-%m-%d %a>"))
    ;; Position cursor at FILETAGS for easy editing
    (goto-char (point-min))
    (search-forward "#+FILETAGS: ")
    (end-of-line)))

(defun blog/publish ()
  "Publish the blog."
  (interactive)
  (require 'ox-publish)
  (org-publish "blog" t)
  (message "Blog published!"))

(defun blog/update-lastmod ()
  "Update the #+LASTMOD: date in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+LASTMOD:.*$" nil t)
      (replace-match (format "#+LASTMOD: %s" (format-time-string "<%Y-%m-%d %a>"))))))

;; Auto-update LASTMOD on save for blog posts
(add-hook 'before-save-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-prefix-p blog-posts-directory (buffer-file-name))
                       (string-suffix-p ".org" (buffer-file-name)))
              (blog/update-lastmod))))

;; Keybindings
(map! :leader
      :desc "New blog post" "B n" #'blog/new-post
      :desc "Publish blog" "B p" #'blog/publish
      
      ;; GTD Keybindings
      :desc "GTD Inbox" "g i" (lambda () (interactive) (find-file org-inbox-file))
      :desc "GTD Main" "g g" (lambda () (interactive) (find-file org-gtd-file))
      :desc "GTD Calendar" "g c" (lambda () (interactive) (find-file org-calendar-file))
      :desc "GTD Review" "g r" (lambda () (interactive) (org-agenda nil "g"))
      :desc "GTD Next Actions" "g n" (lambda () (interactive) (org-agenda nil "n"))
      :desc "GTD Projects" "g p" (lambda () (interactive) (org-agenda nil "p"))
      :desc "GTD Weekly Review" "g w" (lambda () (interactive) (org-agenda nil "w"))
      :desc "GTD Capture" "g x" #'org-capture
      :desc "Process Inbox" "g I" #'gtd/process-inbox
      :desc "Archive Done" "g a" #'gtd/archive-done-tasks
      :desc "Find Stale Projects" "g s" #'gtd/find-stale-projects)

;; Org-publish configuration for blog
(after! org
  (require 'ox-publish)
  
  ;; GTD Settings
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELLED(c@)")))
  
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#fb4934" :weight bold))
          ("NEXT" . (:foreground "#fabd2f" :weight bold))
          ("WAITING" . (:foreground "#fe8019" :weight bold))
          ("SOMEDAY" . (:foreground "#83a598" :weight bold))
          ("DONE" . (:foreground "#8ec07c" :weight bold))
          ("CANCELLED" . (:foreground "#928374" :weight bold))))
  
  ;; Agenda files
  (setq org-agenda-files (list org-inbox-file
                               org-gtd-file
                               org-calendar-file))
  
  ;; Refile targets
  (setq org-refile-targets '((org-gtd-file :maxlevel . 2)
                             (org-calendar-file :maxlevel . 1)
                             (org-inbox-file :maxlevel . 1)))
  
  ;; Archive location
  (setq org-archive-location (concat org-gtd-directory "archive.org::* Archived Tasks"))
  
  ;; Tags for contexts
  (setq org-tag-alist '((:startgroup)
                        ("@home" . ?h)
                        ("@work" . ?w)
                        ("@computer" . ?c)
                        ("@phone" . ?p)
                        ("@errands" . ?e)
                        (:endgroup)
                        ("URGENT" . ?u)
                        ("PROJECT" . ?P)
                        (:startgroup)
                        ("Energy")
                        ("High" . ?H)
                        ("Medium" . ?M)
                        ("Low" . ?L)
                        (:endgroup)))
  
  ;; Effort estimates
  (setq org-global-properties
        '(("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 4:00 8:00")))
  
  ;; Capture templates for GTD
  (setq org-capture-templates
        '(("i" "Inbox" entry (file org-inbox-file)
           "* TODO %?\n  %U\n  %a" :empty-lines 1)
          
          ("t" "Task" entry (file+headline org-gtd-file "Next Actions")
           "* TODO %? %^g\n  %U\n  %a" :empty-lines 1)
          
          ("p" "Project" entry (file+headline org-gtd-file "Projects")
           "* TODO %? [/] :PROJECT:\n  :PROPERTIES:\n  :CREATED: %U\n  :LAST_REVIEWED: %U\n  :END:\n  %a\n** TODO Define project outcome\n** TODO Identify project milestones\n** TODO List next actions\n" :empty-lines 1)
          
          ("w" "Waiting For" entry (file+headline org-gtd-file "Waiting For")
           "* WAITING %? :@waiting:\n  %U\n  From: %^{Who}\n  %a" :empty-lines 1)
          
          ("s" "Someday/Maybe" entry (file+headline org-gtd-file "Someday/Maybe")
           "* SOMEDAY %?\n  %U\n  %a" :empty-lines 1)
          
          ("c" "Calendar" entry (file+headline org-calendar-file "Appointments & Scheduled Items")
           "* %?\n  SCHEDULED: %^T\n  %U\n  %a" :empty-lines 1)
          
          ("r" "Reference" entry (file+headline org-gtd-file "Reference")
           "* %?\n  %U\n  %a" :empty-lines 1)))
  
  ;; GTD Helper Functions
  (defun gtd/process-inbox ()
    "Interactively process items in the GTD inbox."
    (interactive)
    (find-file org-inbox-file)
    (org-overview)
    (org-show-subtree)
    (goto-char (point-min))
    (forward-line 3)
    (while (not (eobp))
      (when (looking-at "^\\*+ ")
        (org-show-subtree)
        (let* ((title (org-get-heading t t t t))
               (choice (read-char-choice
                        (format "Process: %s\n[n]ext action  [p]roject  [s]omeday  [c]alendar  [w]aiting  [r]eference  [d]elete  [q]uit: "
                                title)
                        '(?n ?p ?s ?c ?w ?r ?d ?q))))
          (pcase choice
            (?q (user-error "Quit processing"))
            (?d (org-cut-subtree)
                (message "Deleted: %s" title))
            (?n (org-todo "NEXT")
                (org-set-tags "@computer")
                (org-refile nil nil (list "Next Actions" org-gtd-file nil nil)))
            (?p (org-todo "TODO")
                (org-set-tags "PROJECT")
                (org-refile nil nil (list "Projects" org-gtd-file nil nil)))
            (?s (org-todo "SOMEDAY")
                (org-refile nil nil (list "Someday/Maybe" org-gtd-file nil nil)))
            (?c (org-todo "TODO")
                (call-interactively 'org-schedule)
                (org-refile nil nil (list "Appointments & Scheduled Items" org-calendar-file nil nil)))
            (?w (org-todo "WAITING")
                (org-set-tags "@waiting")
                (setq waiting-for (read-string "Waiting for whom? "))
                (org-set-property "WAITING_FOR" waiting-for)
                (org-refile nil nil (list "Waiting For" org-gtd-file nil nil)))
            (?r (org-todo nil)
                (org-refile nil nil (list "Reference" org-gtd-file nil nil))))))
      (or (org-at-heading-p) (outline-next-heading)))
    (when (y-or-n-p "Inbox processing complete! Archive empty inbox? ")
      (write-region "" nil org-inbox-file)))
  
  (defun gtd/quick-capture ()
    "Quick capture to inbox with minimal interruption."
    (interactive)
    (org-capture nil "i"))
  
  (defun gtd/archive-done-tasks ()
    "Archive all DONE tasks older than 7 days."
    (interactive)
    (org-map-entries
     (lambda ()
       (let ((closed-time (org-entry-get nil "CLOSED")))
         (when (and closed-time
                    (> (- (float-time)
                          (float-time (date-to-time closed-time)))
                       (* 7 24 60 60)))
           (org-archive-subtree))))
     "/DONE" 'agenda))
  
  (defun gtd/find-stale-projects ()
    "Find projects that haven't been reviewed in over 7 days."
    (interactive)
    (let ((stale-projects '()))
      (org-map-entries
       (lambda ()
         (let* ((last-reviewed (org-entry-get nil "LAST_REVIEWED"))
                (days-old (if last-reviewed
                              (/ (- (float-time)
                                    (float-time (date-to-time last-reviewed)))
                                 (* 24 60 60))
                            999)))
           (when (> days-old 7)
             (push (cons (org-get-heading t t t t) days-old) stale-projects))))
       "PROJECT" 'agenda)
      (if stale-projects
          (with-current-buffer (get-buffer-create "*Stale Projects*")
            (erase-buffer)
            (insert "Projects needing review:\n\n")
            (dolist (project stale-projects)
              (insert (format "- %s (%.0f days old)\n" (car project) (cdr project))))
            (display-buffer (current-buffer)))
        (message "No stale projects found!"))))
  
  ;; Custom Agenda Views for GTD
  (setq org-agenda-custom-commands
        '(("g" "GTD Review"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-prefix-format " %i %-12:c%?-12t% s")))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-sorting-strategy '(priority-down effort-up category-keep))))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting For")
                             (org-agenda-sorting-strategy '(priority-down category-keep))))
            (tags "INBOX" ((org-agenda-overriding-header "Inbox - Process these items")))
            (todo "TODO" ((org-agenda-overriding-header "All TODOs")
                          (org-agenda-sorting-strategy '(priority-down category-keep))))))
          
          ("n" "Next Actions by Context"
           ((tags-todo "@home+NEXT" ((org-agenda-overriding-header "Next Actions - @home")))
            (tags-todo "@work+NEXT" ((org-agenda-overriding-header "Next Actions - @work")))
            (tags-todo "@computer+NEXT" ((org-agenda-overriding-header "Next Actions - @computer")))
            (tags-todo "@phone+NEXT" ((org-agenda-overriding-header "Next Actions - @phone")))
            (tags-todo "@errands+NEXT" ((org-agenda-overriding-header "Next Actions - @errands")))
            (tags-todo "NEXT-@home-@work-@computer-@phone-@errands" 
                       ((org-agenda-overriding-header "Next Actions - No Context")))))
          
          ("p" "Projects" tags "PROJECT" 
           ((org-agenda-overriding-header "Active Projects")
            (org-agenda-sorting-strategy '(priority-down category-keep))))
          
          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-prefix-format " %i %-12:c%?-12t% s")))
            (todo "DONE" ((org-agenda-overriding-header "Completed This Week")
                          (org-agenda-show-log 'closed)
                          (org-agenda-start-day "-7d")
                          (org-agenda-sorting-strategy '(priority-down effort-up category-keep))))
            (todo "SOMEDAY" ((org-agenda-overriding-header "Someday/Maybe - Review for activation")))
            (stuck "" ((org-agenda-overriding-header "Stuck Projects")))))))
  
  ;; Custom HTML preamble and postamble
  (defun blog/preamble (info)
    (concat
     "<header class=\"site-header\">"
     "<div class=\"container\">"
     "<h1 class=\"site-title\"><a href=\"/\">My Blog</a></h1>"
     "<nav class=\"site-nav\">"
     "<a href=\"/\">Home</a>"
     "<a href=\"/about.html\">About</a>"
     "</nav>"
     "</div>"
     "</header>"))
  
  (defun blog/postamble (info)
    (concat
     "<footer class=\"site-footer\">"
     "<div class=\"container\">"
     "<p>&copy; " (format-time-string "%Y") " - Built with Emacs & Org-mode</p>"
     "</div>"
     "</footer>"))
  
  ;; Custom sitemap function to exclude author
  (defun blog/sitemap-function (title list)
    "Generate sitemap as an Org file without author metadata."
    (concat "#+TITLE: \n"  ; Empty title
            "#+AUTHOR:\n"
            "#+OPTIONS: author:nil toc:nil num:nil\n\n"
            (org-list-to-org list)))
  
  ;; Custom function to generate article metadata
  (defun blog/article-meta (info)
    (let* ((author (org-export-data (plist-get info :author) info))
           (date (org-export-data (org-export-get-date info "%B %d, %Y") info))
           (tags (plist-get info :filetags))
           (lastmod (plist-get info :lastmod)))
      (concat
       "<div class=\"article-meta\">"
       (when author
         (format "<span class=\"author\">By %s</span>" author))
       (when date
         (format "<span class=\"date\">Published: %s</span>" date))
       (when (and lastmod (not (string-empty-p lastmod)))
         (format "<span class=\"updated\">Updated: %s</span>" lastmod))
       (when tags
         (concat "<div class=\"tags\">"
                 (mapconcat (lambda (tag)
                              (format "<span class=\"tag\">%s</span>" tag))
                            tags " ")
                 "</div>"))
       "</div>")))
  
  ;; Publishing configuration
  (setq org-publish-project-alist
        `(("blog-posts"
           :base-directory ,blog-posts-directory
           :base-extension "org"
           :publishing-directory ,blog-publish-directory
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :section-numbers nil
           :with-toc t
           :with-author t
           :with-date t
           :html-head "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
<link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap\" rel=\"stylesheet\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/css/blog.css?v=4\" />
<script src=\"/static/js/blog.js?v=4\" defer></script>"
           :html-preamble blog/preamble
           :html-postamble blog/postamble
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Posts"
           :sitemap-sort-files anti-chronologically
           :sitemap-style list
           :sitemap-format-entry (lambda (entry style project)
                                   (format "[[file:%s][%s]]"
                                           entry
                                           (org-publish-find-title entry project)))
           :sitemap-function blog/sitemap-function)
          
          ("blog-static"
           :base-directory ,blog-static-directory
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg\\|woff\\|woff2\\|ico"
           :publishing-directory ,(concat blog-publish-directory "static/")
           :recursive t
           :publishing-function org-publish-attachment)
          
          ("blog" :components ("blog-posts" "blog-static")))))
