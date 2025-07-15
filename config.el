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

;; GTD Configuration (Simple 2-file system)
(setq org-current-file "~/org/gtd/current.org")
(setq org-archive-file "~/org/gtd/archive.org")

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
      :desc "Capture" "g c" #'org-capture
      :desc "Agenda" "g a" #'org-agenda
      :desc "Archive done" "g A" #'org-archive-subtree
      :desc "View current work" "g w" (lambda () (interactive) (find-file org-current-file))
      :desc "View archive" "g v" (lambda () (interactive) (find-file org-archive-file))
      :desc "Insert checkbox" "g b" (lambda () (interactive)
                                      (end-of-line)
                                      (newline-and-indent)
                                      (insert "- [ ] ")))

;; Org-publish configuration for blog
(after! org
  (require 'ox-publish)

  ;; GTD Settings (Simple 2-file system)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Agenda files - only current work
  (setq org-agenda-files (list org-current-file))

  ;; Archive location
  (setq org-archive-location (concat org-archive-file "::* Archived %s"))
  
  ;; Automatically add CLOSED timestamp when marking as DONE
  (setq org-log-done 'time)
  
  ;; Enable recursive counting for statistics cookies
  (setq org-hierarchical-todo-statistics nil)
  
  ;; Auto-complete parent TODO when all children are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-todo-log-states)
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  
  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

  ;; Optional: Update checkbox count and TODO state when toggling checkboxes
  (defun my/org-checkbox-todo ()
    "Update TODO state when checkbox count changes."
    (save-excursion
      (org-back-to-heading t)
      (when-let ((todo-state (org-get-todo-state)))
        (org-update-checkbox-count t))))
  
  (add-hook 'org-checkbox-statistics-hook #'my/org-checkbox-todo)

  ;; Refile targets - can refile within current file
  (setq org-refile-targets '((org-current-file :maxlevel . 3)))

  ;; Simple capture templates
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-current-file "Tasks")
           "** TODO %?\n   %U" :empty-lines 1)
          ("p" "Project" entry (file+headline org-current-file "Projects")
           "** PROJ %? [%]\n   :PROPERTIES:\n   :COOKIE_DATA: todo recursive\n   :END:\n   %U\n*** TODO " :empty-lines 1)
          ("i" "Idea" entry (file+headline org-current-file "Ideas")
           "** %?\n   %U" :empty-lines 1)))

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
