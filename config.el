;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

;; I moved this to ~/personal.el

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Victor Mono" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Calibri" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(defvar not-win (eq system-type 'gnu/linux)
  "If NOT-WIN is non-nil, then we're not in MS-Windows.")

(defvar at-work (string-equal (getenv "HOSTNAME") "Athos")
  "Athos is the machine at work.")

(defvar org-directory-root (if at-work
                               "C:/Users/K.C.Juntunen/OneDrive/org/"
                             "D:/OneDrive/org/")
  "The root upon which to build my org directory")

(setq org-directory
      (if not-win
          "~/Dropbox/org"
        (concat org-directory-root (format-time-string "%Y"))))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 80))

(defun kc/set-up-emacs ()
  (setq-default blink-cursor-delay .2
                blink-cursor-interval .2
                blink-cursor-blinks 10000
                indent-tabs-mode nil
                scroll-step 1
                sentence-end-double-space nil
                scroll-margin 0
                scroll-conservatively 100000
                scroll-preserve-screen-position 1
                show-paren-delay 0
                make-backup-files nil
                auto-save-default nil
                inhibit-startup-screen t)
  (blink-cursor-mode))

(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")

(defun kc/set-up-org ()
  (setq-default
                kc/org-all-agenda-files (directory-files
                                         (expand-file-name org-directory) t org-agenda-file-regexp)
                org-agenda-span 'day
                org-fontify-quote-and-verse-blocks t
                org-use-fast-todo-selection t
                org-hide-emphasis-markers nil
                org-treat-S-cursor-todo-selection-as-state-change nil
                org-ellipsis "▽"
                org-clock-continuously t
                org-clock-out-remove-zero-time-clocks t
                org-log-done 'time
                org-refile-targets (quote ((nil :maxlevel . 1) (kc/org-all-agenda-files :maxlevel . 2)))
                org-catch-invisible-edits 'smart
                org-agenda-clockreport-parameter-plist
                '(:link t :maxlevel 4 :fileskip0 t
                  :properties ("ClientAccount" "TradingPartnerAccount" "Request" "Phase" "Task"))
                org-deadline-warning-days 45
                org-agenda-window-setup 'current-window
                org-agenda-skip-scheduled-if-done t
                org-agenda-skip-deadline-if-done t
                org-agenda-skip-timestamp-if-done t
                org-agenda-log-mode-items '(closed clock state)
                org-columns-default-format
                "%25ITEM(Task) %40Description %20Captured %10Effort(Effort){:} %10CLOCKSUM"
                org-global-properties
                (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                        ("STYLE_ALL" . "habit")))
                org-todo-keywords
                (quote ((sequence "TODO(t)" "WIP(n)" "|" "DONE(d)" "CANCELLED(c/!)")
                        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                                  "CANCELLED(c/!)" "PHONE" "MEETING")))
                org-todo-keyword-faces
                (quote (("TODO" :foreground "red" :weight bold)
                        ("WIP" :foreground "blue" :weight bold)
                        ("DONE" :foreground "forest green" :weight bold)
                        ("WAITING" :foreground "orange" :weight bold)
                        ("HOLD" :foreground "magenta" :weight bold)
                        ("CANCELLED" :foreground "forest green" :weight bold)
                        ("MEETING" :foreground "forest green" :weight bold)
                        ("PHONE" :foreground "forest green" :weight bold)))
                org-todo-state-tags-triggers
                (quote (("CANCELLED" ("ARCHIVE" . t))
                        ("WAITING" ("WAITING" . t))
                        ("HOLD" ("WAITING") ("HOLD" . t))
                        (done ("WAITING") ("HOLD"))
                        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                        ("WIP" ("WAITING") ("CANCELLED") ("HOLD"))
                        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
                kc/refile-file (concat org-directory "/refile.org")
                kc/diary-file (concat org-directory "/diary.org")
                kc/notes-file (concat org-directory "/notes.org")
                org-capture-templates
                '(("t" "todo" entry
                   (file kc/refile-file)
                   "* TODO %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Client: %^{Client}
:END:\n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
                   :clock-in t :clock-resume t :prepend t)
                  ("p" "Phone call" entry
                   (file kc/refile-file)
                   "* PHONE %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:END:" :clock-in t :clock-resume t)
                  ("i" "Interuption" entry
                   (file kc/refile-file)
                   "* %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:END:
:CLIPBOARD:\n#+begin_quote\n%x\n#+end_quote\n:END:\n" :clock-in t :clock-resume t)
                  ("s" "Source Note" entry
                   (file kc/refile-file)
                   "* %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:END:
#+begin_source %^{Language|conf|csharp|powershell|sgml|shell|sql}\n%x\n#+end_source\n" :clock-in t :clock-resume t)
                  ("j" "Journal" entry
                   (file+olp+datetree kc/diary-file)
                   "* %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Category: %^{Entry type|Bible|Note|Journal}
:END:" :clock-in t :clock-resume t)
                  ("n" "Note" entry
                   (file kc/notes-file)
                   "* %? :NOTE:
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:END:" :clock-in t :clock-resume t)
                  ("m" "Meeting" entry
                   (file kc/notes-file)
                   "* MEETING %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:END:" :clock-in t :clock-resume t)
                  ("b" "Bookmark" entry
                   (file+headline kc/notes-file "Bookmarks")
                   "* %?\n:PROPERTIES:\n:Captured: %U\n:END:\n\n" :empty-lines 1))
                org-clock-in-switch-to-state
                (defun kc/clock-in-to-wip (kw)
                  "Switch from TODO to WIP when clocking in."
                  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
                    (cond
                     ((member (org-get-todo-state) (list "TODO"))
                      "WIP")
                     (t
                      kw)))))
  (setq org-agenda-files kc/org-all-agenda-files)
  (message "kc/set-up-org has been executed"))

(after! org
  (kc/set-up-org))

(kc/set-up-emacs)

;; This is here so PLINK can find my private key.
(eval-after-load "tramp"
  '(setf (cadr (assq 'tramp-login-args (cdr (assoc "plink" tramp-methods))))
         '(("-l" "%u") ("-P" "%p") ("-i ~/.ssh/id_rsa.ppk") ("-ssh") ("-t") ("%h") ("\"")
           ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=#$ '") ("/bin/sh") ("\""))))

(setq org-roam-directory "D:/Dropbox/Dropbox/roam")

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))

(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-file-apps
      '((remote . emacs)
        (auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)
        ("\\.sln\\'" . default)))

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 1)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Work"
                           :tag "Work"
                           :order 3)
                          (:name "Dissertation"
                           :tag "Dissertation"
                           :order 7)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Essay 1"
                           :tag "Essay1"
                           :order 2)
                          (:name "Reading List"
                           :tag "Read"
                           :order 8)
                          (:name "Work In Progress"
                           :tag "WIP"
                           :order 5)
                          (:name "Blog"
                           :tag "Blog"
                           :order 12)
                          (:name "Essay 2"
                           :tag "Essay2"
                           :order 3)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily"))))))))
         )
        ("w" "Master Agenda"
         ((agenda ""
                  ((org-agenda-span '1)
                   (org-agenda-files (append (file-expand-wildcards "~/.org/gtd/*.org")))
                   (org-agenda-start-day (org-today))))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'nm/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-SOMEDAY-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-SOMEDAY-REFILE-CANCELLED-/NEXT"
                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'nm/skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "SOMEDAY/"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'nm/skip-scheduled)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         )))

 (after! evil-snipe
   (evil-snipe-mode -1))

(defun load-personal-file ()
  "Load stuff I don't want on Github."
  (interactive)
  (let ((personal-file (expand-file-name "~/.personal.el")))
    (if (not (file-exists-p personal-file))
        (error (format "`%s' does not exist." personal-file))
      (message "Loading %s..." personal-file)
      (load-file personal-file))))

(defun add-missing-info-dir ()
  "Add default info directory to `Info-default-directory-list'"
  (let ((default-Info-directory (car Info-default-directory-list)))
    (if (member  default-Info-directory Info-directory-list)
        (message "%s is already in `Info-directory-list'" default-Info-directory)
      (add-to-list 'Info-directory-list (car Info-default-directory-list)))))

(load-personal-file)
(add-missing-info-dir)

(load-file (concat (file-truename "~/.doom.d/york-mode.el")))

(server-start)

(message "config.el has been eval'd")
