(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-list
      '(
        consult
        display-line-numbers
        ef-themes
        elfeed-org
        fontaine
        helpful
        orderless
        org-drill
        org-super-agenda
        spacious-padding
        vertico
        yascroll
        zenburn-theme))

;; Initialize the packaging systems and prepares it to be used
(package-initialize)

;; Fetch the list of available packages in the case it does not exist locally
(unless package-archive-contents
  (package-refresh-contents))

;; Try to install the packages within the package-list variable previously set
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(blink-cursor-mode -1)           ; disable the blinking of the cursor
(scroll-bar-mode -1)             ; disable the scroll bar
(recentf-mode 1)                 ; remember recently edited files
(savehist-mode 1)                ; remember minibuffer prompt history
(save-place-mode 1)              ; remember the last place you visited in a file
(global-auto-revert-mode 1)      ; automatically revert buffers when the underlying file has changed
(setq use-dialog-box nil)        ; prevent using UI dialogs for prompts
(setq require-final-newline nil) ; prevent Emacs from automatcally adding a new line to the end of a file even
                                 ; when I try to remove it

;; tell Emacs to write customizable variables on another file
;; so that this init.el file don't get polluted
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; enable which-key minor mode, that displays the key
;; bindings following the currently entered incomplete
;; command (a prefix) in a popup
(which-key-mode)

;; disable menu bar if running GUI Emacs
(if window-system (menu-bar-mode -1))

;; disable tool bar if running GUI Emacs
(if window-system (tool-bar-mode -1))

;; configure newlines and indentations to use spaces by default
(setq-default indent-tabs-mode nil)

;; set eww (Emacs Web Wowser) as default browser
(setq browse-url-browser-function 'eww-browse-url)

(setq org-directory "~/org")

(defcustom my/path-aliases
  (list :emacs    "~/.emacs.d"
        :org      org-directory
        :gtd      (expand-file-name "gtd" org-directory)
        :personal (expand-file-name "personal" org-directory))
  "Location of my paths for ease of usage. Customize for each
              environment if needed.")

(defun my/path (dir &optional subpath)
  (let ((dir (file-name-as-directory
              (cl-getf my/path-aliases dir
                       (format "~/%s" dir))))
        (subpath (or subpath "")))
    (concat dir subpath)))

(defcustom myinbox (my/path :gtd "inbox.org")
  "This points to the inbox org file")
(defcustom mypersonal (my/path :gtd "personal.org")
  "This points to the personal org file")
(defcustom mywork (my/path :gtd "work.org")
  "This points to the work org file")
(defcustom myprojects (my/path :gtd "projects.org")
  "This points to the projects org file")
(defcustom myjournal (my/path :gtd "journal.org")
  "This points to the journal org file")
(defcustom mymasters (my/path :gtd "masters.org")
  "This points to the masters org file")

;; Line numbers on the side of the window
(use-package display-line-numbers
  :ensure nil
  :bind
  ("<f7>" . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t))

(global-yascroll-bar-mode 1)

(require 'fontaine)

(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))

;; Aporetic is my highly customised build of Iosevka:
;; <https://github.com/protesilaos/aporetic>.
(setq fontaine-presets
      '((small
         :default-family "Aporetic Serif Mono"
         :default-height 80
         :variable-pitch-family "Aporetic Sans")
        (regular) ; like this it uses all the fallback values and is named `regular'
        (medium
         :default-weight semilight
         :default-height 115
         :bold-weight extrabold)
        (large
         :inherit medium
         :default-height 150)
        (presentation
         :default-height 180)
        (t
         ;; I keep all properties for didactic purposes, but most can be
         ;; omitted.  See the fontaine manual for the technicalities:
         ;; <https://protesilaos.com/emacs/fontaine>.
         :default-family "Aporetic Sans Mono"
         :default-weight regular
         :default-height 100

         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0

         :fixed-pitch-serif-family nil ; falls back to :default-family
         :fixed-pitch-serif-weight nil ; falls back to :default-weight
         :fixed-pitch-serif-height 1.0

         :variable-pitch-family "Aporetic Serif"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0

         :mode-line-active-family nil ; falls back to :default-family
         :mode-line-active-weight nil ; falls back to :default-weight
         :mode-line-active-height 0.9

         :mode-line-inactive-family nil ; falls back to :default-family
         :mode-line-inactive-weight nil ; falls back to :default-weight
         :mode-line-inactive-height 0.9

         :header-line-family nil ; falls back to :default-family
         :header-line-weight nil ; falls back to :default-weight
         :header-line-height 0.9

         :line-number-family nil ; falls back to :default-family
         :line-number-weight nil ; falls back to :default-weight
         :line-number-height 0.9

         :tab-bar-family nil ; falls back to :default-family
         :tab-bar-weight nil ; falls back to :default-weight
         :tab-bar-height 1.0

         :tab-line-family nil ; falls back to :default-family
         :tab-line-weight nil ; falls back to :default-weight
         :tab-line-height 1.0

         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold

         :italic-family nil
         :italic-slant italic

         :line-spacing nil)))

;; Set the last preset or fall back to desired style from `fontaine-presets'
;; (the `regular' in this case).
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; Persist the latest font preset when closing/starting Emacs and
;; while switching between themes.
(fontaine-mode 1)

;; fontaine does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
(define-key global-map (kbd "C-c f") #'fontaine-set-preset)

;; ref: https://protesilaos.com/emacs/ef-themes-pictures

;; Make customisations that affect Emacs faces BEFORE loading a theme
;; (any change needs a theme re-load to take effect).
(require 'ef-themes)

;; If you like two specific themes and want to switch between them, you
;; can specify them in `ef-themes-to-toggle' and then invoke the command
;; `ef-themes-toggle'.  All the themes are included in the variable
;; `ef-themes-collection'.
(setq ef-themes-to-toggle '(ef-melissa-light ef-melissa-dark))

;; define a keyboard shortcut to toggle between the predefined themes
(define-key global-map (kbd "C-c t") #'ef-themes-toggle)

(setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 variable-pitch light 1.9)
        (1 variable-pitch light 1.8)
        (2 variable-pitch regular 1.7)
        (3 variable-pitch regular 1.6)
        (4 variable-pitch regular 1.5)
        (5 variable-pitch 1.4) ; absence of weight means `bold'
        (6 variable-pitch 1.3)
        (7 variable-pitch 1.2)
        (t variable-pitch 1.1)))

;; They are nil by default...
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
(load-theme 'ef-melissa-light :no-confirm)

;; The themes we provide are recorded in the `ef-themes-dark-themes',
;; `ef-themes-light-themes'.

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-toggle'
;; - `ef-themes-select'
;; - `ef-themes-select-dark'
;; - `ef-themes-select-light'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'

;; Load elfeed-org
(require 'elfeed-org)

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files (list "~/org/elfeed.org"))

;;(setq gdb-many-windows t) ; unfortunately this config is freezing my Emacs on macOS Tahoe.
			    ; I'll enable it as soon as I replace my computer to go back to a *nix OS. (try out the gdb-x https://codeberg.org/pastor/gdb-x)
;;(semantic-mode 0) ;; some people said in the past that disabling the semantic mode prevented the gdb from freezing Emacs, but it didn't help in my case.
(setq gdb-show-main t)
(setq gdb-use-separate-frame nil)
(setq gdb-inferior-tty nil)
(setq gdb-display-io-nopopup t)

(defun my/org-mode-setup ()
  (org-indent-mode)       ; prefixes text lines with virtual spaces to vertically
                          ; align with the headline text
  (visual-line-mode 1))   ; wrap the line at word boundaries near the right window edge

(use-package org
  :ensure nil ; don't try to install it as it's built-in
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ⮧")

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))

  ;;(setq org-hide-emphasis-markers t)

  (set-face-underline 'org-ellipsis nil)

  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((eshell . t)
     (latex . t)
     (python . t)
     (sql . t))))

(setq org-confirm-babel-evaluate nil) ; remove the need for confirmation when evaluating code with Babel

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline myinbox "Inbox")
         "* TODO %?\n")
        ("m" "Master's Todo" entry (file+headline mymasters "Masters Inbox")
         "* TODO %?\n")
        ("j" "Journal" entry (file+olp+datetree myjournal)
         "* %?\nEntered on %U\n  %i\n  %a")))

(add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package org-drill
:config
(setq org-drill-maximum-items-per-session nil
      org-drill-spaced-repetition-algorithm 'sm2))

;;(setq org-agenda-start-with-log-mode t)
;;(setq org-agenda-skip-scheduled-if-done t)
;;(setq org-agenda-skip-deadline-if-done t)
;;(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

(setq org-agenda-files (list myinbox mypersonal mywork myprojects mymasters))

(setq org-refile-targets '((nil :maxlevel . 1)
                           (mypersonal :maxlevel . 1)
                           (mywork :maxlevel . 1)
                           ))

(setq org-archive-location "~/org/gtd/archive.org::* From %s")

(use-package org-super-agenda
  :config
  (org-super-agenda-mode 1)
  (setq org-agenda-span 'day)

  (setq org-agenda-custom-commands
        '(("u" "Super view"
           ((agenda "" ((org-super-agenda-groups
                         '(
                           (:name "Important"
                                  :priority>= "B"
                                  :order 0)
                           (:name "Personal Habits"
                                  :and (:file-path "personal" :habit t)
                                  :order 20)
                           (:name "Late tasks"
                                  :deadline past
                                  :scheduled past
                                  :order 30)
                           (:name "Regular for today"
                                  :time-grid t
                                  :date today
                                  :deadline  today
                                  :scheduled today
                                  :order 40)
                           (:name "Future"
                                  :scheduled future
                                  :order 50)
                           )))))))))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(vertico-mode)

(require 'spacious-padding)

;; These are the default values, but I keep them here for visibility.
(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
(setq spacious-padding-subtle-frame-lines
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border))

;; Make the underlines appear below the base line, to create a more
;; consistent effect between overlines and underlines.
(setq x-underline-at-descent-line t)

(spacious-padding-mode 1)

;; Set a key binding if you need to toggle spacious padding.
(define-key global-map (kbd "<f8>") #'spacious-padding-mode)

(add-hook 'emacs-startup-hook
            (lambda ()
              ;; Open your Org Super Agenda view
              (org-agenda nil "u")
              ;; Ensure only one window is open
              (delete-other-windows)))
              ;; Bury *scratch* buffer if it exists
;;              (when (get-buffer "*scratch*")
;;                (bury-buffer "*scratch*"))))
