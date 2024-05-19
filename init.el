;; Package installation and loading
(load (locate-user-emacs-file "init-packages.el"))

;; Inhibit the startup message
(setq inhibit-startup-message t)

;;
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Change the bell from sound to a visual one
(setq visible-bell t)

;; Enable the display of the line numbers globally
(global-display-line-numbers-mode 1)

;; Enable highlighting of the current line
(global-hl-line-mode t)

;; Disable the cursor blinking
(blink-cursor-mode -1)

;; Disable the vertical scrollbar
(scroll-bar-mode -1)

;; Remember recently edited files
(recentf-mode 1)

;; Sets the history items to 25
(setq history-length 25)

;; Remember minibuffer prompt history
(savehist-mode 1)

;; Remember the last place you visited in a file
(save-place-mode 1)

;; Tell Emacs to write customizable variables on another file
;; so that this init.el file don't get polluted
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Prevent using UI dialogs for prompts
(setq use-dialog-box nil)

;; Automatically revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Load zenburn theme and confirm that it is safe
(load-theme 'zenburn t)

;; Configure the incremental completion and selection
;; narrowing Helm framework
;;
;; 2023-12-27: just discovered this framework and tested
;; helm-find-files, helm-M-x and helm-show-kill-ring only
;;
(use-package helm
  :diminish
  :bind (("C-h a"   . helm-apropos)
         ("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-m" . helm-M-x)
         ("C-x m"   . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x r l" . helm-filtered-bookmarks)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x i"   . helm-imenu)
         ("M-y"     . helm-show-kill-ring)
         ("M-i"     . helm-swoop-without-pre-input)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)))

;; Disable line numbers for some modes
;; global-display-line-numbers-mode
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Load and configure the rainbow-delimiters package to improve
;; the readability of multiple parenthesis in programming
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Displays available keybindings some time after pressing
;; key combinations like C-x
(require 'which-key)
(which-key-mode)

;; Configuration for the helpful package, a better Emacs *help* buffer.
;;
;;; Note that the built-in `describe-function' includes both functions
;;; and macros. `helpful-function' is functions only, so we provide
;;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
;;; Lookup the current symbol at point. C-c C-d is a common keybinding
;;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
;;; Look up *F*unctions (excludes macros).
;;;
;;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Function to setup the org mode
;;  org-indent-mode prefixes text lines with virtual spaces to align vertically;
;;  variable-pitch-mode remaps the default face to the variable-pitch defined
;; with set-face-attribute;
;;  visual-line-mode wrap the line at word boiundaries near the right window edge;
(defun nm/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Replace org-mode bullets with better ones
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Define functions 
(defvar nm/default-font-size 180)
(defvar nm/default-variable-font-size 180)

;; Set the default face
(set-face-attribute 'default nil :font "Fira Code Retina" :height nm/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height nm/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Lucida Grande" :height nm/default-variable-font-size :weight 'regular)

;; Define a function to setup org-mode font
(defun nm/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  ;; Change the font size of the different org-mode header levels
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Lucida Grande" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defcustom my/path-aliases
  (list :emacs    "~/.emacs.d"
        :org      "~/Documents/org"
	:botafogo "~/Documents/org/botafogo"
	:personal "~/Documents/org/personal")
  "Location of my paths for ease of usage. Customize for each
  environment if needed.")

(defun my/path (dir &optional subpath)
  "Build a path name. See https://github.com/arecker/emacs.d"
  (let ((dir (file-name-as-directory
              (cl-getf my/path-aliases dir
                       (format "~/%s" dir))))
        (subpath (or subpath "")))
    (concat dir subpath)))

;(defcustom main-agenda (my/path :org "inbox.org")
;  "This is used to store quickly todo items without refiling")

(setq
 org-agenda-files (list (my/path :org)
			(my/path :botafogo "1:1.org")
			(my/path :botafogo "agenda.org")
			(my/path :botafogo "meetings.org")
			(my/path :personal "agenda.org")
			))
 
;; org-mode package configuration
(use-package org
  :hook (org-mode . nm/org-mode-setup)
  :config
  (setq org-agenda-span 'day)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (set-face-underline 'org-ellipsis nil)
  (nm/org-font-setup))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Define a function to configure the visual-fill-column on org-mode
(defun nm/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Uses the visual-fill-column package on org-mode
(use-package visual-fill-column
  :hook (org-mode . nm/org-mode-visual-fill))

;; org-super-agenda package configuration
(use-package org-super-agenda
;  :straight t
  :config
  (org-super-agenda-mode 1))
