;; Initialize package sources
(require 'package)

;; Adds Melpa repo to the package-archives list
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; List the packages to be installed
(setq package-list
      '(helm
	helm-org-rifle
	helpful
	org-bullets
;        org-roam
        org-super-agenda
        rainbow-delimiters
	visual-fill-column
	which-key
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
