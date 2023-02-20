;;; corvus-packages.el --- Corvus' package dependencies.
;;; Commentary:
;; Main dependencies on other packages. It also takes care of the automatic
;; installation of all packages required by Corvus.

(require 'cl-lib)
(require 'package)

;; Add MELPA to the trusted package archives.
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; List of packages dependencies:
;; - Diminish
;;     Properly hide minor modes from the mode line.
;; - Expand Region
;;     Smart expanding System, useful to select words and functions.
;; - Magit
;;     Git interface in Emacs.
;; - Multiple Cursors
;;     Allows the editor to have multiple cursors at once.
;; - Nord Theme
;;     Cold theme for the editor.
;; - Smartparens
;;     Smart parenthesis minor mode.
;; - Super Save
;;     Smart save functions (e.g. save when switching buffers).
;; - Windmove
;;     Move between windows using Shift + arrow keys.
(defvar corvus-main-packages
      '(diminish
	expand-region
        magit
	multiple-cursors
	nord-theme
	smartparens
	super-save
	windmove)
      "List of main dependencies of Corvus")

(defun corvus-install-package (package)
  "Install the given package when necessary."
  (if (package-installed-p package)
      (corvus-message (format "Package %s is already installed." package))
    (package-install package)
    (corvus-message (format "Installed package %s." package))))

(defun corvus-install-packages ()
  "Install all the dependencies of Corvus"
  (corvus-message "Initializing packages...")
  (package-initialize)
  (corvus-message "Installing dependencies...")
  (mapc #'corvus-install-package corvus-main-packages))

(corvus-install-packages)

(provide 'corvus-packages)

;;; packages.el ends here
