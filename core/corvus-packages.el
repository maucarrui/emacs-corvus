;;; corvus-packages.el --- Corvus' package dependencies.
;;; Commentary:
;; Main dependencies on other packages. It also takes care of the automatic
;; installation of all packages required by Corvus.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package configurations.                                                ;;;;
;;;;   - Add MELPA to the trusted package archives.                         ;;;;
;;;;   - List of packages dependencies:                                     ;;;;
;;;;       * Anzu                                                           ;;;;
;;;;         Show number of matches in mode-line while searching.           ;;;;
;;;;       * Company                                                        ;;;;
;;;;         Auto-completion framework for Emacs.                           ;;;;
;;;;       * Crux                                                           ;;;;
;;;;         A Collection of ridiculously useful extensions.                ;;;;
;;;;       * Diminish                                                       ;;;;
;;;;         Properly hide minor modes from the mode line.                  ;;;;
;;;;       * Expand Region                                                  ;;;;
;;;;         Smart expanding System, useful to select words and             ;;;;
;;;;         functions.                                                     ;;;;
;;;;       * Flycheck                                                       ;;;;
;;;;         Spell-checking on the fly as one types.                        ;;;;
;;;;       * Magit                                                          ;;;;
;;;;         Git interface in Emacs.                                        ;;;;
;;;;       * Multiple Cursors                                               ;;;;
;;;;         Allows the editor to have multiple cursors at once.            ;;;;
;;;;       * Nord Theme                                                     ;;;;
;;;;         Cold theme for the editor.                                     ;;;;
;;;;       * Smartparens                                                    ;;;;
;;;;         Smart parenthesis minor mode.                                  ;;;;
;;;;       * Super Save                                                     ;;;;
;;;;         Smart save functions (e.g. save when switching buffers).       ;;;;
;;;;       * Windmove                                                       ;;;;
;;;;         Move between windows using Shift + arrow keys.                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defvar corvus-main-packages
  '(anzu
    company
    crux
    diff-hl
    diminish
    dumb-jump
    expand-region
    flycheck
    highlight-indent-guides
    magit
    markdown-mode
    multiple-cursors
    nord-theme
    smartparens
    super-save
    wucuo
    windmove)
  "List of main dependencies of Corvus")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Install Dependencies.                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'package)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End of configurations.                                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'corvus-packages)

;;; packages.el ends here
