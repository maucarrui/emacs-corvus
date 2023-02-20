;;; corvus-editor.el --- Corvus' core editing configurations.
;;; Commentary:
;; Core editing refinements specified for Corvus.

;; Don't use tabs to indent but maintain correct appearance.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; Delete the selection with a keypress.
(delete-selection-mode t)

;; Newline at the end of file.
(setq require-final-newline t)

;; Scrolling.
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Smart parenthesis.
(require 'smartparens-config)
(smartparens-mode t)

(provide 'corvus-editor)

;;; corvus-editor.el ends here
