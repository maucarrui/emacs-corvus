;;; corvus-golang.el --- Golang functionality mode.

;; Package dependencies for Golang functionality.
(defvar golang-dependencies
  '(go-mode))

;; Set up Golang configurations.
(corvus-message "Installing Golang dependencies...")
(mapc #'corvus-install-package golang-dependencies)
(corvus-message "Setting up Golang configurations...")
(require 'go-mode)

(add-hook 'go-mode-hook
          (lambda ()
            ;; Set tab-width to two.
            (setq tab-width 2)

            ;; Ignore naming convention for Flyspell (such as camel-case and
            ;; pascal-case).
            (setq flyspell-generic-check-word-predicate
                  'flyspell-generic-progmode-ignore-naming-conventions)

            ;; Disable company auto-completion on comments.
            (delete 'company-dabbrev company-backends)

            ;; Run the go formatting program (gofmt) before saving.
            (add-hook 'before-save-hook 'gofmt-before-save)))

(provide 'corvus-golang)

;;; corvus-golang.el ends here.
