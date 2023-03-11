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
            (set tab-width 2)

            ;; Run the go formatting program (gofmt) before saving.
            (add-hook 'before-save-hook 'gofmt-before-save)))

(provide 'corvus-golang)

;;; corvus-golang.el ends here.
