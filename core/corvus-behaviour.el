;;; corvus-behaviour.el --- Corvus' behaviour configurations.
;;; Commentary:
;; Core behaviours when dealing with buffers for Corvus.

;; Store all backup and autosaves files in the tmp directory.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

;; Meaningful names for buffers with the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Use Shift + arrow keys to switch between windows.
(require 'windmove)
(windmove-default-keybindings)

;; Automatically save buffers associated with files when switching to another
;; window.
(require 'super-save)
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)

;; Compilation details.
(require 'compile)
(setq compilation-ask-about-save nil         ;; Save before compiling.
      compilation-always-kill t              ;; Kill old compile process.
      compilation-scroll-output 'first-error ;; Move to the first error.
      )

;; Spell checking on buffers.
(require 'flyspell)
(setq ispell-program-name "aspell")

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(provide 'corvus-behaviour)

;;; corvus-behaviour.el ends here
