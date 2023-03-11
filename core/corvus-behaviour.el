;;; corvus-behaviour.el --- Corvus' behaviour configurations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Backups and Auto-saves configurations.                                 ;;;;
;;;;     - Store backup files in the temporal directory (/tmp).             ;;;;
;;;;     - Store auto-save files in the temporal directory (/tmp).          ;;;;
;;;;     - Revert buffers automatically when underlying files are           ;;;;
;;;;       changed externally.                                              ;;;;
;;;;     - Automatically save buffers associated with files when            ;;;;
;;;;       switching to another window.                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(require 'super-save)
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Uniquify configurations.                                               ;;;;
;;;;     - Meaningful names for buffers with the same name.                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Window movement configurations.                                        ;;;;
;;;;     - Use Shift + Arrow Keys to switch between windows.                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'windmove)
(windmove-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compilation configurations                                             ;;;;
;;;;     - Save before compiling.                                           ;;;;
;;;;     - Kill old compile process when executing a new one.               ;;;;
;;;;     - Point to the first error.                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)
(setq compilation-ask-about-save nil        
      compilation-always-kill t             
      compilation-scroll-output 'first-error
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Spell-checking configurations.                                         ;;;;
;;;;     - Use Hunspell.                                                    ;;;;
;;;;     - Capable of detecting multiple languages at the same time.        ;;;;
;;;;     - Current languages: English (GB, US), Spanish (MX).               ;;;;
;;;;     - Save non-included words to a personal dictionary.                ;;;;
;;;;     - Configure spell-checking on texts and programming modes.         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flyspell)
(with-eval-after-load "ispell"
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_GB,en_US,es_MX")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,en_US,es_MX")
  (setq ispell-personal-dictionary "~/.hunspell_personal")
  )

(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Which-function configurations.                                         ;;;;
;;;;     - Show current function the pointer is on while programming.       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq which-func-cleanup-function #'trim-function-name)
(add-hook 'prog-mode-hook #'which-function-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto-completion configurations.                                        ;;;;
;;;;     - Show current function the pointer is on while programming.       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook #'company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End of configurations.                                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'corvus-behaviour)

;;; corvus-behaviour.el ends here
