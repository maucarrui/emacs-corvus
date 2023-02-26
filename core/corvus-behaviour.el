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

(with-eval-after-load "ispell"
  ;; Configure 'LANG', otherwise ispell.el cannot find a 'default-dictionary'
  ;; even though multiple dictionaries will be configured in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure the necessary languages.
  (setq ispell-dictionary "en_GB,en_US,es_MX")
  ;; ispell-set-spellchecker-params has to be called before
  ;; ispell-hunspell-add-multi-dic to work.
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,en_US,es_MX")
  ;; Saving words to the personal dictionary.
  (setq ispell-personal-dictionary "~/.hunspell_personal")
  )

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

;; Spell checking on texts.
(add-hook 'text-mode-hook #'flyspell-mode)

;; Spell checking while programming.
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(provide 'corvus-behaviour)

;;; corvus-behaviour.el ends here
