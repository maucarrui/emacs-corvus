;;; corvus-mode-line.el --- Corvus' mode line specifications.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mode-line configurations.                                              ;;;;
;;;;   - Indicate the line and column the pointer in located at.            ;;;;
;;;;   - Disable window scroll percentage.                                  ;;;;
;;;;   - Trim the buffer name when its too long.                            ;;;;
;;;;   - Diminish the following minor modes:                                ;;;;
;;;;       * Super Save mode.                                               ;;;;
;;;;       * ElDoc mode.                                                    ;;;;
;;;;       * Smart-parenthesis mode.                                        ;;;;
;;;;       * Anzu mode.                                                     ;;;;
;;;;       * Fly-Spell mode.                                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode t)

(setq mode-line-percent-position nil)

(setq-default mode-line-buffer-identification
              (quote
                (:eval (trim-buffer-name (buffer-name)))))

(diminish 'super-save-mode)
(diminish 'eldoc-mode)
(diminish 'smartparens-mode)
(diminish 'anzu-mode)
(diminish 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End of configurations.                                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'corvus-mode-line)

;;; corvus-mode-line.el ends here
