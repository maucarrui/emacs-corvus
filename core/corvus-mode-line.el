;;; corvus-mode-line.el --- Corvus' mode line specifications.
;;; Commentary:
;; Core mode line appearance.

;; Indicate line and column at the modeline.
(column-number-mode t)

;; Disable window scroll percentage.
(setq mode-line-percent-position nil)

;; Trim buffer names when they're too long.
(setq-default mode-line-buffer-identification
              (quote
                (:eval (trim-buffer-name (buffer-name)))))

;; Diminish the following minor modes.
(diminish 'super-save-mode)
(diminish 'eldoc-mode)
(diminish 'smartparens-mode)
(diminish 'anzu-mode)
(diminish 'flyspell-mode)

(provide 'corvus-mode-line)

;;; corvus-mode-line.el ends here
