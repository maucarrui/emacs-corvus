;;; corvus-mode-line.el --- Corvus' mode line specifications.
;;; Commentary:
;; Core mode line appearance.

;; Indicate line and column at the modeline.
(column-number-mode t)

;; Disable window scroll percentage.
(setq mode-line-percent-position nil)

;; Diminish the following minor modes.
(diminish 'super-save-mode)
(diminish 'eldoc-mode)
(diminish 'smartparens-mode)

(provide 'corvus-mode-line)

;;; corvus-mode-line.el ends here
