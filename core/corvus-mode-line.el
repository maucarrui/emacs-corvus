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

(setq-default
 mode-line-format
 (list " "
       " " 'mode-line-mule-info
       'mode-line-modified
       " " 'mode-line-buffer-identification
       '(:eval (when locked-buffer-mode
                 (propertize " [Locked]" 'face '(:foreground "#ebcb8b"))))
       " " 'mode-line-modes
       " " '(vc-mode vc-mode)
       " "
       '(:eval (concat
                "(%l, %c) ["
                (format "%d" (/ (* (line-number-at-pos) 100)
                                (count-lines (point-min) (point-max))))
                "%%]"))
       'mode-line-end-spaces
       ))
 
(diminish 'super-save-mode)
(diminish 'eldoc-mode)
(diminish 'smartparens-mode)
(diminish 'anzu-mode)
(diminish 'flyspell-mode)
(diminish 'wucuo-mode)
(diminish 'locked-buffer-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (diminish 'company-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End of configurations.                                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'corvus-mode-line)

;;; corvus-mode-line.el ends here
