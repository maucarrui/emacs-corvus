;;; corvus-keybindings.el --- Corvus' defined keybindings.
;;; Commentary:
;; Core keybindings defined for Corvus.

;; Ignore redefinitions warnings.
(set 'ad-redefinition-action 'accept)

;; Multiple cursors.
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; Select words.
(require 'expand-region)
(global-set-key (kbd "S-SPC") 'er/expand-region)

(provide 'corvus-keybindings)

;;; corvus-keybindings.el ends here
