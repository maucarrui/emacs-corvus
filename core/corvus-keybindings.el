;;; corvus-keybindings.el --- Corvus' defined keybindings.

(set 'ad-redefinition-action 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Multiple Cursors.                                                      ;;;;
;;;;     - "Control + Shift + c Control + Shift + c"                        ;;;;
;;;;       Place a cursor at each line of a selected region.                ;;;;
;;;;     - "C->"                                                            ;;;;
;;;;       Place a cursor at the next line or the next word that            ;;;;
;;;;       matches the selection.                                           ;;;;
;;;;     - "C-<"                                                            ;;;;
;;;;       Place a cursor at the previous line or the next word that        ;;;;
;;;;       matches the selection.                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Select Words.                                                          ;;;;
;;;;     - "Shift + Space"                                                  ;;;;
;;;;       Select the current word the pointer is on, press the             ;;;;
;;;;       keybinding again to expand the word.                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'expand-region)
(global-set-key (kbd "S-SPC") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End of configurations.                                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'corvus-keybindings)

;;; corvus-keybindings.el ends here
