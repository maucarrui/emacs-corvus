;;; corvus-editor.el --- Corvus' core editing configurations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Main configurations.                                                   ;;;;
;;;;     - Set fill-paragraph to 80 characters.                             ;;;;
;;;;     - Don't use tabs to indent.                                        ;;;;
;;;;     - The default value of the tab-size is 8 characters.               ;;;;
;;;;     - Delete the selected text with a key press.                       ;;;;
;;;;     - Insert a newline at the end of the file.                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default fill-column 80)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(delete-selection-mode t)

(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scrolling configurations.                                              ;;;;
;;;;     - Number of lines at top and bottom of the window.                 ;;;;
;;;;     - Perform an automatic scrolling when the pointer is out of        ;;;;
;;;;       the visible portion of the text.                                 ;;;;
;;;;     - Preserve point position when scrolling.                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Smart parenthesis configurations.                                      ;;;;
;;;;     - Automatically insert parenthesis, brackets and other pairs.      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smartparens-config)
(smartparens-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Search/Replace configurations.                                         ;;;;
;;;;     - Display the current amount of occurrences of a word.             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anzu)
(global-anzu-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End of configurations.                                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'corvus-editor)

;;; corvus-editor.el ends here
