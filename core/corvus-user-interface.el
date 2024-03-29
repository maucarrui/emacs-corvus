;;; user-interface.el --- Corvus' user interface specifications.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User Interface Configurations.                                         ;;;;
;;;;   - Disable start-up screen.                                           ;;;;
;;;;   - Start full-screen always.                                          ;;;;
;;;;   - Change font size depending on the screen size.                     ;;;;
;;;;   - Disable tool, menu, and scrolling bar.                             ;;;;
;;;;   - Display line numbers.                                              ;;;;
;;;;   - Make the fringe the same color as the background.                  ;;;;
;;;;   - Always truncate long lines.                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)

(if (> (display-pixel-width) 1600)
    (set-face-attribute 'default (selected-frame) :height 120)
  (set-face-attribute 'default (selected-frame) :height 110))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(global-display-line-numbers-mode)

(set-face-attribute 'fringe nil :background nil)

(setq-default truncate-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indentation Highlights Configurations.                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq-default highlight-indent-guides-method 'character)
(setq-default highlight-indent-guides-responsive 'top)
(setq-default highlight-indent-guides-delay 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Git Diff Highlights Configurations.                                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diff-hl)

(let ((bitstr (fringe-helper-convert
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX"
               "........XX")))
  (define-fringe-bitmap 'my-diff-hl-bitmap bitstr nil nil 'center))
(setq diff-hl-fringe-bmp-function (lambda (type pos) 'my-diff-hl-bitmap))

(setq diff-hl-flydiff-delay 0.05)
(diff-hl-flydiff-mode)
(global-diff-hl-mode)

;; Update buffer appearance after committing with Magit.
(with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Enable diff-hl in dired mode.
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End of configurations.                                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'corvus-user-interface)

;;; corvus-user-interface.el ends here.
