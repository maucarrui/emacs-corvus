;;; user-interface.el --- Corvus' user interface specifications.
;;; Commentary:
;; Core user interface specifications for Corvus.

;; Disable start-up screen.
(setq inhibit-startup-screen t)

;; Start fullscreen always.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)

;; Change font size accordingly depending on the screen size.
(if (> (display-pixel-width) 1600)
    (set-face-attribute 'default (selected-frame) :height 120)
  (set-face-attribute 'default (selected-frame) :height 110))

;; Disable tool bar, menu bar, and scrolling bar.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Display line numbers.
(global-display-line-numbers-mode)

;; Make the fringe the same colour as the background.
(set-face-attribute 'fringe nil :background nil)

;; Always truncate long lines.
(setq-default truncate-lines t)

(provide 'corvus-user-interface)

;;; corvus-user-interface.el ends here.
