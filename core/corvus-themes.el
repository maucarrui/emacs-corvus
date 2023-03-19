;;; corvus-themes.el --- Corvus' themes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Nord theme configurations.                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-up-nord-theme ()
  "Sets up the nord theme with some personal modifiers."
  (load-theme 'nord t)
  ;; Brighter comments.
  (set-face-foreground 'font-lock-doc-face "MediumOrchid3")
  (set-face-foreground 'font-lock-comment-face "MediumOrchid2")
  (set-face-foreground 'font-lock-comment-delimiter-face "MediumOrchid2")

  ;; Handle diff highlights.
  (set-face-attribute 'diff-hl-insert nil
                      :background nil
                      :foreground "#a3be8c")

  (set-face-attribute 'diff-hl-delete nil
                      :background nil
                      :foreground "#bf616a")

  (set-face-attribute 'diff-hl-change nil
                      :background nil
                      :foreground "#ebcb8b")
  
  ;; Highlight current number line with a bright orange.
  (set-face-attribute 'line-number-current-line nil 
		      :weight 'bold
		      :foreground "tan1")
  
  ;; Make the cursor a bright orange.
  (set-cursor-color "tan1")

  ;; Make selected text a lighter orange.
  (set-face-attribute 'region nil 
		      :background "burlywood1" 
		      :foreground "#ffffff"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set up a theme.                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-up-nord-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; End of configurations.                                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'corvus-themes)

;;; corvus-themes.el ends here.
