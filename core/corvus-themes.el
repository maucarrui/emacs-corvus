;;; corvus-themes.el --- Corvus' themes.
;;; Commentary:
;; The main themes available for corvus. Each theme corresponds to a season of
;; the year:
;; Winter - Nord theme.

(defun set-up-nord-theme ()
  "Sets up the nord theme with some personal modifiers."
  (load-theme 'nord t)
  ;; Brighter comments.
  (set-face-foreground 'font-lock-doc-face "MediumOrchid3")
  (set-face-foreground 'font-lock-comment-face "MediumOrchid2")
  (set-face-foreground 'font-lock-comment-delimiter-face "MediumOrchid2")
  
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

(set-up-nord-theme)

(provide 'corvus-themes)

;;; corvus-themes.el ends here.
