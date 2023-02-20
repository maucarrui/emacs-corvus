;;; corvus-leetcode.el --- Leetcode functionality module.

;; Package dependencies for the leetcode functionality.
(defvar leetcode-dependencies
  '(shrface
    leetcode))

;; Set up function for leetcode.
(defun set-up-leetcode ()
  "Set up leetcode functionality."
  (corvus-message "Installing Leetcode dependencies...")
  (mapc #'corvus-install-package leetcode-dependencies)
  (corvus-message "Setting up leetcode configurations...")
  (setq leetcode-prefer-language "python3")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/leetcode")
  ;; Show the problem's description within 80 characters.
  (setq-default shr-max-width 80))

;; Defined function to show a problem given its ID.
(defun leetcode-show-problem-by-id (id)
  "Show the problem given its ID."
  (interactive "nProblem ID: ")
  (leetcode-show-problem id))

(set-up-leetcode)

(provide 'corvus-leetcode)

;;; corvus-leetcode.el ends here
