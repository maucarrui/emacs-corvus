;;; corvus-utilities.el --- Corvus' utility functions.
;;; Commentary:
;; Some utility functions related to Corvus.
(require 'benchmark)

(defun corvus-message (msg)
  "Corvus prints the contents of a message"
  (message (concat "[CORVUS] - " msg)))

(defun message-function-time (fun msg)
  "Execute a function and print a message with the time it took
the function to execute."
  (let* ((elapsed-time (benchmark-elapse fun))
	 (report-msg (format (concat msg " [%.3f s]") elapsed-time)))
    (corvus-message report-msg)))

(defvar file-separators "\\/"
  "Buffer separators such as '/'.")

(defvar function-separators "\\."
  "Function separators such as '.'.")

(defun get-proper-function-name (fun-path)
  "Given a path of a function (e.g. A.B.C.foo), return the proper
name of the function, i.e. 'foo'."
  (car (last (split-string fun-path function-separators)))
  )

(defun get-proper-file-name (file-path)
  "Given a path of a file (e.g. A/B/C/foo.c), return the proper
name of the function, i.e. 'foo.c'."
  (car (last (split-string file-path file-separators)))
  )

(defun trim-function-name (fun-path)
  "Given a function path, trim it so that it only displays the
first 20 characters of the function name."
  (let* ((fun-name (get-proper-function-name fun-path)))
    (if (> (length fun-name) 20)
        (concat (truncate-string-to-width fun-name 17) "...")
      fun-name)))

(defun trim-buffer-name (file-path)
  "Given a buffer name, trim it so that it only displays the first
20 characters."
  (let* ((file-name (get-proper-file-name file-path)))
    (if (> (length file-path) 20)
        (concat (truncate-string-to-width file-name 9)
                "..."
                (substring file-name (- (length file-path) 8)))
      file-name)))

(defun fringe-helper-convert (&rest strings)
"Convert STRINGS into a vector usable for `define-fringe-bitmap'.
Each string in STRINGS represents a line of the fringe bitmap.
Periods (.) are background-colored pixel; Xs are foreground-colored. The
fringe bitmap always is aligned to the right. If the fringe has half
width, only the left 4 pixels of an 8 pixel bitmap will be shown.
For example, the following code defines a diagonal line.
\(fringe-helper-convert
\"XX......\"
\"..XX....\"
\"....XX..\"
\"......XX\"\)"
  (unless (cdr strings)
  ;; only one string, probably with newlines
    (setq strings (split-string (car strings) "\n")))
  (apply 'vector
    (mapcar
      (lambda (str)
        (let ((num 0))
          (dolist (c (string-to-list str))
            (setq num (+ (* num 2) (if (eq c ?.) 0 1))))
          num))
      strings)))

(defun flyspell-generic-progmode-ignore-naming-conventions ()
  "Ignores sensitive case words (such like pascal-case and
camel-case formats) using 'flyspell-generic-progmode-verify' behaviour."
  (let (
         ;; Enable case-sensitive searches.
         (case-fold-search nil)

         ;; Get the word the pointer is on.
         (str (current-word 'strict))

         ;; Regular Expression for pascal-case matches.
         (pascal-regexp
          (rx (any upper)
              (one-or-more (one-or-more (any lower))
                           (one-or-more (any upper)))))
         
         ;; Regular expression for camel-case matches.
         (camel-regexp
          (rx (any lower)
              (one-or-more (one-or-more (any upper))
                           (zero-or-more (any lower))))))
    
    ;; When the current word is misspelled, only correct it if its not pascal or
    ;; camel cased.
    (when (flyspell-generic-progmode-verify)
      (if (or (string-match-p pascal-regexp str)
              (string-match-p camel-regexp str))
          nil
        t))
    ))

(provide 'corvus-utilities)
;;; corvus-utilities.el ends here.
