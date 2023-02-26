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

;;(corvus-message (quote (:eval (trim-buffer-name "1011_capacity_to_ship_packages_within_d_days.py"))))
(corvus-message (trim-buffer-name "1011_capacity_to_ship_packages_within_d_days.py"))

(provide 'corvus-utilities)
;;; corvus-utilities.el ends here.
