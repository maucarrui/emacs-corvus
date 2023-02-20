;;; init.el --- Corvus' configuration entry point.
;;
;; Copyright (c) 2023 Mauricio Carrasco Ruiz
;;
;; Author: Mauricio Carrasco Ruiz
;; URL: https://github.com/maucarrui
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This file sets up all the default configurations and modules of Corvus.

(require 'benchmark)

;; Variable to keep track of the configuration files of Corvus.
(defvar corvus-root-dir (file-name-directory load-file-name)
  "The root directory of Corvus.")

(defvar corvus-core-dir (expand-file-name "core" corvus-root-dir)
  "The directory which contains Corvus' core configurations.")

;; Corvus will print the time it takes to set up.

(defun corvus-message (msg)
  "Corvus prints the contents of a message"
  (message (concat "[CORVUS] - " msg)))

(defun message-function-time (fun msg)
  "Execute a function and print a message with the time it took
the function to execute."
  (let* ((elapsed-time (benchmark-elapse fun))
	 (report-msg (format (concat msg " [%.3f s]") elapsed-time)))
    (corvus-message report-msg)))

(defun set-up-corvus ()
  "Load all configuration files."
  (corvus-message "Loading configurations...")
  (corvus-message "Loading packages...")
  (require 'corvus-packages)
  (corvus-message "Setting up behaviour...")
  (require 'corvus-behaviour)
  (corvus-message "Setting up editor...")
  (require 'corvus-editor)
  (corvus-message "Setting up keybindings...")
  (require 'corvus-keybindings)
  (corvus-message "Setting up user interface...")
  (require 'corvus-user-interface)
  (corvus-message "Setting up mode line...")
  (require 'corvus-mode-line)
  (corvus-message "Setting up themes...")
  (require 'corvus-themes))

(defun initialize-corvus ()
  "Initialize Corvus, indicating the time it took to set up."
  (corvus-message "Starting...")
  (add-to-list 'load-path corvus-core-dir)
  (setq custom-file (expand-file-name "custom.el" corvus-root-dir))
  (message-function-time (set-up-corvus)
			 "Finished setting up."))

(initialize-corvus)

;;; init.el ends here
