;; erlang-sinan.el --- Functions for launching sinan commands.

;; Author:   Dave Peticolas
;; Version:  0.1
;; Keywords: erlang, sinan, erlware
;; Created:  2007-09-18
;; Date:     2008-01-01


(require 'compile)


(defun erlang-sinan-run-command (command)
  "Run sinan with the command given as an argument. Runs sinan
in a separate process asynchronously with output going to the
buffer `*sinan*'."
  (interactive)
  (save-some-buffers)
  (compile-internal command "No more errors." "sinan"))

(defun erlang-sinan-build ()
  (interactive)
  (erlang-sinan-run-command "sinan"))
