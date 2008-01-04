;; erlang-sinan.el --- Functions for launching sinan commands.

;; Author:   Dave Peticolas
;; Version:  0.1
;; Keywords: erlang, sinan, erlware
;; Created:  2007-09-18
;; Date:     2008-01-01


(require 'compile)


(defun erlang-sinan-run-command (&optional args)
  "Run sinan with the command given as an argument. Runs sinan
in a separate process asynchronously with output going to the
buffer `*sinan*'."
  (interactive)
  (save-some-buffers)
  (let ((cmd (string-join " " (cons "sinan" args))))
    (compile-internal cmd "No more errors." "sinan")))

(defun erlang-sinan-build ()
  (interactive)
  (erlang-sinan-run-command))

(defun erlang-sinan-clean ()
  (interactive)
  (erlang-sinan-run-command '("clean")))

(defun string-join (joiner strings)
  (string-join-accum joiner strings ""))

(defun string-join-accum (joiner strings accum)
  (cond ((not strings) accum)
        ((not (cdr strings)) (concat accum (car strings)))
        (t (string-join-accum joiner (cdr strings)
                              (concat accum (car strings) joiner)))))
