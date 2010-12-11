;; erlang-exports.el -- on-the-fly exports generator

;; Author:  Christopher Brown (skeptomai) <skeptomai@opscode.com>
;; Maintainer: Christopher Brown (skeptomai) <skeptomai@opscode.com>

;;; Commentary:
;;
;; Runs an erlang source file through the Erlang compiler 'erlc'
;; with the -S option, dumping the abstract syntax tree
;; We then read that tree, looking for functions and their arity

;; Version:  0.1
;; Keywords: erlang, languages, processes
;; Date:     2009-07-26

;; The contents of this file are subject to the Erlang Public License,
;; Version 1.1, (the "License"); you may not use this file except in
;; compliance with the License. You should have received a copy of the
;; Erlang Public License along with this software. If not, it can be
;; retrieved via the world wide web at http://www.erlang.org/.

;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;; the License for the specific language governing rights and limitations
;; under the License.

;;; Code:

(defvar *erlang-exports-files* (list)
  "association list of source files to their AST files")
(defvar *erlang-ast-command* (concat (file-name-directory (locate-library "erlang")) "compile-export"))
(defvar *erlang-ast-function-expression* "^\{function,\s*\\([a-zAb-Z0-9_]*\\),\s\\([0-9]*\\),\s[0-9]*")
(defvar *erlang-ast-exports-declaration-expression*
         "^\{exports,\s+\\[\s*") ;; Find the exports declaration
(defvar *erlang-ast-export-expression* "\s*\{\\([a-z_0-9]*\\),\\([0-9]+\\)\},?\\\n?")
(defvar *erlang-full-exports-expression* (concat *erlang-ast-exports-declaration-expression* "\\(" *erlang-ast-export-expression* "\\)*\\]\\}\\.\\\n"))

(defun match-exports ()
  (interactive)
  (beginning-of-buffer)
  (let ((end-point (re-search-forward *erlang-full-exports-expression* nil 1))
        (exports nil))
    (if end-point
        (progn
          (setf (point) (- (point) (length (match-string-no-properties 0))))
          (while (re-search-forward *erlang-ast-export-expression* end-point 1)
            (let ((fn-name (match-string-no-properties 1))
                  (fn-arity (match-string-no-properties 2)))
              (push (cons fn-name fn-arity) exports)))
          (reverse exports))
      nil)))

(defun erlang-exports-build-exports (new-buffer-name)
  "gathers exports from the Erlang AST"
  (let* ((exports (match-exports))
         (oel-length (length exports)))
    (insert (format "%s" (length exports)))
    (forward-line 1)
    (beginning-of-line)
    (set-buffer (get-buffer-create new-buffer-name))
    (when (> oel-length 0)
      (progn
        (insert "-export([")
        (dolist (export (mapcar #'(lambda (x) (concat (format "%s/%s" (car x) (cdr x)) (if (> (decf oel-length) 2) "," ""))) exports))
          (unless (string-match "module_info" export)
            (insert export)))
        (insert "]).\n")))))

;; get the buffer-file-name for the erlang file
;; get the directory
;; get the file name bare and extension
;; change to that directory
;; call erlc -S filename
;; load and parse "bare file name".S

(defun erlang-exports ()
  "finds all erlang functions and creates an appropriate export statement with fn name and arity"
  (interactive)
  (multiple-value-bind (source-file-directory source-bare-file)
      (source-directory-and-file (buffer-file-name))
    (let* ((ast-file-name (concat source-bare-file ".S")))
      (save-current-buffer
        (push (cons buffer-file-name ast-file-name) *erlang-exports-files*)
        (setq erlang-exports-process 
              (apply 'start-process
                     "erlang-exports-process" 
                     (get-buffer-create "*erlang-exports*") 
                     *erlang-ast-command* 
                     (list (file-name-sans-extension (buffer-file-name)))))
        (set-process-sentinel erlang-exports-process 'erlang-exports-process-sentinel)))))

(defun erlang-exports-process-sentinel (process event)
  (let* ((current-file-name (buffer-file-name))
         (ast-file-name (cdr (assoc current-file-name *erlang-exports-files*)))
         (new-buffer-name (concat "*erlang-exports-" ast-file-name "*"))
         (new-buffer-point nil))
    (with-temp-buffer new-buffer-name
      (insert-file-contents ast-file-name)
      (erlang-exports-build-exports new-buffer-name)
      (setq new-buffer-point (point)))
    (insert-buffer-substring new-buffer-name (point-min) new-buffer-point)
    (kill-buffer new-buffer-name)))

(defun source-directory-and-file (source-file-name)
  "separate the file name from the directory, call with buffer-file-name"
  (let* ((source-file-directory (file-name-directory source-file-name)))
    (string-match source-file-directory (file-name-sans-extension source-file-name))
    (values source-file-directory (substring (file-name-sans-extension source-file-name) (match-end 0)))))

(provide 'erlang-exports)
