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
(defvar *erlang-compile-command* "erlc")
(defvar *erlang-ast-function-expression* "^\{function,\s*\\([a-zAb-Z0-9_]*\\),\s\\([0-9]*\\),\s[0-9]*")

(defun erlang-exports-build-exports (new-buffer-name)
  "gathers exports from the Erlang AST"
  (let ((exports-list (list))
        (lines (count-lines (point-min) (point-max))))
    (goto-line 0)
    (beginning-of-line)
    (dotimes (line-num lines)
      (let* ((line-limit (line-end-position))
             (expression-match (re-search-forward *erlang-ast-function-expression* line-limit t)))
        (when expression-match
          (let ((matched-string 
                 (concat (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                         "/"
                         (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))
            (push matched-string exports-list))))
      (forward-line 1)
      (beginning-of-line))
    (goto-line 0)
    (beginning-of-line)
    (set-buffer (get-buffer-create new-buffer-name))
    (let* ((ordered-export-list (reverse exports-list))
           (oel-length (length ordered-export-list)))
      (when (> oel-length 0)
        (insert "-export([\n")
        (dolist (export ordered-export-list)
          (unless (string-match "module_info" export)
            (insert (concat "\t" export (if (> (decf oel-length) 2) ",\n" "")))))
        (insert "\n]).\n")))))


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
    (let* ((ast-file-name (concat source-bare-file ".S"))
           (new-buffer-name (concat "*erlang-exports-" ast-file-name "*")))
      (save-current-buffer
        (setq erlang-exports-process 
              (apply 'start-process
                     "erlang-exports-process" 
                     (get-buffer-create "*erlang-exports*") 
                     *erlang-compile-command* 
                     (build-command-line (buffer-file-name) ast-file-name) ))
        (push (cons buffer-file-name ast-file-name) *erlang-exports-files*)
        (set-process-sentinel erlang-exports-process 'erlang-exports-process-sentinel)))))

(defun build-command-line (source-file-name out-file-name)
  `("-S" ,source-file-name "-o " ,out-file-name))

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
