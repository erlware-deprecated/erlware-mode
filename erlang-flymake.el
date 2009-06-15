;; erlang-flymake.el --- Flymake support.

;; Author:   Mats Cronqvist
;; Version:  0.1
;; Keywords: erlang, flymake
;; Created:  2009-05-14

;; based on snippets stolen from 
;; http://www.emacswiki.org/emacs/FlyMake
;; and
;; http://www.emacswiki.org/emacs/FlymakeErlang

;; to enable, put something like this in your erlang-mode-hook
;;   (flymake-mode)
;;   (local-set-key (kbd "M-'") 'erlang-flymake-next-error)

(if (locate-library "flymake")
    (progn
      (require 'flymake)
      (defun flymake-tmp-filename(filename prefix)
        (let*
            ((tmp-dir (concat (getenv "HOME") "/.erlang-flymake"))
             (tmp-name (file-name-nondirectory filename))
             (tmp-file (concat tmp-dir "/" tmp-name)))
          (flymake-log 3 "made temp-file: %s" tmp-file)
          tmp-file))
      (defun erlang-flymake-init ()
        "Set up the command used to parse our buffer"
        (let* ((erlang-dir (file-name-directory (locate-library "erlang")))
               (temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-tmp-filename)))
          (list (concat erlang-dir "flymaker.sh") 
                (list temp-file default-directory))))
      (defun flymake-siblicide()
        "Kill all next-error capable buffers."
        (condition-case nil
            (progn (kill-buffer (next-error-find-buffer))
                   (flymake-siblicide))
          (error nil)))
      (defun erlang-flymake-next-error ()
        "Goto next error, if any. Display error in mini-buffer."
        (interactive)
        (flymake-siblicide)
        (let ((err-buf nil))
          (condition-case err
              (setq err-buf (next-error-find-buffer))
            (error))
          (if err-buf
              (next-error)
            (progn
              (flymake-goto-next-error)
              (let ((err (get-char-property (point) 'help-echo)))
                (when err
                  (message err)))))))
      ;; add our init file to flymake's alist of filename regexps
      (add-to-list 
       'flymake-allowed-file-name-masks
       '(".+\\.erl$" erlang-flymake-init))))
