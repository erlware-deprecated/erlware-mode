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
      (defun erlang-flymake-init ()
        "Set up the command used to parse our buffer"
        (let* ((erlang-dir (file-name-directory (locate-library "erlang")))
               (temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
               (local-file (file-relative-name 
                            temp-file
                            (file-name-directory buffer-file-name))))
          (list (concat erlang-dir "flymaker.sh") (list local-file))))
      (defun erlang-flymake-next-error ()
        "Goto next error, if any. Display error in mini-buffer."
        (interactive)
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
