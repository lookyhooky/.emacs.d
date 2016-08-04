;;; my-js-config.el --- A collections of functions to setup `js-mode'

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; Commentary:

;;;

;;; Code:

(defun my/js-hook ()
  "My personal `js-mode-hook'."

  ;; Change the word "function" to just "f".
  (font-lock-add-keywords
   'js-mode `(("\\(function\\)"
               (0 (progn (compose-region (match-beginning 1) (match-end 1) "ƒ")
                         nil)))))

  ;; Highlight with warning font around "TODO" and others.
  (font-lock-add-keywords 'js-mode
                          '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                             1 font-lock-warning-face t)))

  (autoload 'tern-mode "tern.el" nil t)

  ;; Use auto-complete for tern completion.
  (eval-after-load 'tern-mode
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))

  ;; Use basic tern-mode.
  (tern-mode t)

  ;; Use subword mode for word commands to work on camel case.
  (subword-mode t)

  ;; Prevent subword from inserting a comma in mode line.
  (let ((entry (assq 'subword-mode minor-mode-alist)))
    (when entry (setcdr entry '(nil)))))


(provide 'my-js-config)
;;; my-js-config.el ends here
