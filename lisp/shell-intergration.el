;;; shell-intergration.el -- Sets up exec-path-from shell

;;; Commentary:

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login.  Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell

;;; https://github.com/purcell/exec-path-from-shell

;;; Code:

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  ;; (exec-path-from-shell-copy-envs
  ;;  '("PATH"))
  )

(provide 'shell-intergration)

;;; shell-intergration.el ends here
