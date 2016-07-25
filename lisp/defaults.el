;;; defaults.el --- A set of basic defaults.

;;; Commentary:

;; Based on techromancy/better-defaults.el

;;; Code:

;; I'll try using zap-to-char...
;; (autoload 'zap-up-to-char "misc"
;;   "Kill up to, but not including ARGth occurence of CHAR/" t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Just 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show line numbers
;; Adding a hook to prog-mode seems better to me than global
;; (global-linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)

;; Highlight current line
(global-hl-line-mode 1)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Highlight matching parenthesis.
(show-paren-mode 1)

;; Automatically insert matching closing symbols to opening symbols.
(electric-pair-mode t)

;; Use spaces unstead of tabs.
(setq-default indent-tabs-mode nil)

;; Require a newline at the end of a file.
(setq require-final-newline t)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Load newer compiled files.
(setq load-prefer-newer t)

;;(setq backup-directory-alist *backups-dir*)
;; I want to set backup-directory-alist, but having some issues.
;; This is the short term fix.
(setq make-backup-files nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Suggested by https://github.com/lewang/flx to speed up the garbage collector.
(setq gc-cons-threshold 20000000)

;; (setq visible-bell t)

;; Limit da' ding
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort
                        abort-recursive-edit
                        exit-minibuffer
                        keyboard-quit))
          (ding))))

(provide 'defaults)

;;; defaults.el ends here
