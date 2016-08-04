;;;  ui.el --- Emacs aesthetic customizations.

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; Commentary:

;;; Configure ui early in initialization process.

;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-startup-message t)

;; Set frame size.
(add-to-list 'initial-frame-alist '(height . 38))
(add-to-list 'initial-frame-alist '(width . 90))

;; Increase font size.
(set-face-attribute 'default nil :height 140)

;; Set font face.
(set-frame-font "Source Code Pro")

;; (load-theme 'zenburn t)
;; (load-theme 'monokai t)

;; (setq monokai-use-variable-pitch nil)
;; (with-eval-after-load "monokai-theme" (setq monokai-use-variable-pitch nil))

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing `:height' relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'my/org-mode-hook)

(provide 'ui)

;;; ui.el ends here
