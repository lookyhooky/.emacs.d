;;;  ui.el --- Emacs aesthetic customizations.

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
(set-face-attribute 'default nil :height 150)

;; Set font face.
(set-frame-font "Source Code Pro")

;;(load-theme 'zenburn t)
(load-theme 'monokai t)


(provide 'ui)

;;; ui.el ends here
