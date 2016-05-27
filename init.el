;;; init.el --- This is my emacs configuration.

;;; Commentary:

;;; Uses `use-package' to load and configure packages.

;;; Code:

(defvar *lisp-dir*
 (expand-file-name "lisp/" user-emacs-directory)
 "This directory contains my personal settings.")
(add-to-list 'load-path *lisp-dir*)

(defvar *snippets-dir*
 (expand-file-name "snippets/" user-emacs-directory)
 "This directory contains my snippets.")

(defvar *themes-dir*
 (expand-file-name "themes/" user-emacs-directory)
 "This directory contains the themese.")
(add-to-list 'custom-theme-load-path *themes-dir*)

(defvar *savefiles-dir*
 (expand-file-name "savefiles/" user-emacs-directory)
 "This directory contains save and history files.")
(unless (file-exists-p *savefiles-dir*)
 (make-directory *savefiles-dir*))

(defvar *backups-dir*
  (expand-file-name "backups/" user-emacs-directory)
  "This directory contains backup files of buffers being edited.")
(unless (file-exists-p *backups-dir*)
 (make-directory *backups-dir*))

(require 'ui) ;; Remove mouse ui and adusting window size right away.
(require 'defaults) ;; My default setting.

(require 'package)

;; Define package repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Disable automatic package load
(setq package-enable-at-startup nil)

;; Fire up package.el
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Use the :init keyword to execute code before a package is loaded.
;; It accepts one or more forms, up until the next keyword:

;; Use the :config keyword to execute code after a package is loaded.
;; In cases where loading is done lazily (see more about autoloading below),
;; this execution is deferred until after the autoload occurs

(use-package diminish
  :ensure t)

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :config (require 'shell-intergration)))

(use-package ido
  :bind ("C-x M-f" . ido-find-file-other-window)
  :init
  (setq ido-everywhere t)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-prospects 10)
  (setq ido-save-directory-list-file
        (expand-file-name "ido.last" *savefiles-dir*))
  (setq ido-file-extensions-order '(".py" ".el" ".js" ".less"))
  :config
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (ido-mode t))

(use-package flx-ido
  :ensure t
  :init
  (setq ido-enable-flex-matching t) ;; Turn on ido flexible matching.
  (setq ido-use-faces nil) ;; Turn off ido text highlighting so flx-ido can do it.
  :config
  (flx-ido-mode t))

(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only) ;; Bind C-n and C-p.
  :config
  (ido-vertical-mode t))

(use-package smex
  :ensure t
  :init
  (setq smex-save-file
		(expand-file-name "smex-items" *savefiles-dir*))
  :bind  (("M-x" . smex)
          ("M-X" . smex-major-mode-commands)
          ("C-c C-c M-x" . execute-extended-command)) ;; The old M-x.
  :config
  (smex-initialize))

(use-package projectile
  :ensure t
  :diminish (projectile-mode . "Prjl")
  :init
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" *savefiles-dir*))
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" *savefiles-dir*))
  :config
  (projectile-global-mode))

(use-package recentf
  :init
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 15)
  (setq recentf-save-file
        (expand-file-name "recentf" *savefiles-dir*))
  :config
  (defun recentf-ido-find-file ()
    "Find a recent file using ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
  (recentf-mode 1))

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" *savefiles-dir*)))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode t))

(use-package functions
  :load-path "lisp/"
  :bind (([remap move-beginning-of-line] . my/move-beginning-of-line)
         ([(meta o)] . my/open-line)
         ([(meta O)] . my/open-line-above)
         ("C-;" . my/toggle-comment-on-line)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; (use-package company
;;   :ensure t
;;   :defer 1
;;   :config
;;   (global-company-mode t))

(use-package flycheck
  :ensure t
  :defer 2
  :init
  (setq-default flycheck-emacs-lisp-load-path load-path)
  :config
  (global-flycheck-mode t))

(use-package auto-complete
  :ensure t
  :init
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" *savefiles-dir*))
  :config
  (add-to-list 'ac-dictionary-directories "ac-dict")
  (ac-config-default))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs
        '(*snippets-dir*))
  :config
  (yas-reload-all))

(require 'my-js-config)

(use-package js-mode
  :defines js-indent-level
  :diminish (js-mode . "JS")
  :mode "\\.js$"
  :init
  (setq js-indent-level 2)
  :config
  (add-hook 'js-mode-hook 'my/js-hook)
  (add-hook 'js-mode-hook #'yas-minor-mode))

(require 'my-python-config)
(use-package python-mode
  :defines python-indent
  :mode "\\.py\\'"
  :interpreter "python"
  :init
  (setq python-indent 4)
  :config
  (add-hook 'python-mode-hook 'my/python-hook))

(provide 'init)

;;; init.el ends here
