;;; init.el --- This is my emacs configuration.

;;; Commentary:

;;; Uses `use-package' to load and configure packages.

;;; Code:

(defvar *lisp-dir*
 (expand-file-name "lisp/" user-emacs-directory)
 "This directory contains my personal settings.")
(add-to-list 'load-path *lisp-dir*)

(defvar *node-modules-dir*
 (expand-file-name "node_modules/" user-emacs-directory)
 "This directory contains npm packages required for my-js-config.el.")

(defvar *tern-dir*
  (expand-file-name "tern/emacs/" *node-modules-dir*)
  "This directory contains tern.")
(add-to-list 'load-path *tern-dir*)

(defvar *snippets-dir*
 (expand-file-name "snippets/" user-emacs-directory)
 "This directory contains my snippets.")

(defvar *themes-dir*
 (expand-file-name "themes/" user-emacs-directory)
 "This directory contains the themes.")
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

(setq default-directory (concat (getenv "HOME") "/Documents/projects"))

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

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t))

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :config (require 'shell-intergration)))

;; Add package node_modules directory to exec-path
(let ((node-bin (concat *node-modules-dir* ".bin")))
  (setenv "PATH" (concat node-bin ":" (getenv "PATH")))
  (setq exec-path (cons node-bin exec-path)))

(use-package diminish
  :ensure t
  :init
  (eval-after-load "js-mode" (diminish 'js-mode "JS")))

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
         ("M-o" . my/open-line)
         ("M-O" . my/open-line-above)
         ("C-;" . my/toggle-comment-on-line)))

(use-package my-toggle-buffer
  :load-path "lisp/"
  :bind (("C-o" . my/toggle-buffer)))

;; Decided to clobber `open-line' in favor of mode-line-other-buffer
;; I perfer my/open-line functions anyway.

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-repl-mode #'enable-paredit-mode))

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
  (setq-default flycheck-disabled-checkers '(javascript-eslint))
  (setq-default flycheck-emacs-lisp-load-path load-path)
  :config
  (global-flycheck-mode t))

(use-package whitespace-cleanup-mode
  :ensure t
  :init
  (add-hook 'js-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'elm-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'lisp-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'elisp-mode-hook 'whitespace-cleanup-mode))

(use-package auto-complete
  :ensure t
  :init
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" *savefiles-dir*))
  ;; resetting ac-sources
  (setq-default ac-sources '(
                             ac-source-yasnippet
                             ac-source-abbrev
                             ac-source-dictionary
                             ac-source-words-in-same-mode-buffers))
  :config
  (add-to-list 'ac-dictionary-directories "ac-dict")
  (ac-config-default))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs
        '(*snippets-dir*))
  :config
  (yas-reload-all))

(use-package elm-mode
  :ensure t)

(add-to-list 'load-path (expand-file-name "flycheck-elm/" user-emacs-directory))

(require 'flycheck-elm)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(use-package magit
  :bind (("C-x g" . magit-status)))

;; (use-package clojure-mode
;;   :defines inferior-lisp-program
;;   :init
;;   (setq inferior-lisp-program "lein repl")
;;   :config
;;   (add-hook 'clojure-mode-hook #'subword-mode)
;;   (add-hook 'clojure-mode-hook #'company-mode)
;;   (add-hook 'clojure-mode-hook #'enable-paredit-mode))

;; (use-package cider
;;   :init
;;   ;; go right to the REPL buffer when it's finished connecting
;;   (setq cider-repl-pop-to-buffer-on-connect t)
;;   ;; When there's a cider error, show its buffer and switch to it
;;   (setq cider-show-error-buffer t)
;;   (setq cider-auto-select-error-buffer t)
;;   ;; Where to store the cider history.
;;   (setq cider-repl-history-file "~/.emacs.d/cider-history")
;;   ;; Wrap when navigating history.
;;   (setq cider-repl-wrap-history t)
;;   :config
;;   ;; provides minibuffer documentation for the code you're typing into the repl
;;   (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;;   ;; enable paredit in your REPL
;;   (add-hook 'cider-repl-mode-hook 'paredit-mode))


;; (use-package js2-mode
;;   :diminish "JS"
;;   :init
;;   (setq js2-strict-missing-semi-warning nil)
;;   (setq js2-basic-offset 2)
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))

(require 'my-js-config)

(use-package js-mode
  :defines js-indent-level
  :diminish (js-mode . "JS")
  :mode "\\.js$"
  :init
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)      ; Offset switch statements properly.
  (add-hook 'js-mode-hook #'my/js-hook)
  (add-hook 'js-mode-hook #'yas-minor-mode))

(use-package web-mode
  :ensure t
  :init
  ;; Adjust Indent
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; Adding this was the only way I could get dictionary to work
;; found it on emacswiki https://www.emacswiki.org/emacs/InteractiveSpell
(setenv "DICTIONARY" "en_US")

(with-eval-after-load "ispell"
  (setq ispell-program-name "hunspell"))

;; Not Working
;; (defun comint-clear-buffer ()
;;   "Clear the comint buffer."
;;   (interactive)
;;   (let ((comint-buffer-maximum-size 0))
;;     (comint-truncate-buffer)))

;; let's bind the new command to a keycombo
;; (define-key comint-mode-map "\C-c\M-o" #'comint-clear-buffer)

(provide 'init)

;;; init.el ends here
