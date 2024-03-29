(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.org/packages/")
   t))

(package-initialize)

(eval-when-compile
  (require 'use-package))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "a325ba05dc3b5c2fa89af0ff354bbbe90251fb1a6e6d5682977cebe61ce72ab7" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default))
 '(max-mini-window-height 10)
 '(package-selected-packages
   '(sql-indent company-ctags lsp-ui js2-mode js-mode robe helm-projectile projectile lsp-python-ms pyvenv lsp-mode use-package markdown-mode docker docker-compose-mode dockerfile-mode go-autocomplete exec-path-from-shell restclient elpy solarized-theme zeno-theme ## avy which-key cider clojure-mode company magit multiple-cursors slim-mode projectile-rails go-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Init electric-pair-mode upon Emacs startup
(electric-pair-mode)

;; Init linecount mode
(global-display-line-numbers-mode)

;; Load theme of choice
(use-package zeno-theme
  :ensure t
  :config
  (load-theme 'zeno t))

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;; Init projectile upon Emacs startup
(use-package projectile
  :ensure t
  :init
  ;; native will take into account .projectile files as opposed to alien
  ;; Consider going back to alien. Seems to be faster. Find a way to ignore
  ;; files with alien
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)))

;; Autocompletion suggestion for project navigation
(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on))

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; Required in order to ensure that the exec-path in Emacs GUI
;; is the same as that for the shell
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  (when (memq window-system '(mac ns x))
   (exec-path-from-shell-initialize)))

;; LANGUAGE ENHANCEMENTS

(use-package company
  :ensure t
  :config
  (global-company-mode))

;; Ruby
;; TODO: use projectile-rails to better navigate a rails project
(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode)
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

;; Javascript enhancement
(use-package js2-mode
  :ensure t
  :config
  (setq js2-basic-offset 2))

;; Clojure, CIDER mode enhancing
(use-package cider
  :ensure t
  :hook (clojure-mode . cider-mode))

;; SQL mode indentation support
(use-package sql-indent
  :ensure t)

;; set prefix for lsp-command-keymap
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :config
  ;; Fine tunning LSP configs for better performance
  (setq gc-cons-threshold (* 100 1024 1024) ; Increase this one by a factor of 2 until satisfied
	read-process-output-max (* 1024 1024)
	lsp-log-io nil
        lsp-idle-delay 0.500)
  ;; Python specific LSP configs
  (setq lsp-enable-symbol-highlighting nil
	lsp-pyls-plugins-flake8-enabled t
	lsp-pyls-plugins-yapf-enabled t)
  ;; Disable real time syntax check
  (setq lsp-diagnostic-package :none)
  ;; use ffip instead
  (setq lsp-enable-links nil)
  ;; Disable completion
  (setq lsp-enable-completion-at-point nil)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  ;; Dirs/files to be ignored by watcher (find a more elegant way to express this)
  (push "/Users/humberto.tellechea/tesorio/projects/Dashboard/\\.log" lsp-file-watch-ignored-directories)
  (push "/Users/humberto.tellechea/tesorio/projects/Dashboard/\\.fonts" lsp-file-watch-ignored-directories)
  (push "/Users/humberto.tellechea/tesorio/projects/Dashboard/media" lsp-file-watch-ignored-directories)
  (push "/Users/humberto.tellechea/tesorio/projects/.*/node_modules" lsp-file-watch-ignored-directories)
  (push "/Users/humberto.tellechea/tesorio/projects/Dashboard/.*\\.pyc$" lsp-file-watch-ignored) 
  ;; (yas-global-mode) TODO: disabling for now but should look into it as it seems like a cool tool
  :hook ((go-mode . lsp)
	 (js-mode . lsp)
	 (python-mode . (lambda ()
			  (require 'lsp-python-ms)
			  (lsp)))
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; Using the Microsoft Python Language Server as it is faster
;; than Palantir's crap
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t))

(use-package lsp-ui
  :ensure t)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; Go mode hooks
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; js mode enhancement
(add-hook 'js-mode-hook 'js2-minor-mode)

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs")
  :config
  (setq pyvenv-workon "Dashboard")  ; Default venv for tesorio project
  (pyvenv-tracking-mode t))	    ; Read from active virtualenvs

