;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(eval-when-compile
  (require 'use-package))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.org/packages/")
   t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "a325ba05dc3b5c2fa89af0ff354bbbe90251fb1a6e6d5682977cebe61ce72ab7" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default))
 '(max-mini-window-height 10)
 '(package-selected-packages
   '(pyvenv lsp-mode use-package markdown-mode docker docker-compose-mode dockerfile-mode go-autocomplete exec-path-from-shell restclient elpy solarized-theme zeno-theme ## avy which-key cider clojure-mode company magit multiple-cursors slim-mode projectile-rails go-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Init electric-pair-mode upon Emacs startup
(electric-pair-mode +1)

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
  :ensure t)

;; Init projectile upon Emacs startup
(use-package projectile
  :ensure t
  :init
  ;; native will take into account .projectile files as opposed to alien
  ;; Consider going back to alien. Seems to be faster. Find a way to ignore
  ;; files with alien
  (setq projectile-indexing-method 'native)
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
  (which-key-mode +1))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; LANGUAGE ENHANCEMENTS

(use-package company
  :ensure t
  :config
  (global-company-mode t))

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

;; Clojure, CIDER mode enhancing

(use-package cider
  :ensure t
  :hook (clojure-mode . cider-mode)
  :config
  ;; Setting this to true to enable sshing into remote hosts to establish connection with
  ;; an nREPL server.
  ;; Employed to establish connection with running leiningen repl server running inside vm.
  (setq nrepl-use-ssh-fallback-for-remote-hosts t)
  ;; Make sure to start lein repl in port 36096 in vagrant vm.
  ;; Otherwise, will fall back to ask for specifics.
  ;; TODO: test if removing this affects cider-mode in any way in our current use case.
  ;; This is specifically for the VM development environment use case.
  (setq cider-known-endpoints
	'(("vagrant" "127.0.0.1" "36096")
	  ("localhost" "127.0.0.1" "36096"))))

;; Go
;; TODO: enable Guru. Download is failing. Perhaps requires a more recent version than 1.10

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

;; [wololo] Test this against supported golang version to ensure that it works properly without it
;; (use-package exec-path-from-shell
;;   :config
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize)))

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-enable-symbol-highlighting t
	lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook ((go-mode . lsp)
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs")
  ;; :demand t
  :config
  (setq pyvenv-workon "Dashboard")  ; Default venv for tesorio project
  (pyvenv-mode t))

