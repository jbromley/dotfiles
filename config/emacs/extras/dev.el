;;; dev.el --- Configure development tools -*-coding: utf-8 -*-
;;
;; Copyright (C) 2025 J. Bromnley <jbromley@gmail.com>
;;
;; Author:      J. Bromnley <jbromley@gmail.com>
;; Version:     0.1
;; Package-Requires: ("org")
;; Keywords:    convenience,languages,lisp,tools
;; URL:         
;;
;;; Commentary:

;; This package configures tools to facilitate development in various languages.
;; The following items are configured.
;;  - Built-in config for developers
;;  - Version Control
;;  - Common file types
;;  - Eglot, the built-in LSP client for Emacs

;;; Change Log:

;;; Code:

;; Write code here. defcustom first, then defconst, defvar,
;; defsubst/defmacro and defuns last

;;;   Built-in config for developers

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (C++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (elixir-mode . elixir-ts-mode)
          (js2-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  ;; Set up to use programs installed with mise
  (setenv "PATH" (concat (getenv "PATH") ":~/.local/share/mise/shims"))
  (setq exec-path (append exec-path '("~/.local/share/mise/shims")))
  
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

(use-package paredit
  :ensure t
  :autoload enable-paredit-mode
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode))
 

;;;   Version Control

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;;;   Common file types

(use-package elixir-mode
  :ensure t
  :mode ("\\.ex\\'" "\\.exs\\'")
  :interpreter ("iex"))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package json-mode
  :ensure t)

(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'")
  :interpreter "racket"
  :hook (racket-mode . enable-paredit-mode))

(use-package yaml-mode
  :ensure t)

;;;   Eglot, the built-in LSP client for Emacs

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  :hook
  (((c-mode c++-mode elixir-mode elixir-ts-mode python-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (add-to-list 'eglot-server-programs
               ; '(haskell-mode . ("haskell-language-server-wrapper" "--lsp"))
               '((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls"))))

(provide 'dev)
