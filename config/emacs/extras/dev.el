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
  ;; Set up to use programs installed with mise
  (setenv "PATH" (concat (getenv "PATH") ":~/.local/share/mise/shims"))
  (setq exec-path (append exec-path '("~/.local/share/mise/shims")))
  
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;; Automatically handle installing and using treesitter modes.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package paredit
  :ensure t
  :defer t
  :autoload enable-paredit-mode
  :bind
  (:map paredit-mode-map
   ("M-s" . nil)
   ("M-S-<up>" . paredit-splice-sexp))
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . (lambda () (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp))))

;;;   Version Control

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

;;;   Common file types

(use-package elixir-mode
  :ensure t
  :defer t
  :mode ("\\.ex\\'" "\\.exs\\'")
  :interpreter ("iex"))

(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'")
  :hook ((markdown-mode . visual-line-mode)))

(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'"))

(use-package racket-mode
  :ensure t
  :defer t
  :mode ("\\.rkt\\'")
  :interpreter "racket"
  :hook (racket-mode . enable-paredit-mode))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;;;   Eglot, the built-in LSP client for Emacs

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)
  (let ((servers '(((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls"))
                   ((verilog-mode verilog-ts-mode) . ("svls")))))
    (dolist (server servers eglot-server-programs)
      (add-to-list 'eglot-server-programs server)))

  :hook
  (((c-mode c++-mode elixir-mode elixir-ts-mode python-mode) . eglot-ensure)))

(provide 'dev)

