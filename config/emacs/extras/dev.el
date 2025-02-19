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

;; Project management
(use-package project
  :custom
  (project-vc-extra-root-markers '("mix.exs")))

;; Open shell configuration in shell-script-mode
(add-to-list 'auto-mode-alist '("\\.?zshenv\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.?zshrc\\'" . shell-script-mode))

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

(use-package elixir-ts-mode
  :ensure t
  :defer t
  :mode ("\\.ex\\'" "\\.exs\\'")
  :interpreter ("iex"))

(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'")
  :hook ((markdown-mode . (lambda ()
                            (visual-line-mode)
                            (set-fill-column 80)
                            (auto-fill-mode t)))))

(use-package json-ts-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'"))

(use-package racket-mode
  :ensure t
  :defer t
  :mode ("\\.rkt\\'")
  :interpreter "racket"
  :hook (racket-mode . enable-paredit-mode))

(use-package verilog-ts-mode
  :ensure t
  :defer t
  :mode ("\\.s?vh?\\'")
  :custom
  (verilog-indent-level 2)
  (verilog-indent-level-behavioral 2)
  (verilog-indent-level-declaration 2)
  (verilog-indent-level-module 2)
  (verilog-ts-indent-level 2)
  :init
  (setq jb/verilog-ts-auto-config
        (make-treesit-auto-recipe
         :lang 'verilog
         :ts-mode 'verilog-ts-mode
         :remap '(verilog-mode)
         :url "https://github.com/gmlarumbe/tree-sitter-systemverilog"
         :revision "master"
         :source-dir "src"
         :ext "\\.s?vh?"))
  (add-to-list 'treesit-auto-recipe-list jb/verilog-ts-auto-config))

(use-package yaml-ts-mode
  :ensure t
  :defer t
  :mode ("\\.ya?ml\\'"))

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

