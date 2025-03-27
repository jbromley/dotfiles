;;; dev.el --- Configure development tools -*-coding: utf-8 -*-
;;
;; Copyright (C) 2025 J. Bromley <jbromley@gmail.com>
;;
;; Author:      J. Bromley <jbromley@gmail.com>
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
  ;; Open shell configuration in shell-script-mode
  (add-to-list 'auto-mode-alist '("\\.?zshenv\\'" . shell-script-mode))
  (add-to-list 'auto-mode-alist '("\\.?zshrc\\'" . shell-script-mode))

  :custom
  (ansi-color-for-compilation-mode t)
  
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)
   (compilation-filter . ansi-color-compilation-filter)))

;; Mise en place
(use-package mise
  :config
  (global-mise-mode))

;; Project management
(use-package project
  :custom
  (project-vc-extra-root-markers '("mix.exs" "CMakeLists.txt")))

;; Automatically handle installing and using treesitter modes.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode)
  :bind
  (:map hl-todo-mode-map
        ("C-c p" . hl-todo-previous)
        ("C-c n" . hl-todo-next)
        ("C-c o" . hl-todo-occur)
        ("C-c i" . hl-todo-insert)))

(use-package consult-todo
  :demand t
  :init
  (define-prefix-command 'consult-todo-map)
  :config
  (which-key-add-key-based-replacements "M-s M-t" "consult-todo-map")
  :bind-keymap
  ("M-s M-t" . consult-todo-map)
  :bind
  (:map consult-todo-map
        ("t" . consult-todo)
        ("a" . consult-todo-all)
        ("p" . consult-todo-project)))

(use-package paredit
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
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;;;   Common file types

;; Erlang
(use-package erlang-ts
  :defer 't
  :mode ("\\.erl\\'" . erlang-ts-mode)
  :interpreter ("erl"))

;; Elixir
(use-package elixir-ts-mode
  :defer t
  :mode ("\\.ex\\'" "\\.exs\\'")
  :interpreter ("iex"))

;; Markdown
(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'")
  :hook ((markdown-mode . (lambda ()
                            (visual-line-mode)
                            (set-fill-column 80)
                            (auto-fill-mode t)))))

;; JSON
(use-package json-ts-mode
  :defer t
  :mode ("\\.json\\'"))

;; Racket
(use-package racket-mode
  :defer t
  :mode ("\\.rkt\\'")
  :interpreter "racket"
  :hook (racket-mode . enable-paredit-mode))

;; CMake
(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt\\'")
  :defer t)

(use-package cmake-project
  :load-path "~/Code/emacs-cmake-project/"
  :commands (cmake-project-mode cmake-project-configure-project)
  :after (:any cmake-mode cmake-ts-mode)
  :hook
  (((c-mode c-ts-mode) . maybe-cmake-project-mode)
   ((c++-mode c++-ts-mode) . maybe-cmake-project-mode)))

;; Verilog
(use-package verilog-ts-mode
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

;; YAML
(use-package yaml-ts-mode
  :defer t
  :mode ("\\.ya?ml\\'"))

;;; Eglot, the built-in LSP client for Emacs

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (let ((servers '(((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls"))
                   ((erlang-mode erlang-ts-mode) "erlang_ls" "--transport" "stdio")
                   ((verilog-mode verilog-ts-mode) . ("svls")))))
    (dolist (server servers eglot-server-programs)
      (add-to-list 'eglot-server-programs server)))
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements global-map
      "C-c e" "eglot"))
  :bind
  (:map eglot-mode-map
        ("C-c e b" . eglot-events-buffer)
        ("C-c e f" . eglot-format)
        ("C-c e F" . eglot-format-buffer)
        ("C-c e r" . eglot-rename)

        ("C-c e a" . eglot-code-actions)
        ("C-c e q" . eglot-code-action-quickfix)
        ("C-c e o" . eglot-code-action-organize-imports)

        ("C-c e h" . eglot-help-at-point)

        ("C-c e d" . eglot-find-declaration)
        ("C-c e D" . eglot-find-definition)
        ("C-c e i" . eglot-find-implementation)
        ("C-c e t" . eglot-find-typeDefinition)
        ("C-c e R" . eglot-find-references)
        ("C-c e w" . eglot-show-workspace-configuration)

        ("C-c e l" . flymake-show-diagnostics-buffer)
        ("C-c e n" . flymake-goto-next-error)
        ("C-c e p" . flymake-goto-prev-error))
  :hook
  (((c-mode c-ts-mode c++-mode c++-ts-mode elixir-mode elixir-ts-mode python-mode) . eglot-ensure)))

(use-package flymake
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

;; Eldoc configuration

;; (use-package eldoc-box
;;   :defer t
;;   :custom
;;   ((eldoc-documentation-strategy 'eldoc-documentation-default)
;;    (eldoc-echo-area-use-multiline-p 5)
;;    (eldoc-idle-delay 1.0)))

;; Make eldoc show in a buffer below the current buffer.
(add-to-list 'display-buffer-alist
             '("^\\*eldoc" display-buffer-at-bottom
               (window-height . 16)))

;;; Ligatures
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  ;; (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   '(prog-mode utop-mode)
   '("|||>" "<|||" "<==>" "<!--" "~~>" "||=" "||>" ; removed "***", "####"
     "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!==" ; removed ":::"
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~=" ; removed "..."
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ; removed ">:"  
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".?" "+>" "++" "?:" ; removed ".."
     "?=" "?." "??" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)" ; removed ";;"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(provide 'dev)
