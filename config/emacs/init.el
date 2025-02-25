;;; Minimal init.el

;;; Package initialization - we'll use the built-in GNU and MELPA archives.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(setopt use-package-always-ensure t)

;;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; Basic Emasc settings

(use-package emacs
  :config
  (global-auto-revert-mode)
  (savehist-mode)
  (windmove-default-keybindings 'shift) ; You can use other modifiers here

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

  (blink-cursor-mode 20)
  (pixel-scroll-precision-mode)

  ;; Global key binds
  (global-set-key (kbd "C-z") #'zap-up-to-char)
  ;; TAB acts more like how it does in the shell
  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

  ;; Add our configuration subdirectory to load-path.
  (add-to-list 'load-path (expand-file-name "extras" user-emacs-directory))

  :custom
  (set-mark-command-repeat-pop t)
  (initial-major-mode 'fundamental-mode)
  (display-time-default-load-average nil)
  (auto-revert-avoid-polling t)
  (auto-revert-interval 5)
  (auto-revert-check-vc-info t)
  (sentence-end-double-space nil)

  ;; Minibuffer/completion settings
  ;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion
  (enable-recursive-minibuffers t)
  (completion-cycle-threshold 1)
  (completions-detailed t)             ; Show annotations with completions.
  (tab-always-indent 'complete)
  (completion-styles '(basic initials substring))
  (completion-auto-help 'always)
  (completions-max-height 20)
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-group t)
  (completion-auto-select 'second-tab)
  ;; (completion-auto-select t))

  ;; Show line and column in mode line.
  (line-number-mode t)
  (column-number-mode t)

  ;; Prettier underlines
  (x-underline-at-descent-line nil)
  ;; Make switching buffers more consistent
  (switch-to-buffer-obey-display-actions t)

  ;; By default, don't underline trailing spaces
  (show-trailing-whitespace nil)

  ;; Show buffer top and bottom in the margin
  (indicate-buffer-boundaries 'left)

  ;; Enable horizontal scrolling
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)

  ;; Tab settings
  (indent-tabs-mode nil)
  (tab-width 4)

  ;; Only show tab-bar if there is more than one tab.
  (tab-bar-show 1)
  
  ;; Width of line numbers in fringe.
  (display-line-numbers-width 3)

  ;; Save customizations in their own file.
  (custom-file "~/.config/emacs/custom.el")

  :hook
  ;; Display line numbers in programming modes.
  (prog-mode . display-line-numbers-mode)

  ;; Use automatic word wrap in text modes.
  (text-mode . visual-line-mode)

  ;; Highlight current line in any text mode.
  ;; (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))

;;; Welcome screen

(use-package dashboard
  :ensure t
  :custom
  (dashboard-items '((recents . 8)
                     (projects . 5)
                     (bookmarks . 5)))
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

;;;   Discovery aids

;; Show a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-add-key-based-replacements "C-x a" "abbrev-map")
  (which-key-add-key-based-replacements "C-x n" "narrow-map")
  (which-key-add-key-based-replacements "C-x p" "project-map")
  (which-key-add-key-based-replacements "C-x r" "register-map")
  (which-key-mode))

;;; Themes and UI

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t))

(use-package dracula-theme
  :ensure t
  :defer t
  :custom
  (dracula-enlarge-headings nil))

(use-package solarized-theme
  :ensure t
  :defer t
  :custom
  (solarized-scale-org-headlines nil)
  (solarized-use-more-italic t))

(load-theme 'dracula t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;;   Optional extras

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(require 'base)

;; Packages for software development
(require 'dev)

;; Vim-bindings in Emacs (evil-mode configuration)
;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
(require 'org-config)

;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the elisp file for more
;; details.
;(load-file (expand-file-name "extras/email.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))

;;;   Built-in customization framework
(load custom-file)
