;;; Minimal init.el

;;; Package initialization - we'll use the built-in GNU and MELPA archives.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; Ensure all packages are installed.
(use-package use-package-ensure
  :custom
  (use-package-always-ensure t))

;;; Basic Emacs settings
(setopt set-mark-command-repeat-pop t)
(setopt initial-major-mode 'fundamental-mode)
(setopt display-time-default-load-average nil)
(setopt auto-revert-avoid-polling t
        auto-revert-interval 5
        auto-revert-check-vc-info t)
(setopt sentence-end-double-space nil)

;; Minibuffer/completion settings
;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion
(setopt enable-recursive-minibuffers t
        completion-cycle-threshold 5
        completions-detailed t           ; Show annotations with completions.
        tab-always-indent 'complete
        ;; completion-styles '(basic initials substring)
        completion-auto-help nil
        completions-max-height 8
        completions-detailed t
        completions-format 'one-column
        completions-group t
        ;;completion-auto-select 'second-tab
        )

(setopt line-number-mode t
        column-number-mode t)
(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt show-trailing-whitespace nil)
(setopt indicate-buffer-boundaries 'left)
(setopt mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction t)
(setopt indent-tabs-mode nil
        tab-width 4)
(setopt tab-bar-show 1)
;; (setopt display-line-numbers-width 3)
(setopt custom-file "~/.config/emacs/custom.el")

;; Emacs configuration functions
(global-auto-revert-mode)
(savehist-mode)
(windmove-default-keybindings 'shift) ; You can use other modifiers here
(when (display-graphic-p)
  (context-menu-mode))
(blink-cursor-mode 20)
(pixel-scroll-precision-mode)

;; Global key binds
(global-set-key (kbd "C-c f") #'menu-set-font)
(global-set-key (kbd "C-z") #'zap-up-to-char)
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;; Add our configuration subdirectory to load-path.
(add-to-list 'load-path (expand-file-name "extras" user-emacs-directory))

;; Emacs hooks
;; Display line numbers in programming modes.
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;;; Welcome screen

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents . 8)
                     (projects . 5)
                     (bookmarks . 5)))
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-set-file-icons t)
  (initial-buffer-choice 'dashboard-open))

;;;   Discovery aids

;; Show a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-add-key-based-replacements "C-x a" "abbrev-map")
  (which-key-add-key-based-replacements "C-x n" "narrow-map")
  (which-key-add-key-based-replacements "C-x p" "project-map")
  (which-key-add-key-based-replacements "C-x r" "register-map")
  (which-key-mode))

;;; Themes and UI

;; Configure the built-in modus-themes
(use-package emacs
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(use-package eziam-themes
  :config
  (custom-set-faces '(show-paren-match ((t (:background "lightgreen" :foreground "black")))))
  (custom-set-faces '(show-paren-mismatch ((t (:background "pink" :foreground "black")))))
  :custom
  (eziam-color-comments t)
  (eziam-scale-headings nil)
  (eziam-scale-other nil))

(use-package tok-theme)

(use-package almost-mono-themes)

(load-theme 'modus-vivendi-tinted t)
(set-cursor-color "magenta")

(use-package doom-modeline
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

;;; Emacs server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;;;   Built-in customization framework
(load custom-file)
(put 'narrow-to-region 'disabled nil)
