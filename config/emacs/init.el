;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional extras
;;;  - Built-in customization framework

;;;   Basic settings

;; Package initialization - we'll use the built-in GNU and MELPA archives.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(setopt use-package-always-ensure t)

;;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; Welcome screen

;; Use a dashboard.
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

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Shift-<arrow keys>
(windmove-default-keybindings 'shift) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

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

;;; Global key binds
(global-set-key (kbd "C-z") #'zap-up-to-char)

;;;   Minibuffer/completion settings
;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates
(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;;;   Interface enhancements/defaults

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; Tab settings
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode 20)
(pixel-scroll-precision-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;   Tab-bar configuration

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
;; (setopt display-time-format "%a %F %T")
;; (setopt display-time-interval 1)
;; (display-time-mode)

;;;   Theme

(use-package emacs
  :config
  (require-theme 'modus-themes)
  :custom
  (set-mark-command-repeat-pop t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t))

(use-package dracula-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :defer t
  :custom
  (solarized-scale-org-headlines nil)
  (solarized-use-more-italic t))

(load-theme 'modus-vivendi t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;;   Optional extras

(add-to-list 'load-path (expand-file-name "extras" user-emacs-directory))

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
(setopt custom-file "~/.config/emacs/custom.el")
(load custom-file)
