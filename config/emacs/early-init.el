;; early-init.el -*- coding: utf-8; lexical-binding: t; -*-

;; (setq use-package-compute-statistics t)

;; Configure GC for fast initialization.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(defun jb/emacs-startup ()
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'jb/emacs-startup)

;; Basic frame settings
(setopt frame-resize-pixelwise t
        frame-inhibit-implied-resize t
        use-dialog-box t
        use-file-dialog nil
        use-short-answers t
        inhibit-splash-screen t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t)

(setq frame-title-format '("%b")
      ring-bell-function 'ignore
      inhibit-x-resources t)

;; Startup speed, annoyance suppression
(setopt byte-compile-warnings (not 'obsolete)
        warning-suppress-log-types '((comp) (bytecomp))
        native-comp-async-report-warnings-errors 'silent)

;; Silence startup message
(setopt inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-frame-alist '((fullscreen . maximized)
                            ;; You can turn off scroll bars by uncommenting these lines:
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)

                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#0d0e1c")
                            (foreground-color . "#fcfefd")
                            (alpha-background . 97)
                            (font . "JetBrains Mono-10")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . nil)))
