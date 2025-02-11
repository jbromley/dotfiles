;;; base.el --- UI, consult, and completion enhancements -*-coding: utf-8 -*-
;;
;; Copyright (C) 2025 J. Bromnley <jbromley@gmail.com>
;;
;; Author:      J. Bromnley <jbromley@gmail.com>
;; Version:     0.1
;; Package-Requires: ("avy" "consult" "embark" "embark-consult" "vertico" "vertico-directory" "marginalia" "corfu" "corfu-popupinfo" "corfu-terminal" "cape" "kind-icon" "eat" "vterm" "orderless" "wgrep")
;; Keywords:    calendar,hypermedia,outlines
;; URL:         https://orgmode.org
;;
;;; Commentary:

;; This package configures the following items.
;;  - Motion aids
;;  - Power-ups: Embark and Consult
;;  - Minibuffer and completion
;;  - Misc. editing enhancements

;;; Change Log:

;;; Code:

;; Write code here. defcustom first, then defconst, defvar,
;; defsubst/defmacro and defuns last

;;;   Motion aids

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;   Power-ups: Embark and Consult

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :custom
  (consult-narrow-key "<")
  (consult-fd-args
   '((if (executable-find "fd" 'remote) "fd" "fdfind")
     "--full-path --color=never"))
  :bind
  (("C-x b" . consult-buffer)     ; orig. switch-to-buffer
   ("M-y"   . consult-yank-pop)   ; orig. yank-pop
   ("M-s f" . consult-fd)
   ("M-s F" . consult-find)
   ("M-s l" . consult-line)     ; Alternative: rebind C-s to use
   ("M-s L" . consult-line-multi) ; isearch to M-s s
   ("M-s m" . consult-man)
   ("M-s o" . consult-outline)
   ("M-s r" . consult-ripgrep)
   ("M-s s" . consult-line) ; consult-line instead of isearch, bind
   ("M-s t" . consult-theme)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history) ; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
   ("M-s l" . consult-line) ; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)))

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c a" . embark-act))     ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun jb/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'jb/avy-action-embark))

(use-package embark-consult
  :ensure t)

;;;   Minibuffer and completion

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package vterm
  :ensure t)

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;;;   Misc. editing enhancements

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; Use ChatGPT in Emacs
(use-package chatgpt-shell
  :ensure t
  :defer t
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pick-first-password :host "api.openai.com"))))


(provide 'base)
