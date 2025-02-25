;;; org-config.el --- Org mode configuration -*-coding: utf-8 -*-
;;
;; Copyright (C) 2025 J. Bromnley <jbromley@gmail.com>
;;
;; Author:      J. Bromnley <jbromley@gmail.com>
;; Version:     0.1
;; Package-Requires: ("org")
;; Keywords:    calendar,hypermedia,outlines
;; URL:         https://orgmode.org
;;
;;; Commentary:

;; This package configures Org mode. It sets up ~/Org as the org directory,
;; adds some tasks states, and generally configures org mode.

;;; Change Log:

;;; Code:

;; Write code here. defcustom first, then defconst, defvar,
;; defsubst/defmacro and defuns last

(use-package org-superstar
  :ensure t
  :defer t)

(use-package org
  :defer t
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  :custom
  (org-directory "~/Org/")
  (org-agenda-files '("~/Org"))  
  (org-tag-alist '((:startgroup)
		           ("home" . ?h)
		           ("work" . ?w)
		           ("learning" . ?l)
		           (:endgroup)
                   (:newline)
                   (:startgroup)
                   ("one-shot" . ?o)
                   ("project" . ?j)
                   ("tiny" . ?t)
                   (:endgroup)
                   ("meta")
                   ("review")
                   ("reading")))
  (org-refile-targets nil)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-capture-templates '(("c" "Default Capture" entry (file "Inbox.org")
			                "* TODO %?\n%U\n%i")
			               ;; Capture and keep an org-link to the thing we're currently working with
			               ("r" "Capture with Reference" entry (file "Inbox.org")
			                "* TODO %?\n%U\n%i\n%a")))
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (org-export-with-smart-quotes t)

  :bind (:map global-map
              ("C-c l s" . org-store-link) ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert

  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode)
         (org-mode . (lambda () (org-superstar-mode 1)))
         (org-mode . (lambda ()
                       (set-fill-column 80)
                       (auto-fill-mode t)))))

(provide 'org-config)
